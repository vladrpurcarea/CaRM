;;;; google-auth.lisp

(in-package #:carm)

(defvar *priv-key*)
(defvar *access-token*)
(defvar *access-token-expiry-date*)

(defun setup-google-service-auth (rsa-key-path)
  (syslog :info "Reading RSA key for Google Service Account auth.")
  (setf *priv-key* (pem:read-from-file rsa-key-path))
  (when (not *priv-key*)
    (error (format nil "Could not read RSA key from ~A" rsa-key-path)))
  (refresh-access-token))

(defun refresh-access-token ()
  (syslog :info "Refreshing Google auth token.")
  (if-let ((response
	    (from-json
	     (drakma:http-request "https://oauth2.googleapis.com/token"
				  :method :POST
				  :parameters `(("grant_type" . "urn:ietf:params:oauth:grant-type:jwt-bearer")
						("assertion" . ,(make-signed-auth-token)))
				  :want-stream t))))
    (progn
      (setf *access-token* (gethash "access_token" response))
      (setf *access-token-expiry-date* (+ (get-universal-time)
					  (gethash "expires_in" response))))
    (error "Could not refresh token")))

(defun make-signed-auth-token ()
  (jose:encode :rs256 *priv-key*
	       (make-google-jwt-claim-set)))

(defun make-google-jwt-claim-set (&optional (duration-s 3500))
  (let ((now (to-1970-timestamp (get-universal-time))))
    `(("iss" . ,*google-iam-service-email*)
      ("scope" . "https://www.googleapis.com/auth/spreadsheets https://www.googleapis.com/auth/calendar")
      ("aud" . "https://oauth2.googleapis.com/token")
      ("iat" . ,now)
      ("exp" . ,(+ now duration-s)))))

(defmacro auth-google-req (uri &rest rest)
  `(labels
       ((auth-google-req-f (retries)
	  (when (> retries 0)
	    (multiple-value-bind
		  (stream status headers)
		(drakma:http-request ,uri
				     :additional-headers (list (get-auth-header))
				     :want-stream t
				     :content-type "application/json"
				     ,@rest)
	      (cond
		((or
		  (= status 401)
		  (= status 403))
		 (refresh-access-token)
		 (auth-google-req-f (1- retries)))
		((or (< status 200)
		     (> status 299))
		 (error "Google Request returned status ~A: ~A"
			status
			(uiop:slurp-stream-string stream)))
		(t (values stream status headers)))))))
     (auth-google-req-f 2)))

(defun get-auth-header ()
  (when (< (- *access-token-expiry-date*
	      (get-universal-time))
	   10)
    (refresh-access-token))
  (cons "Authorization"
	(format nil "Bearer ~A" *access-token*)))

(defun to-1970-timestamp (universal-time)
  (- universal-time 2208988800))

