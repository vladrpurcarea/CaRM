;;;; google.lisp

(in-package #:carm)

(defvar *priv-key*)
(defvar *access-token*)
(defvar *access-token-expiry-date*)

(defun setup-google-service-auth (rsa-key-path)
  (setf *priv-key* (pem:read-from-file rsa-key-path))
  (when (not *priv-key*)
    (error (format nil "Could not read RSA key from ~A" rsa-key-path)))
  (refresh-access-token))

(defun refresh-access-token ()
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
    `(("iss" . "bergmannager@bergmannager.iam.gserviceaccount.com")
      ("scope" . "https://www.googleapis.com/auth/devstorage.read_only")
      ("aud" . "https://oauth2.googleapis.com/token")
      ("iat" . ,now)
      ("exp" . ,(+ now duration-s)))))

(defun to-1970-timestamp (universal-time)
  (- universal-time 2208988800))

