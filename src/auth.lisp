;;;; auth.lisp

(in-package #:carm)

;;; WEB

(defroute post-auth
    ("/carm/api/v1/auth"
     :method :POST
     :decorators (@log-errors @json-out))
    ()
  (multiple-value-bind (username password)
      (hunchentoot:authorization)
    (if (or (null username) (null password))
	(http-403-forbidden)
	(let ((userid (get-userid-by-user-pass username password)))
	  (if userid
	      (to-json `(("sessionId" . ,(create-session userid))))
	      (http-403-forbidden))))))

(defroute delete-auth
    ("/carm/api/v1/auth"
     :method :DELETE
     :decorators (@log-errors))
    ()
  (invalidate-session (get-basic-auth-header))
  (http-204-no-content))

(defroute get-user ("/carm/api/v1/user" :method :GET
					:decorators (@auth @json-out))
    ()
  (to-json (get-user-by-id (@userid))))

;;; INTERNAL

(defun create-user (username unhashed-pass &key (email nil))
  (let ((time (get-universal-time)))
    (when (zerop
	   (db-exec "INSERT INTO users (username, password, email, enabled, created_at, timestamp)
              VALUES (?, ?, ?, ?, ?, ?);"
		    (list username
			  (cl-pass:hash unhashed-pass)
			  email
			  1
			  time
			  time)))
      (error "Could not create user"))))

(defun get-user-by-id (userid)
  (db-fetch-one "SELECT username, email, created_at FROM users where id = ?;"
		:args (list userid)
		:error-more-results t))

(defun get-userid-by-user-pass (username unhashed-pass)
  (handler-case
      (let* ((res
	       (db-fetch-one "SELECT id, password FROM users WHERE username = ?;"
			     :args (list username)
			     :error-no-result t
			     :error-more-results t))
	     (hashed-pass (getf res :|password|))
	     (id (getf res :|id|)))
	(when (cl-pass:check-password unhashed-pass hashed-pass)
	  id))
    (db-no-result nil)))

(defun get-userid-by-valid-session (sessionid)
  (let ((res (db-fetch-one "SELECT userid FROM sessions where enabled = 1 AND sessionid = ? AND expires_at > ?"
			   :args (list sessionid (get-universal-time))
			   :error-more-results t)))
    (when res
      (getf res :|userid|))))

(defun create-session (userid)
  (let* ((cur-time (get-universal-time))
	 (expiration-time (+ cur-time
			     (* 60 60 24 2)))
	(sessionid (qbase64:encode-bytes (secure-random:bytes 32 secure-random:*generator*))))
    (db-exec "INSERT INTO sessions (userid, sessionid, created_at, expires_at, enabled)
              VALUES (?, ?, ?, ?, ?);"
	     (list userid
		   sessionid
		   cur-time
		   expiration-time
		   1))
    sessionid))

(defun invalidate-session (sessionid)
  (db-exec "UPDATE sessions SET enabled = 0, expires_at = ? WHERE sessionid = ?"
	   (list (get-universal-time)
		 sessionid)))

