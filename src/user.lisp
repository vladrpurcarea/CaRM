;;;; auth.lisp

(in-package #:carm)

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

(defun correct-auth-p (username unhashed-pass)
  (handler-case (let ((hashed-pass
			(getf (db-fetch-one "SELECT password FROM users WHERE username = ?;"
					    (list username)
					    t)
			      :|password|)))
		  (cl-pass:check-password unhashed-pass hashed-pass))
    (db-no-result () nil)))

