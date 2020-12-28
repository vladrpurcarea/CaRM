;;;; database.lisp

(in-package #:carm)

(defvar *conn*)
(defvar *db-master-table-name* "carm_master")
(defvar *users-table-init-query*
  "CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username TEXT NOT NULL UNIQUE,
    password TEXT NOT NULL,
    email TEXT UNIQUE,
    enabled INTEGER NOT NULL,
    created_at INTEGER NOT NULL,
    timestamp INTEGER NOT NULL
);")
(defvar *sessions-table-init-query*
  "CREATE TABLE sessions (
    id INTEGER PRIMARY KEY,
    userid INTEGER NOT NULL,
    sessionid TEXT NOT NULL,
    created_at INTEGER NOT NULL,
    expires_at INTEGER NOT NULL,
    enabled INTEGER NOT NULL,
    FOREIGN KEY(userid) REFERENCES users(id)
  );")

(defvar *master-table-init-query*
  (format nil "CREATE TABLE ~A (id INTEGER PRIMARY KEY);" *db-master-table-name*))

(defun connect-to-db (location)
  (setf *conn*
	(dbi:connect :sqlite3
		     :database-name location)))

(defun disconnect-from-db (&optional (conn *conn*))
  (dbi:disconnect conn))

(defun db-fetch (query &optional (args '()))
  (dbi:fetch-all (dbi:execute (dbi:prepare *conn* query)
			      args)))

(define-condition db-no-result (error) ())
(define-condition db-more-than-one-result (error)
  ((results :initarg :results
	   :reader db-more-than-one-result-results)))
(defun db-fetch-one (query &key (args '()) (error-no-result nil) (error-more-results nil))
  (let ((res (dbi:fetch-all (dbi:execute (dbi:prepare *conn* query)
					 args))))
    (cond
      ((and error-no-result
	    (null res))
       (error 'db-no-result))
      ((or (not error-more-results)
	   (null (cdr res)))
       (car res))
      (t (error 'db-more-than-one-result :results res)))))

(defun db-exec (query &optional (args '()))
  (dbi:do-sql *conn* query args))

(defun setup-schema ()
  (labels ((schema-initialized-p ()
	     (db-fetch-one
	      "SELECT 1 FROM sqlite_master WHERE type='table' AND name = ?;"
	      :args (list *db-master-table-name*))))
    (when (not (schema-initialized-p))
      (db-exec *users-table-init-query*)
      (db-exec *master-table-init-query*)
      (db-exec *sessions-table-init-query*))))

(defun drop-schema ()
  (ignore-errors
   (db-exec "DROP TABLE IF EXISTS users;")
   (db-exec "DROP TABLE IF EXISTS carm_master;")
   (db-exec "DROP TABLE IF EXISTS sessions;")))
