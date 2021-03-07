;;;; server.lisp

(in-package #:carm)

(setf hunchentoot:*hunchentoot-default-external-format*
hunchentoot::+utf-8+)

(defun make-server (&optional (port 4200) (document-root #p"./"))
  (make-instance 'easy-routes:easy-routes-acceptor
		 :port port
		 :document-root document-root))

(defun start-debug-swank-server (&optional (port 4201))
  (syslog :info "Starting debug Swank server on port ~D" port)
  (bt:make-thread (lambda ()
		    (swank:create-server :port port
					 :dont-close t))))

(defmacro @json-body () `(hunchentoot:session-value 'json-body))
(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (setf (@json-body)
	(from-json
	 (hunchentoot:raw-post-data)))
  (funcall next))

(defun @json-out (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun json-symbol-key-encoder (key)
  (str:camel-case (str:downcase (string key))))
(setf yason:*symbol-key-encoder* #'json-symbol-key-encoder)

(defun encode-keys-to-strings (list)
  (mapcar (lambda (x)
	    (typecase x
	      (keyword (json-symbol-key-encoder x))
	      (otherwise x)))
	  list))

(defun to-json (o &optional format)
  (with-output-to-string (json)
    (cond
      ((eq format :alist) (yason:encode-alist o json))
      ((eq format :plist) (yason:encode-plist o json))
      ((eq format :hash-table) (yason:encode o json))
      (t
       (typecase o
	 (cons (if (listp (car o))
		   (yason:encode-alist o json)
		   (yason:encode-plist o json)))
	 (otherwise (yason:encode o json)))))))

(defun from-json (stream &optional (format :hash-table))
  (yason:parse stream :object-as format))

(defun get-basic-auth-header ()
  (let ((header (hunchentoot:header-in* :authorization)))
    (when header
      (subseq header 6))))

(defmacro @userid () `(hunchentoot:session-value 'auth-userid))
(defun @auth (next)
  (setf (@userid) (get-userid-by-valid-session (get-basic-auth-header)))
  (if (null (@userid))
      (http-403-forbidden)
      (funcall next)))

(defun @log-errors (next)
  (handler-case (funcall next)
    (error (e) (syslog :err "Lisp Error: ~A" e))))
(defmacro log-errors (&body body)
  `(@log-errors (lambda () ,@body)))

(defun http-204-no-content ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  "")

(defun http-400-bad-request (&optional (content ""))
  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
  content)

(defun http-403-forbidden (&optional (content ""))
  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
  content)

(defun http-404-not-found (&optional (content ""))
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  content)
