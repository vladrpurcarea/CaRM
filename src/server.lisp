;;;; server.lisp

(in-package #:carm)

(defun make-server (&optional (port 4200) (document-root #p"./"))
  (make-instance 'easy-routes:easy-routes-acceptor
		 :port port
		 :document-root document-root))

(defmacro @json-body () `(hunchentoot:session-value 'json-body))
(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (setf (@json-body)
	(yason:parse 
	 (hunchentoot:raw-post-data :want-stream t)))
  (funcall next))

(defun @json-out (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun to-json (o)
  (with-output-to-string (json)
    (typecase o
      (cons (yason:encode-alist o json))
      (otherwise (yason:encode o json)))))

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


(defun http-204-no-content ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  "")

(defun http-403-forbidden ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
  "")
