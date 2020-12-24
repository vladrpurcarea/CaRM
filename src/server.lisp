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

(defun @auth (next)
  "TODO"
  (funcall next))
