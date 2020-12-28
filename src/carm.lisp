;;;; carm.lisp

(in-package #:carm)

(defvar *server* (make-server))

(defroute hello ("/get" :method :GET) ()
  (format nil "~S" *default-pathname-defaults*))

(defun main ()
  (hunchentoot:start *server*)
  (read)
  (hunchentoot:stop *server*))
