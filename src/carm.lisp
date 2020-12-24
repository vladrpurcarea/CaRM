;;;; carm.lisp

(in-package #:carm)

(defun bind-routes ()
  (defroute hello ("/hi2" :method :POST :decorators (@json)) ()
    (with-output-to-string (json)
      (yason:encode-alist '(("name" . "vlad"))))))

(defun main ()
  (let ((server (make-server)))
    (bind-routes)
    (hunchentoot:start server)
    (read)
    (hunchentoot:stop server)))
