;;;; carm.lisp

(in-package #:carm)

(defvar *server* (make-server))

(defroute hello ("/get" :method :GET
			:decorators (@auth))
    ()
  (format nil "~A" (@userid)))

(defun main ()
  (connect-to-db "/home/v28p/projects/scraps/test123.sqlite3")
  (setup-schema)
  (hunchentoot:start *server*)
  (read)
  (hunchentoot:stop *server*)
  (disconnect-from-db))
