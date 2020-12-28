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
  (handler-case (bt:join-thread (find-if (lambda (th)
                                            (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format t "Closing..")
           (hunchentoot:stop *server*)
	   (disconnect-from-db)
           (uiop:quit)))
    (error (c) (format t "Unknown error:~&~a~&" c))))
