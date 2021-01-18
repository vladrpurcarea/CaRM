;;;; carm.lisp

(in-package #:carm)

(defvar *server*)

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (when (not (= (length args)
		  1))
      (format t "Usage: carm path/to/carm.conf.~%")
      (uiop:quit 1))
    (setup-config (merge-pathnames (pathname (car args)) (uiop:getcwd)))
    (daemon:daemonize :exit-parent t)
    (connect-to-db (merge-pathnames *db-filename* *base-path*))
    (setup-schema)
    (setup-google-service-auth *google-rsa-key-path*)
    (setup-gsheets)
    (setup-cron-jobs)
    (setf *server* (make-server *port* (merge-pathnames #P "web/" *base-web-path*)))
    (hunchentoot:start *server*)
    (handler-case (bt:join-thread
		   (find-if (lambda (th)
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
	    (daemon:exit)
            (uiop:quit)))
      (error (c) (format t "Unknown error:~&~a~&" c)))))
