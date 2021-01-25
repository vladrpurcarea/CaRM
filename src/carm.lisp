;;;; carm.lisp

(in-package #:carm)

(defvar *server*)

(defun main ()
  (syslog :info "Initializing.")
  (let ((args (uiop:command-line-arguments)))
    (when (not (= (length args)
		  1))
      (format t "Usage: carm path/to/carm.conf.~%")
      (uiop:quit 1))
    (setup-config (merge-pathnames (pathname (car args)) (uiop:getcwd)))

    (when *daemonize*
      (syslog :info "Daemonizing myself.")
      (daemon:daemonize :exit-parent t))
    (start-debug-swank-server)
    (connect-to-db (merge-pathnames *db-filename* *base-path*))
    (setup-schema)
    (setup-google-service-auth *google-rsa-key-path*)
    (setup-gsheets)
    (setup-cron-jobs)
    (setf *server* (make-server *port* (merge-pathnames #P "web/" *base-web-path*)))
    (syslog :info "Starting server on port ~D." *port*)
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
            (syslog :info "Closing.")
            (hunchentoot:stop *server*)
	    (disconnect-from-db)
	    (when *daemonize*
	      (daemon:exit))
            (uiop:quit)))
      (error (c) (format t "Unknown error:~&~a~&" c)))))
