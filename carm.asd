;;;; carm.asd

(asdf:defsystem #:carm
  :author "Vlad Purcarea <vladrpurcarea@gmail.com>"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
	       #:easy-routes
	       #:yason
	       #:cl-dbi
	       #:cl-pass
	       #:cl-ppcre
	       #:str
	       #:qbase64
	       #:secure-random
	       #:alexandria
	       #:bordeaux-threads
	       #:drakma
	       #:jose
	       #:ironclad
	       #:pem
	       #:cl-cron
	       #:cl-smtp
	       #:daemon
	       #:swank
	       #:cl-syslog
	       #:local-time
	       #:trivial-backtrace)
  :components ((:file "package")
	       (:file "src/log")
	       (:file "src/util")
	       (:file "src/config")
	       (:file "src/server")
	       (:file "src/database")
	       (:file "src/auth")
	       (:file "src/spam")
	       (:file "src/email-templates")
	       (:file "src/contact-requests")
	       (:file "src/appointments")
	       (:file "src/carm")
	       (:file "src/google-auth")
	       (:file "src/google-sheets")
	       (:file "src/google-calendar")
	       (:file "src/mail")
	       (:file "src/cron"))
  :build-operation "program-op"
  :build-pathname "carm/carm"
  :entry-point "carm:main")
