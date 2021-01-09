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
	       #:str
	       #:qbase64
	       #:secure-random
	       #:alexandria
	       #:bordeaux-threads
	       #:drakma
	       #:jose
	       #:ironclad)
  :components ((:file "package")
	       (:file "src/config")
	       (:file "src/server")
	       (:file "src/database")
	       (:file "src/auth")
	       (:file "src/contact-requests")
	       (:file "src/carm")
	       (:file "src/google"))
  :build-operation "program-op"
  :build-pathname "carm/carm"
  :entry-point "carm:main")
