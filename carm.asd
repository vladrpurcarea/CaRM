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
	       #:qbase64)
  :components ((:file "package")
	       (:file "src/server")
	       (:file "src/database")
	       (:file "src/auth")
	       (:file "src/carm"))
  :build-operation "program-op"
  :build-pathname "carm/carm"
  :entry-point "carm:main")
