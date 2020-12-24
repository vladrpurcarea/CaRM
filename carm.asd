;;;; carm.asd

(asdf:defsystem #:carm
  :author "Vlad Purcarea <vladrpurcarea@gmail.com>"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "src/carm"))
  :build-operation "program-op"
  :build-pathname "carm/carm"
  :entry-point "carm:main")
