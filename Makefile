LISP ?= sbcl
APPNAME ?= carm

build:
	$(LISP) --dynamic-space-size 4096 \
		--load $(APPNAME).asd \
		--eval '(ql:quickload :$(APPNAME))' \
		--eval '(asdf:make :$(APPNAME))' \
		--eval '(quit)'
