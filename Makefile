LISP ?= sbcl
APPNAME ?= carm
WEB_DIR ?= web/

build:
	$(LISP) --dynamic-space-size 4096 \
		--load $(APPNAME).asd \
		--eval '(ql:quickload :$(APPNAME))' \
		--eval '(asdf:make :$(APPNAME))' \
		--eval '(quit)'
	cp -r $(WEB_DIR) $(APPNAME)/
