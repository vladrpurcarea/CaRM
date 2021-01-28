LISP ?= sbcl
APPNAME ?= carm
WEB_DIR ?= web/

build:
	$(LISP) --dynamic-space-size 4096 \
		--load $(APPNAME).asd \
		--eval '(ql:quickload :$(APPNAME))' \
		--eval '(asdf:make :$(APPNAME))' \
		--eval '(quit)'
	cp -rp $(WEB_DIR) $(APPNAME)/
	cp -p scripts/logs.sh $(APPNAME)/
	cp -p  scripts/restart.sh $(APPNAME)/

install:
	sudo cp -rpT $(APPNAME)/ /etc/$(APPNAME)
	sudo chown -R "root:root" /etc/$(APPNAME)
	sudo cp carm.service /etc/systemd/system/
	sudo chmod 664 /etc/systemd/system/carm.service
	sudo systemctl daemon-reload
