;;;; log.lisp

(in-package #:carm)

(defmacro syslog (level format-string &rest format-args)
  `(cl-syslog:log "CaRM" :user ,level (format nil ,format-string ,@format-args)))
