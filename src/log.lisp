;;;; log.lisp

(in-package #:carm)

(defmacro syslog (level format-string &rest format-args)
  `(cl-syslog:log "carm" :user ,level (remove #\Newline (format nil ,format-string ,@format-args))))
