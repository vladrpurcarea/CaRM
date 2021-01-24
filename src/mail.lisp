;;;; mail.lisp

(in-package #:carm)

(defun send-mail (to subject message
		  &key
		    (from *smtp-user*)
		    (smtp-user *smtp-user*)
		    (smtp-pass *smtp-pass*))
  (syslog :info "Sending mail to ~A with subject \"~A\"." to subject)
  (cl-smtp:send-email "smtp.variomedia.de"
		      from
		      to
		      subject
		      message
		      :ssl :tls
		      :authentication (list smtp-user smtp-pass)))
