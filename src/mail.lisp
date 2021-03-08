;;;; mail.lisp

(in-package #:carm)

(defun send-mail (to subject message
		  &key
		    (from *smtp-user*)
		    (smtp-user *smtp-user*)
		    (smtp-pass *smtp-pass*)
		    (signature nil))
  (syslog :info "Sending mail to ~A with subject \"~A\"." to subject)
  (let* ((html-message (ppcre:regex-replace-all "\\n" message "<br/>"))
	 (html-message (if signature
			   (concatenate 'string
					html-message
					signature)
			   html-message)))
    (cl-smtp:send-email "smtp.variomedia.de"
			from
			to
			subject
			message
			:html-message html-message
			:ssl :tls
			:authentication (list smtp-user smtp-pass))))
