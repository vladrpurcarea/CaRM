;;;; config.lisp

(in-package #:carm)

(defvar *base-path*)
(defvar *base-web-path*)
(defvar *port*)
(defvar *cr-forbidden-fields*)
(defvar *cr-required-fields*)
(defvar *mail-type*)
(defvar *smtp-server*)
(defvar *smtp-user*)
(defvar *smtp-pass*)
(defvar *google-rsa-key-path*)
(defvar *google-iam-service-email*)
(defvar *contact-request-spreadsheet-id*)
(defvar *contact-request-notification-email*)
(defvar *contact-request-sheet-names*)

(defun setup-config (path)
  (syslog :info "Reading config from ~A" path)
  (labels ((get-directory-pathname (path)
	     (make-pathname :directory (pathname-directory path))))
    (with-open-file (fd path)
      (let ((conf-ht (from-json fd)))
	(when (not conf-ht)
	  (error "Could not read config file"))
	(setf *base-path* (pathname (gethash "basePath" conf-ht (get-directory-pathname path))))
	(syslog :info "Base path: ~A" *base-path*)
	(setf *base-web-path* (gethash "baseWebPath" conf-ht *base-path*))
	(setf *port* (gethash "port" conf-ht 4200))
	(syslog :info "Port: ~D" *port*)
	(setf *cr-required-fields* (gethash "contactRequiredFields" conf-ht '()))
	(setf *cr-forbidden-fields* (gethash "contactForbiddenFields" conf-ht '()))
	(setf *mail-type* (gethash "mailType" conf-ht))
	(setf *smtp-server* (gethash "smtpServer" conf-ht))
	(setf *smtp-user* (gethash "smtpUser" conf-ht))
	(setf *smtp-pass* (gethash "smtpPass" conf-ht))
	(setf *google-rsa-key-path* (gethash "googleRsaKeyPath" conf-ht
					     (merge-pathnames *base-path* "keyrsa.pem")))
	(setf *google-iam-service-email* (gethash "googleIamServiceEmail" conf-ht))
	(setf *contact-request-spreadsheet-id* (gethash "contactRequestSpreadsheetId" conf-ht))
	(setf *contact-request-notification-email* (gethash "contactRequestNotificationEmail" conf-ht))
	(setf *contact-request-sheet-names* (gethash "contactRequestSheetNames" conf-ht))))))
