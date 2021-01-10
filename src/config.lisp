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

(defun setup-config (path)
  (labels ((get-directory-pathname (path)
	     (make-pathname :directory (pathname-directory path))))
    (with-open-file (fd path)
      (let ((conf-ht (from-json fd)))
	(when (not conf-ht)
	  (error "Could not read config file"))
	(setf *base-path* (pathname (gethash "basePath" conf-ht (get-directory-pathname path))))
	(setf *base-web-path* (gethash "baseWebPath" conf-ht *base-path*))
	(setf *port* (gethash "port" conf-ht 4200))
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
	(setf *contact-request-notification-email* (gethash "contactRequestNotificationEmail" conf-ht))))))
