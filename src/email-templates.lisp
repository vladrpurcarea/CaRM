;;;; email-templates.lisp

(in-package #:carm)

(defvar *email-templates*)
(defvar *months*
  '("Januar" "Februar" "März" "April" "Mai" "Juni" "Juli"
    "August" "September" "Oktober" "November" "Dezember"))

(defun setup-email-templates (&optional (email-templates-path *email-templates-path*))
  (read-email-templates email-templates-path))

(defun read-email-templates (&optional (email-templates-path *email-templates-path*))
  (setf *email-templates* (make-hash-table :test 'equalp))
  (loop for file in (directory (merge-pathnames email-templates-path #P "*.template"))
	do (let ((filename (str:replace-all ".template" ""
					    (file-namestring file)))
		 (file (uiop:read-file-string file)))
	     (setf (gethash filename *email-templates*) file))
	finally (return *email-templates*)))

(defun concretize-email-from-appointment (appointment-ht)
  (let* ((aht (alexandria:copy-hash-table appointment-ht))
	 (photoshoot-type (gethash "photoshootType" aht))
	 (template (gethash photoshoot-type *email-templates*)))
    (setf (gethash "date" aht)
	  (to-day-month-date (gethash "startTime" aht)))
    (setf-apply (gethash "startTime" aht) #'to-hour-minute)
    (setf-apply (gethash "endTime" aht) #'to-hour-minute)
    (concretize-template template aht)))

(defun to-day-month-date (universal-time)
  (multiple-value-bind (sec min hour date month)
      (decode-universal-time universal-time)
    (declare (ignore sec min hour))
    (format nil "~d. ~A" date (nth (1- month) *months*))))
(defun to-hour-minute (universal-time)
  (multiple-value-bind (sec min hour)
      (decode-universal-time universal-time)
    (declare (ignore sec))
    (format nil "~2,'0d:~2,'0d" hour min)))

(defun concretize-template (template values-ht)
  (reduce (lambda (acc k) (replace-in-template acc
					       k
					       (format nil "~A" (gethash k values-ht))))
	  (alexandria:hash-table-keys values-ht)
	  :initial-value template))

(defun replace-in-template (template key val)
  (let* ((regex (str:concat "\\$\\{" key "\\}")))
    (multiple-value-bind (new-template replaced-p)
	(ppcre:regex-replace regex template val)
      (declare (ignore replaced-p))
      new-template)))
