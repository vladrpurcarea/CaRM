;;;; email-templates.lisp

(in-package #:carm)

(defvar *email-templates*)
(defvar *months*
  '("Januar" "Februar" "März" "April" "Mai" "Juni" "Juli"
    "August" "September" "Oktober" "November" "Dezember"))
(defvar *gallery-opening-time* (* 60 60 24 21)) ;; 21 days

(defun setup-email-templates (&optional (email-templates-path *email-templates-path*))
  (read-email-templates email-templates-path))

(defun read-email-templates (&optional (email-templates-path *email-templates-path*))
  (setf *email-templates* (make-hash-table :test 'equalp))
  (loop for file in (directory (merge-pathnames email-templates-path #P "*.template"))
	do (let ((filename (str:replace-all ".template" ""
					    (file-namestring file)))
		 (file (uiop:read-file-string file :external-format :utf8)))
	     (setf (gethash filename *email-templates*) file))
	finally (return *email-templates*)))

(defun concretize-email-from-appointment (appointment-ht)
  (let* ((aht (alexandria:copy-hash-table appointment-ht))
	 (photoshoot-type (gethash "photoshootType" aht))
	 (template (gethash photoshoot-type *email-templates*))
	 (start-time (gethash "startTime" aht))
	 (end-time (gethash "endTime" aht)))
    (setf (gethash "date" aht)
	  (to-day-month-date start-time))
    (setf (gethash "galleryOpeningDate" aht)
	  (to-day-month-date (+ *gallery-opening-time*
				start-time)))
    (setf (gethash "duration" aht)
	  (to-duration (- end-time start-time)))
    (setf-apply (gethash "startTime" aht) #'to-hour-minute)
    (setf-apply (gethash "endTime" aht) #'to-hour-minute)
    (concretize-template template aht)))

(defun get-email-signature ()
  (gethash "signature" *email-templates*))

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
(defun to-duration (seconds)
  (let* ((hours (round (/ seconds 3600)))
	 (minutes (round (/ (- seconds (* hours 3600))
			    60))))
    (if (> hours 0)
	(if (not (zerop minutes))
	    (format nil "~d Stunde ~d Minuten" hours minutes)
	    (format nil "~d Stunde" hours))
      (format nil "~d Minuten" minutes))))

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
