;;;; util.lisp

(in-package #:carm)

(defmacro assert-type (x type)
  `(if (typep ,x ,type)
       ,x
       (error (make-condition 'type-error
			      :datum ,x
			      :expected-type ,type))))
(defun universal-time->rfc3339 (utime)
  (local-time:format-rfc3339-timestring
   nil
   (local-time:universal-to-timestamp utime)))

(defmacro setf-apply (place fn)
  `(setf ,place
	 (funcall ,fn ,place)))
