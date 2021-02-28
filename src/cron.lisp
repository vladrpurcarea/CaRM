;;;; cron.lisp

(in-package #:carm)

(defun setup-cron-jobs ()
  (cl-cron:make-cron-job
   #'process-contact-requests
   :hash-key :process-contact-requests)
  (cl-cron:make-cron-job
   #'process-appointments
   :hash-key :process-appointments)
  (cl-cron:start-cron))
