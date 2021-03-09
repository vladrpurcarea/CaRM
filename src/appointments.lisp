;;;; appointments.lisp

(in-package #:carm)

;;; WEB

(defroute post-appointment
    ("/carm/api/v1/appointment"
     :method :POST
     :decorators (@log-errors @auth @json))
    ()
  (handler-case
      (progn
	(create-appointment (assert-type (gethash "host" (@json-body)) 'string)
			    (assert-type (gethash "customerName" (@json-body)) 'string)
			    (gethash "telephone" (@json-body))
			    (assert-type (gethash "email" (@json-body)) 'string)
			    (assert-type (gethash "emailText" (@json-body)) 'string)
			    (assert-type (gethash "startTime" (@json-body)) 'fixnum)
			    (assert-type (gethash "endTime" (@json-body)) 'fixnum)
			    (assert-type (gethash "price" (@json-body)) 'real)
			    (assert-type (gethash "currency" (@json-body)) 'string)
			    (assert-type (gethash "photographer" (@json-body)) 'string)
			    (assert-type (gethash "photoshootAddress" (@json-body)) 'string)
			    (assert-type (gethash "photoshootType" (@json-body)) 'string)
			    (assert-type (gethash "photoshootPackage" (@json-body)) 'string))
	(http-204-no-content))
    (type-error (e) (http-400-bad-request (write e :escape nil)))))


(defroute post-appointment-email-template
    ("/carm/api/v1/appointment/template"
     :method :POST
     :decorators (@log-errors @auth @json))
    ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (handler-case
      (concretize-email-from-appointment (@json-body))
    (error (e) (write e :escape nil))))

;;; INTERNAL

(defun create-appointment (host customer-name telephone email email-text start-time end-time price currency photographer
			   photoshoot-address photoshoot-type photoshoot-package)
  (syslog :info "Creating new appointment from ~A" host)
  (db-exec "INSERT INTO appointments (host, customer_name, telephone, email, email_text, start_time, end_time, price, currency, photographer,
                                      photoshoot_address, photoshoot_type, photoshoot_package) 
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
	   (list host customer-name telephone email email-text start-time end-time price currency photographer
		 photoshoot-address photoshoot-type photoshoot-package)))

(defun process-appointments ()
  (process-appointments-to-gcalendar)
  (process-appointments-to-email))

(defun process-appointments-to-gcalendar ()
  (when *appointment-calendar-id*
    (syslog :info "Processing unprocessed appointments to Google Calendar.")
    (let ((unproc-appointments
	    (db-fetch "SELECT id, host, customer_name, telephone, email, start_time, end_time, price, currency, 
                       photographer, photoshoot_type, photoshoot_package FROM appointments WHERE processed_calendar = 0")))
      (loop for appmnt in (mapcar #'plist-hash-table unproc-appointments)
	    do (let ((summary (format nil "~A ~A ~A ~F~A"
				      (gethash :|customer_name| appmnt)
				      (str:capitalize (gethash :|photoshoot_type| appmnt))
				      (str:capitalize (gethash :|photoshoot_package| appmnt))
				      (gethash :|price| appmnt)
				      (gethash :|currency| appmnt)))
		     (description (format nil
					  "Site: ~A~%Telephone: ~A~%Email: ~A~%Package: ~A ~A~%Photographer: ~A"
					  (gethash :|host| appmnt)
					  (gethash :|telephone| appmnt)
					  (gethash :|email| appmnt)
					  (gethash :|photoshoot_type| appmnt)
					  (gethash :|photoshoot_package| appmnt)
					  (gethash :|photographer| appmnt))))
		 (gcalendar-insert-event (gethash :|start_time| appmnt)
					 (gethash :|end_time| appmnt)
					 summary
					 :description description) ;; TODO: event :color
		 (db-exec "UPDATE appointments SET processed_calendar = 1 WHERE id = ?"
			  (list (gethash :|id| appmnt)))))))) 

(defun process-appointments-to-email ()
  (syslog :info "Processing appointments to email.")
  (let ((unproc-appointments (db-fetch "SELECT id, email, email_text FROM appointments WHERE processed_email = 0")))
    (loop for appmnt in (mapcar #'plist-hash-table unproc-appointments)
	  do (progn
	       (send-mail (gethash :|email| appmnt)
			  "Booking"
			  (gethash :|email_text| appmnt)
			  :signature (get-email-signature))
	       (db-exec "UPDATE appointments SET processed_email = 1 WHERE id = ?"
			(list (gethash :|id| appmnt)))))))