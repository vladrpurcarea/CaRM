;;;; appointments.lisp

(in-package #:carm)

;;; WEB

(defroute post-appointment-route
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
			    (assert-type (gethash "photoshootPackage" (@json-body)) 'string)
			    :confirmed (if (gethash "confirmed" (@json-body)) 1 0))
	(http-204-no-content))
    (type-error (e) (http-400-bad-request (write e :escape nil)))))


(defroute post-appointment-email-template-route
    ("/carm/api/v1/appointment/template"
     :method :POST
     :decorators (@log-errors @auth @json))
    ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (handler-case
      (concretize-email-from-appointment (@json-body))
    (error (e) (write e :escape nil))))

(defroute get-appointments-route
    ("/carm/api/v1/appointment"
     :method :GET
     :decorators (@log-errors @auth @json-out))
    (&get offset limit)
  (let ((offset (if offset
		    (parse-integer offset)
		    0)); default
	(limit (if limit
		   (min (parse-integer limit)
			100) ;max
		   20))) ;default
    (to-json
     (alist-hash-table
      `(("appointments" . ,(mapcar #'plist-hash-table
				   (get-appointments offset limit))))))))

(defroute get-appointment-route
    ("/carm/api/v1/appointment/:id"
     :method :GET
     :decorators (@log-errors @auth @json-out))
    ()
  (to-json (get-appointment id)))

(defroute delete-cancel-appointment-route
    ("/carm/api/v1/appointment/:id/cancel"
     :method :DELETE
     :decorators (@log-errors @auth))
    ()
  (cancel-appointment id))

(defroute put-appointment-confirmed-route
    ("/carm/api/v1/appointment/:id/confirmed"
     :method :PUT
     :decorators (@log-errors @auth))
    ()
  (if (set-appointment-confirmed id t)
      (http-204-no-content)
      (http-404-not-found)))

(defroute delete-appointment-confirmed-route
    ("/carm/api/v1/appointment/:id/confirmed"
     :method :DELETE
     :decorators (@log-errors @auth))
    ()
  (if (set-appointment-confirmed id nil)
      (http-204-no-content)
      (http-404-not-found)))

;;; INTERNAL

(defun get-appointment (id)
  (car (db-fetch "SELECT id, host, customer_name, telephone, email, email_text, start_time, end_time, price, currency, photographer,photoshoot_address, photoshoot_type, photoshoot_package, confirmed, processed_email, processed_email_reminder, created_at FROM appointments WHERE id = ?;"
		 (list id))))

(defun get-appointments (offset limit)
  (syslog :info "Getting appointment list with offset ~A limit ~A" offset limit)
  (db-fetch "SELECT id, host, customer_name, telephone, email, email_text, start_time, end_time, price, currency, photographer,photoshoot_address, photoshoot_type, photoshoot_package, confirmed, created_at, processed_email, processed_email_reminder FROM appointments ORDER BY created_at DESC LIMIT ? OFFSET ?;"
	    (list limit offset)))

(defun create-appointment (host
			   customer-name
			   telephone
			   email
			   email-text
			   start-time
			   end-time
			   price
			   currency
			   photographer
			   photoshoot-address
			   photoshoot-type
			   photoshoot-package
			   &key
			     (confirmed nil))
  (syslog :info "Creating new appointment from ~A" host)
  (db-exec "INSERT INTO appointments (host, customer_name, telephone, email, email_text, start_time, end_time, price, currency, photographer,photoshoot_address, photoshoot_type, photoshoot_package, confirmed) 
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
	   (list host customer-name telephone email email-text start-time end-time price currency photographer
		 photoshoot-address photoshoot-type photoshoot-package confirmed)))

(defun cancel-appointment (appt-id)
  (let ((gcalendar-id (getf (car
			     (db-fetch "SELECT gcalendar_id FROM appointments WHERE id = ?"
				       (list appt-id)))
			    :|gcalendar_id|)))
    (when gcalendar-id
      (gcalendar-delete-event gcalendar-id))
    (db-exec "DELETE FROM appointments WHERE id = ?" (list appt-id))))

(defun process-appointments ()
  (process-appointments-to-gcalendar)
  (process-appointments-to-email))

(defun process-appointments-to-gcalendar ()
  (when *appointment-calendar-id*
    (syslog :info "Processing unprocessed appointments to Google Calendar.")
    (let ((unproc-appointments
	    (db-fetch "SELECT id, host, customer_name, telephone, email, start_time, end_time, price, currency, 
                       photographer, photoshoot_type, photoshoot_package, confirmed FROM appointments WHERE processed_calendar = 0")))
      (loop for appmnt in (mapcar #'plist-hash-table unproc-appointments)
	    do (let* ((summary (format nil "~A~A ~A ~A ~F~A"
				       (if (gethash :|confirmed| appmnt) "(?) " "")
				       (gethash :|customer_name| appmnt)
				       (str:capitalize (gethash :|photoshoot_type| appmnt))
				       (str:capitalize (gethash :|photoshoot_package| appmnt))
				       (gethash :|price| appmnt)
				       (gethash :|currency| appmnt)))
		      (description (format nil
					   "Site: ~A~%Telephone: ~A~%Email: ~A~%Package: ~A ~A~%Photographer: ~A~%Link: ~A"
					   (gethash :|host| appmnt)
					   (gethash :|telephone| appmnt)
					   (gethash :|email| appmnt)
					   (gethash :|photoshoot_type| appmnt)
					   (gethash :|photoshoot_package| appmnt)
					   (gethash :|photographer| appmnt)
					   (format nil "https://bergmann-fotografin-muenchen.de/carm/static/viewappointment.html?id=~D" (gethash :|id| appmnt))))
		      (insert-event-result (gcalendar-insert-event
					    (gethash :|start_time| appmnt)
					    (+ (gethash :|end_time| appmnt)
					       1800) ; +30 minutes during corona time, only in gcalendar
					    summary
					    :description description
					    :color-id (get-appointment-color (gethash :|photographer| appmnt)
									  (gethash :|confirmed| appmnt))))
		      (event-id (gethash "id" insert-event-result))) ;; TODO: event :color
		 (db-exec "UPDATE appointments SET processed_calendar = 1, gcalendar_id = ? WHERE id = ?"
			  (list event-id  (gethash :|id| appmnt)))))))) 

(defun get-appointment-color (photographer confirmed)
  (cond
    ((= confirmed 0) 8) ; grey
    ((string-equal photographer "Carmen Bergmann") 6) ; red
    (t 5) ; yellow
    ))

(defun process-appointments-to-email ()
  (syslog :info "Processing appointments to email.")
  (let ((unproc-appointments (db-fetch "SELECT id, email, email_text FROM appointments 
                                        WHERE processed_email = 0
                                        AND confirmed = 1
                                        AND created_at < datetime(CURRENT_TIMESTAMP, '-3 hours')")))
    (loop for appmnt in (mapcar #'plist-hash-table unproc-appointments)
	  do (progn
	       (send-mail (gethash :|email| appmnt)
			  "Booking Confirmation"
			  (gethash :|email_text| appmnt)
			  :signature (get-email-signature))
	       (db-exec "UPDATE appointments SET processed_email = ? WHERE id = ?"
			(list (get-universal-time)
			      (gethash :|id| appmnt)))))))

(defun process-appointment-reminders-to-email ()
  (syslog :info "Processing appointment reminders to email.")
  (let ((unproc-appointments (db-fetch "SELECT id, email, email_text FROM appointments 
                                        WHERE processed_email = 1 
                                        AND processed_email_reminder = 0
                                        AND confirmed = 1
                                        AND created_at < datetime(CURRENT_TIMESTAMP, '-2 days')
                                        AND start_time - ? < 115200"  ; 1 day and 8 hours
				        (list (get-universal-time)))))
    (loop for appmnt in (mapcar #'plist-hash-table unproc-appointments)
	  do (progn
	       (send-mail (gethash :|email| appmnt)
			  "Booking Confirmation Reminder"
			  (gethash :|email_text| appmnt)
			  :signature (get-email-signature))
	       (db-exec "UPDATE appointments SET processed_email_reminder = ? WHERE id = ?"
			(list (get-universal-time)
			      (gethash :|id| appmnt)))))))

(defun set-appointment-confirmed (id confirmed)
  (not (zerop
	(db-exec "UPDATE appointments SET confirmed = ? WHERE id = ?"
		 (list (if confirmed 1 0)
		       id)))))

