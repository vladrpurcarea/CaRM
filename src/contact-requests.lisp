;;;; contact-requests.lisp

(in-package #:carm)

;;; WEB

(defroute post-contact-request
    ("/carm/api/v1/contact-request"
     :method :POST
     :decorators (@log-errors @json))
    ()
  (let* ((forbidden-fields-p
	   (loop for k being the hash-keys of (@json-body)
		 using (hash-value v)
		 when (and (member k *cr-forbidden-fields* :test #'string-equal)
			   (not (str:empty? v)))
		   return t))
	 (spam-p (or forbidden-fields-p
		     (spam-filter (gethash "message" (@json-body)))))
	 (required-fields-p
	   (loop for f in *cr-required-fields*
		 when (not (gethash f (@json-body)))
		   return nil
		 finally (return t))))
    (if required-fields-p
	(progn
	  (create-contact-request (to-json (@json-body))
				  (if spam-p 1 0)
				  (gethash "host" (@json-body)
					   (str:replace-all "www."
							    ""
							    (hunchentoot:host))))
	  (http-204-no-content))
	(http-400-bad-request (to-json `(("error" . "Required fields missing")))))))

(defroute get-contact-requests-route
    ("/carm/api/v1/contact-request"
     :method :GET
     :decorators (@log-errors @auth @json-out))
    (&get offset limit spam)
  (let ((offset (if offset
		    (parse-integer offset)
		    0)); default
	(limit (if limit
		   (min (parse-integer limit)
			100) ;max
		   20))) ;default
    (to-json
     (alist-hash-table
      `(("contactRequests"
	 . ,(get-contact-requests offset limit spam)))))))

(defroute get-contact-request-route
    ("/carm/api/v1/contact-request/:id"
     :method :GET
     :decorators (@log-errors @auth @json-out))
    ()
  (if-let ((contact-request (get-contact-request id)))
    (to-json contact-request)
    (http-404-not-found)))


(defroute put-contact-request-seen-route
    ("/carm/api/v1/contact-request/:id/seen"
     :method :PUT
     :decorators (@log-errors @auth))
    ()
  (if (set-contact-request-seen id t)
      (http-204-no-content)
      (http-404-not-found)))



(defroute delete-contact-request-seen-route
    ("/carm/api/v1/contact-request/:id/seen"
     :method :DELETE
     :decorators (@log-errors @auth))
    ()
  (if (set-contact-request-seen id nil)
      (http-204-no-content)
      (http-404-not-found)))

;;; INTERNAL

(defun create-contact-request (data spam host)
  (syslog :info "Creating new contact request from ~A (spam: ~A)" host spam)
  (db-exec "INSERT INTO contact_requests (data, seen, spam, host, timestamp) VALUES (?, 0, ?, ?, ?);"
	   (list data spam host (get-universal-time))))

(defun get-contact-requests (offset limit spam)
  (when (and (typep offset 'integer)
	     (typep limit 'integer))
    (mapcar
     #'parse-contact-request
     (db-fetch "SELECT id, data, seen, timestamp FROM contact_requests WHERE spam = ?
                ORDER BY timestamp DESC LIMIT ?,?;"
	       (list (if spam 1 0) offset limit)))))

(defun parse-contact-request (x)
  (let ((x (plist-hash-table (encode-keys-to-strings x)
			     :test 'equalp)))
    (setf (gethash "data" x)
	  (yason:parse (gethash "data" x)))
    x))

(defun get-contact-request (id)
  (when-let ((db-data
	      (db-fetch-one "SELECT id, data, seen, timestamp FROM contact_requests WHERE id = ?;"
			    :args (list id))))
    (parse-contact-request db-data)))

(defun set-contact-request-seen (id seen)
  (not (zerop
	(db-exec "UPDATE contact_requests SET seen = ? WHERE id = ?"
		 (list (if seen 1 0)
		       id)))))

(defun process-contact-requests ()
  (log-errors
    (syslog :info "Processing unprocessed contact requests.")
    (process-contact-requests-to-gsheets)
    (process-contact-requests-to-mail)))

(defun process-contact-requests-to-gsheets ()
  (when *contact-request-spreadsheet-id*
    (syslog :info "Processing contact requests to GSheets")
    (let* ((unprocessed-reqs
	    (db-fetch "SELECT id, data, host, timestamp FROM contact_requests WHERE processed_spreadsheet = 0 AND spam = 0"))
	   (spreadsheet-data
	    (mapcar (lambda (x)
		      (let* ((x (parse-contact-request x))
			     (data (gethash "data" x)))
			(list (gethash "id" x)
			      (gethash "host" x)
			      (gethash "name" data)
			      (gethash "phone" data)
			      (gethash "email" data)
			      (to-gsheets-date (gethash "timestamp" x))
			      ""
			      (gethash "message" data)
						(gethash "voucher-type" data)
						(gethash "voucher-package" data)
						(gethash "makeup" data)
						(gethash "von" data)
						(gethash "fur" data)
						(gethash "delivery-address" data)
       			(gethash "category" data)
       			(gethash "language" data))))
		    unprocessed-reqs)))
      (when unprocessed-reqs
	(syslog :info "~D contact request(s) to sync to spreadsheets." (length unprocessed-reqs))
	(loop for s-data in spreadsheet-data
	      for id = (car s-data)
	      for host = (cadr s-data)
	      for data = (cddr s-data)
	      do (handler-case
		     (progn
		       (gsheets-append-contact-request host
						       data)
		       (gsheets-sort-contact-requests-by-date host)
		       (db-exec "UPDATE contact_requests SET processed_spreadsheet = 1 WHERE id = ?"
				(list id)))
		   (error (e)
		     (syslog :err "Could not process contact request ~A." id)
		     (syslog :err "~A" e))))))))

(defun process-contact-requests-to-mail ()
  (when *contact-request-notification-email*
    (syslog :info "Processing contact requests to email")
    (let* ((unprocessed-reqs
	    (db-fetch "SELECT id, data, host, timestamp FROM contact_requests 
                       WHERE processed_email = 0 
                       AND spam = 0")))
      (when unprocessed-reqs
	(syslog :info "~D contact request(s) to send mail for." (length unprocessed-reqs))
	(loop for req in unprocessed-reqs
	      for data = (gethash "data" (parse-contact-request req))
	      for to = *contact-request-notification-email*
	      for subject = (format nil "~A: ~A"
				    (host->sheet-name (getf req :|host|))
				    (gethash "name" data))
	      for message = (format nil "Site: ~A~%Name: ~A~%Phone: ~A~%Email: ~A~%Category: ~A~%Package: ~A~%Date: ~A~%Pregnancy Week: ~A~%Newborn Birth Date: ~A~%Makeup: ~A~%Message: ~A~%Voucher Type: ~A~%Voucher Package: ~A~%From: ~A~%To: ~A~%Delivery Address: ~A~%"
				    (getf req :|host|)
				    (gethash "name" data)
				    (gethash "phone" data)
				    (gethash "email" data)
            (gethash "category" data)
            (gethash "package" data)
            (gethash "date" data)
            (gethash "pregnancyWeek" data)
            (gethash "birthDate" data)
            (gethash "makeup" data)
				    (gethash "message" data)
				    (gethash "voucher-type" data)
				    (gethash "voucher-package" data)
				    (gethash "von" data)
				    (gethash "fur" data)
				    (gethash "delivery-address" data)
				    (gethash "language" data))
	      do (progn
		(send-mail to subject message)
		(db-exec "UPDATE contact_requests SET processed_email = 1 WHERE id = ?"
			 (list (getf req :|id|)))))))))
