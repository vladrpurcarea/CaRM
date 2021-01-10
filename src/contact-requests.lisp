;;;; contact-requests.lisp

(in-package #:carm)

;;; WEB

(defroute post-contact-request
    ("/carm/api/v1/contact-request"
     :method :POST
     :decorators (@json))
    ()
  (labels ((forbidden-fields-p ()
	     (loop for k being the hash-keys of (@json-body)
		   when (member k *cr-forbidden-fields* :test #'string-equal)
		     return t))
	   (requests-field-p ()
	     (loop for f in *cr-required-fields*
		   when (not (gethash f (@json-body)))
		     return nil
		   finally (return t))))
    (cond
      ((forbidden-fields-p) (http-204-no-content))
      ((not (requests-field-p))
       (http-400-bad-request (to-json `(("error" . "Required fields missing")))))
      ((create-contact-request (to-json (@json-body)))
       (http-204-no-content)))))

(defroute get-contact-requests-route
    ("/carm/api/v1/contact-request"
     :method :GET
     :decorators (@auth @json-out))
    (&get offset limit)
  (labels ((parse (val default)
	     (if val
		 (parse-integer val)
		 default)))
    (to-json
     (alist-hash-table
      `(("contactRequests"
	 . ,(get-contact-requests (parse offset 0)
				  (parse limit 100))))))))

(defroute get-contact-request-route
    ("/carm/api/v1/contact-request/:id"
     :method :GET
     :decorators (@auth @json-out))
    ()
  (if-let ((contact-request (get-contact-request id)))
    (to-json contact-request)
    (http-404-not-found)))


(defroute put-contact-request-seen-route
    ("/carm/api/v1/contact-request/:id/seen"
     :method :PUT
     :decorators (@auth))
    ()
  (if (set-contact-request-seen id t)
      (http-204-no-content)
      (http-404-not-found)))



(defroute delete-contact-request-seen-route
    ("/carm/api/v1/contact-request/:id/seen"
     :method :DELETE
     :decorators (@auth))
    ()
  (if (set-contact-request-seen id nil)
      (http-204-no-content)
      (http-404-not-found)))

;;; INTERNAL

(defun create-contact-request (data)
  (db-exec "INSERT INTO contact_requests (data, seen, spam, timestamp) VALUES (?, 0, 0, ?);"
	   (list data (get-universal-time))))

(defun get-contact-requests (offset limit)
  (when (and (typep offset 'integer)
	     (typep limit 'integer))
    (mapcar
     #'parse-contact-request
     (db-fetch
      (format
       nil
       "SELECT id, data, seen, timestamp FROM contact_requests WHERE spam = 0
        ORDER BY timestamp DESC LIMIT ~A,~A;"
       offset limit)))))

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
  (process-contact-requests-to-gsheets)
  (process-contact-requests-to-mail))

(defun process-contact-requests-to-gsheets ()
  (let* ((unprocessed-reqs
	   (db-fetch "SELECT id, data, timestamp FROM contact_requests WHERE processed_spreadsheet = 0"))
	 (spreadsheet-data
	  (mapcar (lambda (x)
		    (let* ((x (parse-contact-request x))
			   (data (gethash "data" x)))
		      (list (gethash "name" data)
			    (gethash "phone" data)
			    (gethash "email" data)
			    (to-gsheets-date (gethash "timestamp" x))
			    ""
			    (gethash "message" data))))
		  unprocessed-reqs)))
    (when unprocessed-reqs
      (gsheets-append-contact-requests spreadsheet-data)
      (gsheets-sort-contact-requests-by-date)
      (loop for req in unprocessed-reqs
	    do (db-exec "UPDATE contact_requests SET processed_spreadsheet = 1 WHERE id = ?"
			(list (getf req :|id|)))))))

(defun process-contact-requests-to-mail ()
  (let* ((unprocessed-reqs
	   (db-fetch "SELECT id, data, timestamp FROM contact_requests WHERE processed_email = 0")))
    (when unprocessed-reqs
      (loop for req in unprocessed-reqs
	    for data = (gethash "data" (parse-contact-request req))
	    for to = *contact-request-notification-email*
	    for subject = (format nil "Booking request: ~A" (gethash "name" data))
	    for message = (format nil "Name: ~A~%Phone: ~A~%Email: ~A~%Message: ~A~%"
				  (gethash "name" data)
				  (gethash "phone" data)
				  (gethash "email" data)
				  (gethash "message" data))
	    do (progn
		 (send-mail to subject message)
		 (db-exec "UPDATE contact_requests SET processed_email = 1 WHERE id = ?"
			  (list (getf req :|id|))))))))
