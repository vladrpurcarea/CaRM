;;;; contact-requests.lisp

(in-package #:carm)

;;; WEB

(defroute post-contact-request ("/carm/api/v1/contact-request" :method :POST
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

(defroute get-contact-request-route ("/carm/api/v1/contact-request" :method :GET
								    :decorators (@auth @json-out))
    (&get offset limit)
  (labels ((parse (val default)
	     (if val
		 (parse-integer val)
		 default)))
    (to-json (alist-hash-table `(("contactRequests"
				  . ,(get-contact-requests (parse offset 0)
							   (parse limit 100))))))))

;;; INTERNAL

(defun create-contact-request (data)
  (db-exec "INSERT INTO contact_requests (data, seen, spam, timestamp) VALUES (?, 0, 0, ?);"
	   (list data (get-universal-time))))

(defun get-contact-requests (offset limit)
  (when (and (typep offset 'integer)
	     (typep limit 'integer))
    (mapcar
     (lambda (x)
       (let ((x (plist-hash-table (encode-keys-to-strings x)
				  :test 'equalp)))
	 (setf (gethash "data" x)
	       (yason:parse (gethash "data" x)))
	 x))
     (db-fetch
      (format
       nil
       "SELECT id, data, seen, timestamp FROM contact_requests WHERE spam = 0
        ORDER BY timestamp DESC LIMIT ~A,~A;"
       offset limit)))))
