;;;; google-sheets.lisp

(in-package #:carm)

(defvar *contact-request-sheet-ids*)
(defvar +sheets-base-uri+ "https://sheets.googleapis.com/v4/spreadsheets")

(defun setup-gsheets ()
  (syslog :info "Getting Google Spreadsheet information about sheets")
  (let ((spreadsheet (gsheets-get-spreadsheet *contact-request-spreadsheet-id*)))
    (setf *contact-request-sheet-ids* (make-hash-table :test 'equal))
    (loop for sheet in (gethash "sheets" spreadsheet)
	  for props = (gethash "properties" sheet)
	  do (let ((title (gethash "title" props))
		   (sheet-id (gethash "sheetId" props)))
	       (setf (gethash title *contact-request-sheet-ids*)
		     sheet-id)
	       (syslog :info "Sheet ~A: ~A" title sheet-id)))))

(defun gsheets-get-spreadsheet (spreadsheet-id)
  (from-json
   (auth-google-req (base-spreadsheet-url spreadsheet-id)
		    :method :GET)))

(defun base-spreadsheet-url (spreadsheet-id)
  (format nil "~A/~A" +sheets-base-uri+ spreadsheet-id))

(defun gsheets-append (spreadsheet-id data &key (range "Sheet1!A1:G1"))
  (let ((url (format nil "~A/values/~A:append"
			   (base-spreadsheet-url spreadsheet-id)
			   range))
	(request
	  (to-json
	   `(("range" . ,range)
	     ("majorDimension" . "ROWS")
	     ("values" . ,data)))))
    (from-json
     (auth-google-req url
		      :method :POST
		      :content request
		      :parameters '(("valueInputOption" . "USER_ENTERED"))))))

(defun gsheets-batch-update (spreadsheet-id batch-requests)
  (let ((url
	  (format nil "~A:batchUpdate"
		  (base-spreadsheet-url spreadsheet-id)))
	(request
	  (to-json
	   `(("requests" . ,batch-requests)))))
    (from-json
     (auth-google-req url
		      :method :POST
		      :content request))))

(defun gsheets-append-contact-request (host data)
  (gsheets-append *contact-request-spreadsheet-id*
		  (list data)
		  :range (concatenate 'string
				      (host->sheet-name host)
				      "!A1:G1")))

(defun gsheets-sort-contact-requests-by-date (host)
  (let ((request
	  (alist-hash-table
	   `(("sortRange"
	      . ,(alist-hash-table
		  `(("range"
		     . ,(alist-hash-table
			 `(("sheetId" . ,(host->sheet-id host))
			   ("startRowIndex" . 1)
			   ("startColumnIndex" . 0)
			   ("endColumnIndex" . 9))))
		    ("sortSpecs"
		     . (,(alist-hash-table
			  `(("dimensionIndex" . 3)
			    ("sortOrder" . "DESCENDING"))))))))))))
    (gsheets-batch-update *contact-request-spreadsheet-id* (list request))))

(defun host->sheet-name (host)
  (gethash host
	   *contact-request-sheet-names*))
(defun host->sheet-id (host)
  (gethash (host->sheet-name host)
	   *contact-request-sheet-ids*))

(defun to-gsheets-date (timestamp)
  (multiple-value-bind
	(second minute hour date month year)
      (decode-universal-time timestamp)
    (format nil "=DATE(~D,~D,~D)+TIME(~D,~D,~D)"
	    year
	    month
	    date
	    hour
	    minute
	    second)))
