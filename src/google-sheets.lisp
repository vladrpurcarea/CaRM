;;;; google-sheets.lisp

(in-package #:carm)

(defconstant +sheets-base-uri+ "https://sheets.googleapis.com/v4/spreadsheets")

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

(defun gsheets-append-contact-requests (data)
  (gsheets-append *contact-request-spreadsheet-id*
		  data
		  :range (concatenate 'string
				      *contact-request-sheet-name*
				      "!A1:G1")))

(defun gsheets-sort-contact-requests-by-date ()
  (let ((request
	  (alist-hash-table
	   `(("sortRange"
	      . ,(alist-hash-table
		  `(("range"
		     . ,(alist-hash-table
			 `(("sheetId" . ,*contact-request-sheet-id*)
			   ("startRowIndex" . 1)
			   ("startColumnIndex" . 0)
			   ("endColumnIndex" . 6))))
		    ("sortSpecs"
		     . (,(alist-hash-table
			  `(("dimensionIndex" . 3)
			    ("sortOrder" . "DESCENDING"))))))))))))
    (gsheets-batch-update *contact-request-spreadsheet-id* (list request))))

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
