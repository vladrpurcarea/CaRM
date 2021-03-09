;;;; google-calendar.lisp

(in-package #:carm)

(defvar +calendar-base-uri+ "https://www.googleapis.com/calendar/v3")

(defun make-calendar-url (&rest url-parts)
  (apply #'concatenate 'string
	 `(,+calendar-base-uri+ ,@url-parts)))

(defun gcalendar-list-calendars ()
  (from-json
   (auth-google-req (make-calendar-url "/users/me/calendarList")
		    :method :GET)))

(defun gcalendar-accept-calendar (calendar-id)
  (from-json
   (auth-google-req (make-calendar-url "/users/me/calendarList")
		    :method :POST
		    :content (to-json `(("id" . ,calendar-id))))))

(defun gcalendar-insert-event (start end summary &key
						   (calendar-id *appointment-calendar-id*)
						   (description "")
						   (color nil))
  (from-json
   (auth-google-req (make-calendar-url "/calendars/" calendar-id "/events")
		    :method :POST
		    :content (to-json
			      (alist-hash-table
			       `(("start" . ,(universal-time->gcalendar start))
				 ("end" . ,(universal-time->gcalendar end))
				 ("summary" . ,summary)
				 ("description" . ,description)
				 ("color" . ,color)))))))

(defun universal-time->gcalendar (utime)
  (alist-hash-table
   `(("dateTime" . ,(universal-time->rfc3339 utime)))))