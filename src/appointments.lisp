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
			    (assert-type (gethash "startTime" (@json-body)) 'fixnum)
			    (assert-type (gethash "endTime" (@json-body)) 'fixnum)
			    (assert-type (gethash "price" (@json-body)) 'real)
			    (assert-type (gethash "currency" (@json-body)) 'string)
			    (assert-type (gethash "photographer" (@json-body)) 'string)
			    (assert-type (gethash "photoshootType" (@json-body)) 'string)
			    (assert-type (gethash "photoshootPackage" (@json-body)) 'string))
	(http-204-no-content))
    (type-error () (http-400-bad-request))))


;;; INTERNAL

(defun create-appointment (host customer-name start-time end-time price currency photographer
			   photoshoot-type photoshoot-package)
  (syslog :info "Creating new appointment from ~A" host)
  (db-exec "INSERT INTO appointments (host, customer_name, start_time, end_time, price, currency, photographer,
                                      photoshoot_type, photoshoot_package) 
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);"
	   (list host customer-name start-time end-time price currency photographer
		 photoshoot-type photoshoot-package)))
