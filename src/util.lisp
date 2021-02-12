;;;; util.lisp

(in-package #:carm)

(defmacro assert-type (x type)
  `(if (typep ,x ,type)
       ,x
       (error (make-condition 'type-error
			      :datum ,x
			      :expected-type ,type))))
