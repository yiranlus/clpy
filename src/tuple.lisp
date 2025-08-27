(defpackage :clpy.tuple
  (:nicknames :py.tuple)
  (:use :cl)
  (:export #:new-of
           #:new
           #:p
           #:len
           #:get-item
           #:get-slice
           #:set-item))

(in-package :clpy.tuple)

(clpy.type:define-type "PyTuple_Type" tuple)
(clpy.type:define-type "PyTupleIter_Type" tuple-iter)

(defun p (o)
  (or (clpy.type:of o :tuple)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :tuple))))

(defun exact-p (o)
  (clpy.type:of o :tuple))

(defun new-of (n)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-tuple-new n)
    (error 'py.exc:generic-error)))

(defun new (&rest items)
  (let ((l (new-of (length items))))
    (loop for i in items
	  for index from 0
	  do (let ((py-i (clpy.smart:new i)))
	       (set-item l index py-i)))
    l))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :tuple (car x))))
		     #'(lambda (x) (apply #'new (cdr x))))

(defun len (o)
  (clpy.ffi.fns:py-tuple-size o))

(defun get-item (o index)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-tuple-get-item o index)
    (error 'py.exc:generic-error)))

(defun set-item (o index value)
  (clpy.util:ensure-zero
      (let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-tuple-set-item o index -value))
    (error 'py.exc:generic-error)))

(defun get-slice (o low high)
  (clpy.ffi.fns:py-tuple-get-slice o low high))
