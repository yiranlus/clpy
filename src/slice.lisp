(defpackage :clpy.slice
  (:nicknames :py.slice)
  (:use :cl :plus-c)
  (:shadow #:step)
  (:export #:p
           #:new
           #:unpack
           #:adjust))

(in-package :clpy.slice)

(clpy.type:define-type "PySlice_Type" slice)

(defun p (o)
  (clpy.type:of o :slice))

(defun new (start stop step)
  (clpy.util:ensure-null-as-nil
      (py:let ((-start (py:new start))
               (-stop (py:new stop))
               (-step (py:new step)))
        (clpy.ffi.fns:py-slice-new -start -stop -step))
    (error 'py.exc:generic-error)))

(defun unpack (o)
  "Return the start, the stop and the step of a slice object. The returned values can be pass to ADJUST."
  (c-let ((start clpy.ffi:py-ssize-t)
           (stop clpy.ffi:py-ssize-t)
           (step clpy.ffi:py-ssize-t))
    (clpy.util:ensure-zero
        (clpy.ffi.fns:py-slice-unpack o (start &) (stop &) (step &))
      (error 'py.exc:generic-error))
    (values start stop step)))

(defun adjust (len start stop step)
  "Adjust the start and stop of the slices assuming the length is LEN. START, STOP and STEP can be generated from UNPACK."
  (c-let ((-start clpy.ffi:py-ssize-t :from start)
          (-stop clpy.ffi:py-ssize-t :from stop))
    (clpy.ffi.fns:py-slice-adjust-indices len (-start &) (-stop &) step)))

;;; Ellipsis

(cl:defpackage :clpy.ellipsis
  (:nicknames :py.ell)
  (:use :cl)
  (:export #:new))

(cl:in-package :clpy.ellipsis)

(clpy.type:define-type "PyEllipsis_Type" ellipsis)

(defun new ()
  (new-ref (plus-c:c-ref
	    (cffi:foreign-symbol-pointer "_Py_EllipsisObject")
	    clpy.ffi:py-object)))

(clpy.smart:new-hook #'(lambda (x) (eq x :...))
		     #'(lambda (x) (new)))
