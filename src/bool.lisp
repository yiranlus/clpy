(defpackage :clpy.bool
  (:nicknames :py.bool)
  (:use :cl)
  (:export #:new
           #:p))

(in-package :clpy.bool)

(clpy.type:define-type "PyBool_Type" bool)

(defun p (o)
  "Check if `O` is bool."
  (clpy.type:of o :bool))

(defun new (&optional (v nil))
  "Create a new bool from value ``V``."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-bool-from-long (if v 1 0))
    (error 'py.exc:generic-error)))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :bool (car x))))
		     #'(lambda (x) (apply #'new (cdr x))))
