(cl:defpackage :clpy.bool
  (:nicknames :py.bool)
  (:use :cl)
  (:export #:new))

(cl:in-package :clpy.bool)

(clpy.type:define-type "PyBool_Type" bool)

(defun new (&optional (v nil))
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-bool-from-long (if v 1 0))
    (error 'py.exc:generic-error)))
