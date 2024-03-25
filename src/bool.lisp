(defpackage :clpy.bool
  (:nicknames :py.bool)
  (:use :cl)
  (:export #:+type+
           #:from-long))

(in-package :clpy.bool)

(cffi:defcvar "PyBool_Type" :int)
(defconstant +type+ (cffi:get-var-pointer '*pybool-type*))

(defun from-long (v)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-bool-from-long v)))
