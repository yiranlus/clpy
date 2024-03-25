(defpackage :clpy.complex
  (:nicknames :py.complex)
  (:use :cl)
  (:export #:+type+
           #:from
           #:real
           #:imag))

(in-package :clpy.complex)

(cffi:defcvar "PyComplex_Type" :int)
(defconstant +type+ (cffi:get-var-pointer '*pycomplex-type*))

(defun from (real imag)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-complex-from-doubles real imag)
    (error 'py.exc:python-error)))

(defun real (op)
  (clpy.ffi.fns:py-complex-real-as-double op))

(defun imag (op)
  (clpy.ffi.fns:py-complex-imag-as-double op))
