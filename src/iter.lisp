(defpackage :clpy.iter
  (:nicknames :py.iter)
  (:use :cl :plus-c)
  (:export #:p
           #:async-p
           #:next
           #:send))

(in-package :clpy.iter)

(defun p (o)
  (not (zerop
        (clpy.ffi.fns:py-iter-check o))))

(defun async-p (o)
  (not (zerop
        (clpy.ffi.fns:py-a-iter-check o))))

(defun next (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-iter-next o)
    (when (py:error-occurred)
      (error 'py.exc:python-error))))

(defun send (o arg)
  (c-with ((presult (:pointer clpy.ffi:py-object)))
    (let ((res (clpy.ffi.fns:py-iter-send o arg (presult &))))
      (match res
        (clpy.ffi:+pygen-return+ (values res presult))
        (clpy.ffi:+pygen-next+ (values res presult))
        (clpy.ffi:+pygen-error+ nil)))))
