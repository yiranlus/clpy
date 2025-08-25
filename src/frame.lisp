(defpackage :clpy.frame
  (:nicknames :py.frame)
  (:use :cl)
  (:export #:p
	   #:get-code
	   #:get-line-number))

(in-package :clpy.frame)

(defun get-code (frame)
  "Return the FRAME code."
  (clpy.ffi.fns:py-frame-get-code frame))

(defun get-line-number (frame)
  "Return the line number that FRAME is currently executing."
  (clpy.ffi.fns:py-frame-get-line-number frame))
