(defpackage :clpy.system
  (:nicknames :py.sys)
  (:use :cl)
  (:export #:get-object
	   #:set-object
	   #:reset-warn-options
	   #:has-warn-options
	   #:get-x-option))

(in-package :clpy.system)

(defun get-object (name)
  "Get the object named in ``sys`` module."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sys-get-object name)))

(defun set-object (name v)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-sys-set-object name v)
    (clpy.exception:raise-generic-or-python-error
     :message (format nil "Unable to set sys.~A to the value." name))))

(defun reset-warn-options ()
  (clpy.ffi.fns:py-sys-reset-warn-options))

(defun has-warn-options ()
  (plusp
   (clpy.ffi.fns:py-sys-has-warn-options)))

(defun get-x-option ()
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-sys-get-x-options)
    (clpy.exception:raise-generic-or-python-error)))
  
