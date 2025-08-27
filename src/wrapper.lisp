(defpackage :clpy.wrapper
  (:nicknames :py.wrap)
  (:use :cl)
  (:export #:new))

(in-package :clpy.wrapper)

(defun new (descr wrapped)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-wrapper-new descr wrapped)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to create the wrapper.")))
