(defpackage :clpy.weakref
  (:nicknames :py.weakref)
  (:use :cl)
  (:export #:new-ref
           #:new-proxy
           #:get-object))

(in-package :clpy.weakref)

(defun new-ref (obj callback)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-weakref-new-ref obj callback)
   (clpy.exception:raise-generic-or-python-error)))

(defun new-proxy (obj callback)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-weakref-new-proxy obj callback)
   (clpy.exception:raise-generic-or-python-error)))

(defun get-object (obj)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-weakref-get-object obj)
   (clpy.exception:raise-generic-or-python-error)))
