(defpackage :clpy.capsule
  (:nicknames :py.cap)
  (:use :cl)
  (:shadow #:import)
  (:export #:p
	   #:valid-p
	   #:defdestructor
	   #:new
	   #:get-name
	   #:set-name
	   #:get-pointer
	   #:set-pointer
	   #:get-destructor
	   #:set-destructor
	   #:get-context
	   #:set-context))

(in-package :clpy.capsule)

(clpy.type:define-type "PyCapsule_Type" capsule)

(defmacro defdestructor (name lambda-list &body body)
  (when (not (= 1 (length lambda-list)))
    (error "The lambda-list should have exactly one argument."))
  `(autowrap:defcallback ,name :void ((,(first lambda-list) clpy.ffi:py-object))
     ,@body))

(defun p (o)
  "Check if the object is a capsule.

This is same as exact check."
  (clpy.type:of o :capsule))


(defun valid-p (capsule name)
  (plusp (clpy.ffi.fns:py-capsule-is-valid capsule name)))


(defun import (name)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-import name)
    (clpy.exception:raise-generic-or-python-error
     :message "Failed to import from the capsule.")))


(defun new (pointer name destructor)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-new pointer name destructor)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to create the capsule.")))

(defun get-pointer (capsule name)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-get-pointer capsule name)
    (clpy.exception:raise-generic-or-python-error)))

(defun set-pointer (capsule pointer)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-pointer capsule pointer)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-name (capsule)
  (multiple-value-bind (res ptr)
      (clpy.ffi.fns:py-bytes-as-string capsule)
    (when (cffi:null-pointer-p ptr)
      (clpy.exception:return-or-raise-python-error nil))
    res))

(defun set-name (capsule name)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-name capsule name)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-context (capsule)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-get-context capsule)
    (clpy.exception:return-or-raise-python-error nil)))

(defun set-context (capsule context)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-context capsule context)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-destructor (capsule)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-get-destructor capsule)
    (clpy.exception:return-or-raise-python-error nil)))

(defun set-destructor (capsule destructor)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-destructor capsule destructor)
    (clpy.exception:raise-generic-or-python-error)))
