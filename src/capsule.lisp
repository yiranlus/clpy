(defpackage :clpy.capsule
  (:nicknames :py.cap)
  (:use :cl)
  (:shadow #:import)
  (:export #:define-destructor
           #:new
           #:set-pointer
           #:get-pointer
           #:set-destructor
           #:get-destructor
           #:set-context
           #:get-context
           #:set-name
           #:get-name
           #:import
           #:valid-p))

(in-package :clpy.capsule)

(clpy.type:define-type "PyCapsule_Type" capsule)

(defparameter *destructor-table* '())

(defmacro define-destructor (name (obj) &body body)
  `(progn
     (autowrap:defcallback ,name :void
       ((,obj clpy.ffi:py-object))
       ,@body)
     (push (cons ,name (autowrap:callback ,name)) ,*destructor-table*)))
      
(defun new (pointer name destructor)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-new pointer name
                                   (if (atom destructor)
                                       (autowrap:callback destructor)
                                       destructor))
    (error 'py.exc:generic-error)))

(defun get-pointer (o name)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-get-pointer o name)
    (error 'py.exc:generic-error)))

(defun get-destructor (o &optional as-symbol)
  (let ((f (clpy.util:ensure-null-as-nil
               (clpy.ffi.fns:py-capsule-get-destructor o))))
    (if (and as-symbol f)
        (rassoc f *destructor-table* :test #'cffi:pointer-eq)
        f)))

(defun get-context (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-get-context o)))

(defun get-name (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-get-name o)))

(defun import (name no-block)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-capsule-import name (if no-block 1 0))
    (error 'py.exc:generic-error)))

(defun valid-p (o name)
  (not (zerop
        (clpy.ffi.fns:py-capsule-is-valid o name))))

(defun set-context (o context)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-context o context)
    (error 'py.exc:generic-error)))

(defun set-destructor (o destructor)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-destructor
       o (if (atom destructor)
             (autowrap:callback destructor)
             destructor))
    (error 'py.exc:generic-error)))

(defun set-name (o name)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-name o name)
    (error 'py.exc:generic-error)))

(defun set-pointer (o pointer)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-capsule-set-pointer o pointer)
    (error 'py.exc:generic-error)))
