(defpackage :clpy.sequence
  (:nicknames :py.seq)
  (:use :cl)
  (:shadow #:list #:count #:length)
  (:export #:p
	   #:length
	   #:concat #:concat=
	   #:repeat #:repeat=
	   #:get-item #:get-slice
	   #:set-item #:set-slice
	   #:del-item #:del-slice
	   #:count #:contains #:index #:in
	   #:list #:tuple))

(in-package :clpy.sequence)

;; sequence protocol

(defun p (o)
  (plusp (clpy.ffi.fns:py-sequence-check o)))

(defun length (o)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-sequence-length o)
    (clpy.exception:raise-generic-or-python-error)))

(defun concat (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-concat o1 o2)
    (clpy.exception:raise-generic-or-python-error)))

(defun concat= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-in-place-concat o1 o2)
    (clpy.exception:raise-generic-or-python-error)))

(defun repeat (o count)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-repeat o count)
    (clpy.exception:raise-generic-or-python-error)))

(defun repeat= (o count)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-in-place-repeat o count)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-item (o i)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-get-item o i)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-slice (o i1 i2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-get-slice o i1 i2)
    (clpy.exception:raise-generic-or-python-error)))

(defun set-item (o index value)
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-set-item o index -value))
    (clpy.exception:raise-generic-or-python-error)))

(defun del-item (o index)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-sequence-del-item o index)
    (clpy.exception:raise-generic-or-python-error)))

(defun set-slice (o i1 i2 value)
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-set-slice o i1 i2 -value))
    (clpy.exception:raise-generic-or-python-error)))

(defun del-slice (o i1 i2)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-sequence-del-slice o i1 i2)
    (clpy.exception:raise-generic-or-python-error)))

(defun count (o value)
  (clpy.util:ensure-non-negative
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-count o -value))
    (clpy.exception:raise-generic-or-python-error)))

(defun contains (o value)
  (case (clpy.util:let ((-value (clpy.smart:new value)))
	  (clpy.ffi.fns:py-sequence-contains o -value))
    (1 t)
    (0 nil)
    (-1 (clpy.exception:raise-generic-or-python-error))))

(defun index (o value)
  (clpy.util:ensure-non-negative
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-index o -value))
    (clpy.exception:raise-generic-or-python-error)))

(defun in (o value)
  (case (clpy.util:let ((-value (clpy.smart:new value)))
	  (clpy.ffi.fns:py-sequence-contains o -value))
    (1 t)
    (0 nil)
    (-1 (clpy.exception:raise-generic-or-python-error))))

(defun list (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-list o)
    (clpy.exception:raise-generic-or-python-error)))

(defun tuple (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-tuple o)
    (clpy.exception:raise-generic-or-python-error)))

;; iteratable object

(defun new-iter (seq)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-seq-iter-new seq)
    (clpy.exception:raise-generic-or-python-error)))

;; (clpy.type:define-type "PyCallIter_Type" call-iter)

;; (defun new-call-iter (callable sentinel)
;;   (clpy.util:ensure-null-as-nil
;;       (clpy.ffi.fns:py-call-iter-new callable sentinel)
;;     (clpy.exception:raise-generic-or-python-error)))
 
