(defpackage :clpy.sequence
  (:nicknames :py.seq)
  (:use :cl)
  (:shadow #:list #:count)
  (:export #:p
	   #:size
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

(defun size (o)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-sequence-length o)
    (error 'py.exc:generic-error)))

(defun concat (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-concat o1 o2)
    (error 'py.exc:generic-error)))

(defun concat= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-in-place-concat o1 o2)
    (error 'py.exc:generic-error)))

(defun repeat (o count)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-repeat o count)
    (error 'py.exc:generic-error)))

(defun repeat= (o count)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-in-place-repeat o count)
    (error 'py.exc:generic-error)))

(defun get-item (o i)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-get-item o i)
    (error 'py.exc:generic-error)))

(defun get-slice (o i1 i2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-get-slice o i1 i2)
    (error 'py.exc:generic-error)))

(defun set-item (o index value)
  (clpy.util:ensure-zero
      (clpy.pylet:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-set-item o index -value))
    (error 'py.exc:generic-error)))

(defun del-item (o index)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-sequence-del-item o index)
    (error 'py.exc:generic-error)))

(defun set-slice (o i1 i2 value)
  (clpy.util:ensure-zero
      (clpy.pylet:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-set-slice o i1 i2 -value))
    (error 'py.exc:generic-error)))

(defun del-slice (o i1 i2)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-sequence-del-slice o i1 i2)
    (error 'py.exc:generic-error)))

(defun count (o value)
  (clpy.util:ensure-non-negative
      (clpy.pylet:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-count o -value))
    (error 'py.exc:generic-error)))

(defun contains (o value)
  (case (clpy.pylet:let ((-value (clpy.smart:new value)))
	  (clpy.ffi.fns:py-sequence-contains o -value))
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:generic-error))))

(defun index (o value)
  (clpy.util:ensure-non-negative
      (clpy.pylet:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-sequence-index o -value))
    (error 'py.exc:generic-error)))

(defun in (o value)
  (case (clpy.pylet:let ((-value (clpy.smart:new value)))
	  (clpy.ffi.fns:py-sequence-contains o -value))
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:generic-error))))

(defun list (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-list o)
    (error 'py.exc:generic-error)))

(defun tuple (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-tuple o)
    (error 'py.exc:generic-error)))

;; iteratable object

(clpy.type:define-type "PySeqIter_Type" seq-iter)

(defun new-iter (seq)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-seq-iter-new seq)
    (error 'py.exc:generic-error)))

(clpy.type:define-type "PyCallIter_Type" call-iter)

(defun new-call-iter (callable sentinel)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-call-iter-new callable sentinel)
    (error 'py.exc:generic-error)))
 
