(cl:defpackage :clpy.sequence
  (:nicknames :py.seq)
  (:use :cl)
  (:shadow #:list #:count)
  (:export #:p))

(cl:in-package :clpy.sequence)

;; sequence protocol

(defun p (o)
  (plusp (clpy.ffi.fns:py-sequence-check)))

(defun size (o)
  (py:ensure-non-negative
      (clpy.ffi.fns:py-sequence-length o)
    (error 'py.exc:generic-error)))

(defun concat (o1 o2)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-concat o1 o2)
    (error 'py.exc:generic-error)))

(defun concat= (o1 o2)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-in-place-concat o1 o2)
    (error 'py.exc:generic-error)))

(defun repeat (o count)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-repeat o count)
    (error 'py.exc:generic-error)))

(defun repeat= (o count)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-in-place-repeat o count)
    (error 'py.exc:generic-error)))

(defun get-item (o i)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-get-item o i)
    (error 'py.exc:generic-error)))

(defun get-slice (o i1 i2)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-get-slice o i1 i2)
    (error 'py.exc:generic-error)))

(defun set-item (o i v)
  (py:ensure-zero
      (clpy.ffi.fns:py-sequence-set-item o i v)
    (error 'py.exc:generic-error)))

(defun del-item (o i)
  (py:ensure-zero
      (clpy.ffi.fns:py-sequence-del-item o i)
    (error 'py.exc:generic-error)))

(defun set-slice (o i1 i2 v)
  (py:ensure-zero
      (clpy.ffi.fns:py-sequence-set-slice o i1 i2 v)
    (error 'py.exc:generic-error)))

(defun del-slice (o i1 i2)
  (py:ensure-zero
      (clpy.ffi.fns:py-sequence-del-slice o i1 i2)
    (error 'py.exc:generic-error)))

(defun count (o value)
  (py:ensure-non-negative
      (clpy.ffi.fns:py-sequence-count o value)
    (error 'py.exc:generic-error)))

(defun contains (o value)
  (case (clpy.ffi.fns:py-sequence-contains o value)
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:generic-error))))

(defun index (o value)
  (py:ensure-non-negative
      (clpy.ffi.fns:py-sequence-index o value)
    (error 'py.exc:generic-error)))

(defun in (o value)
  (case (clpy.ffi.fns:py-sequence-in o value)
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:generic-error))))

(defun list (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-sequence-list o)
    (error 'py.exc:generic-error)))

;; iteratable object

(clpy.type:define-type "PySeqIter_Type" seq-iter)

(defun new-iter (seq)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-seq-iter-new seq)
    (error 'py.exc:generic-error)))

(clpy.type:define-type "PyCallIter_Type" call-iter)

(defun new-call-iter (callable sentinel)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-call-iter-new callable sentinel)
    (error 'py.exc:generic-error)))
 
