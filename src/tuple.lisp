(defpackage :clpy.tuple
  (:nicknames :py.tuple)
  (:use :cl)
  (:export #:new
           #:pack
           #:size
           #:get-item
           #:get-slice
           #:set-item))

(in-package :clpy.tuple)

(declaim (ftype (function (integer)) new))
(defun new (len)
  (py:ensure-null-as-nil (clpy.ffi.fns:py-tuple-new len)))

(defmacro pack (&rest args)
  "Return a new tuple object contained in ARGS. Every element of ARGS should
be a py-object."
  `(py:ensure-null-as-nil
     (clpy.ffi.fns:py-tuple-pack
      ,(length args)
      ,@(loop for i in args
              collect :pointer
              collect (autowrap:ptr i)))))

(defun size (o)
  (clpy.ffi.fns:py-tuple-size py-tuple))

(defun get-item (p pos)
  (py:ensure-null-as-nil
    (clpy.ffi.fns:py-tuple-get-item p pos)))

(defun get-slice (p low high &key (as-tuple nil))
  (py:ensure-null-as-nil
    (clpy.ffi.fns:py-tuple-get-item p pos)))

(defun set-item (p pos o)
  "Return the object at position POS in the tuple pointed to by P."
  (zerop
   (clpy.ffi.fns:py-tuple-set-item p pos o)))
