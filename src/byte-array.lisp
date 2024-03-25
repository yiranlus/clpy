(defpackage :clpy.byte-array
  (:nicknames :py.byte-array)
  (:use :cl)
  (:export #:+type+
           #:from-object
           #:from-string
           #:concat
           #:size
           #:as-string
           #:resize))

(in-package :clpy.byte-array)

(cffi:defcvar "PyByteArray_Type" :int)
(defconstant +type+ (cffi:get-var-pointer '*pybytearray-type*))

(defun from-object (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-byte-array-from-object o)))

(defun from-string (str)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-byte-array-from-string-and-size str (length str))))

(defun concat (a b)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-byte-array-concat a b)))

(defun size (byte-array)
  (clpy.ffi.fns:py-byte-array-size byte-array))

(defun as-string (byte-array)
  (clpy.ffi.fns:py-byte-array-as-string byte-array))

(defun resize (byte-array len)
  (clpy.ffi.fns:py-byte-array-resize byte-array len))
