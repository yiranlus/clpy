(defpackage :clpy.byte-array
  (:nicknames :py.byte-array)
  (:use :cl)
  (:export #:p
           #:new
           #:concat
           #:size
           #:as-string
           #:resize))

(in-package :clpy.byte-array)

(clpy.type:define-type "PyByteArray_Type" byte-array)
(clpy.type:define-type "PyByteArrayIter_Type" byte-array-iter)

(defun p (o)
  (clpy.type:of o :byte-array))

(defun new (v)
  "Create a new bytearray object from V. V can be either a string or a PyObject which support buffer protocol."
  (py:ensure-null-as-nil
      (if (stringp v)
          (clpy.ffi.fns:py-byte-array-from-string-and-size v (length v))
          (clpy.ffi.fns:py-byte-array-from-object v))
    (error 'py.exc:generic-error)))

(defun concat (o1 o2)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-byte-array-concat o1 o2)
    (error 'py.exc:generic-error)))

(defun size (o)
  (clpy.ffi.fns:py-byte-array-size o))

(defun as-string (o)
  (clpy.ffi.fns:py-byte-array-as-string o))

(defun resize (o length)
  (clpy.ffi.fns:py-byte-array-resize o length))
