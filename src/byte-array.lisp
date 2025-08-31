(defpackage :clpy.byte-array
  (:nicknames :py.ba)
  (:use :cl)
  (:export #:p
           #:exact-p
           #:new
           #:concat
           #:size
           #:as-string
           #:resize))

(in-package :clpy.byte-array)

(clpy.type:define-type "PyByteArray_Type" byte-array)
(clpy.type:define-type "PyByteArrayIter_Type" byte-array-iter)

(defun p (o)
  (or (clpy.type:of o :byte-array)
      (clpy.type:subtype-p (clpy.object:ob-type o)
                           (clpy.type:get :byte-array))))

(defun exact-p (o)
  (clpy.type:of o :byte-array))

(defun new (v)
  "Create a new bytearray object from ``V``.

``V`` can be either a string or a PyObject which support buffer protocol."
  (clpy.util:ensure-null-as-nil
      (if (stringp v)
          (clpy.ffi.fns:py-byte-array-from-string-and-size v (length v))
          (clpy.ffi.fns:py-byte-array-from-object v))
    (error 'clpy.exception:generic-error)))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :byte-array (car x))))
                     #'(lambda (x) (apply #'new (cdr x))))

(defun concat (ba1 ba2)
  "Concatenate two byte arrays and return a new object."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-byte-array-concat ba1 ba2)
    (error 'clpy.exception:generic-error)))

(defun size (ba)
  "Return the size of the byte array."
  (clpy.ffi.fns:py-byte-array-size ba))

(defun as-string (ba)
  "Convert the byte array to a string."
  (clpy.ffi.fns:py-byte-array-as-string ba))

(defun resize (ba length)
  "Resize the byte array."
  (clpy.ffi.fns:py-byte-array-resize ba length))
