(defpackage :clpy.byte-array
  (:nicknames :py.byte-array)
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

(defun concat (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-byte-array-concat o1 o2)
    (error 'clpy.exception:generic-error)))

(defun size (o)
  (clpy.ffi.fns:py-byte-array-size o))

(defun as-string (o)
  (clpy.ffi.fns:py-byte-array-as-string o))

(defun resize (o length)
  (clpy.ffi.fns:py-byte-array-resize o length))
