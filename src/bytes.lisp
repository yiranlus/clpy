(cl:defpackage :clpy.bytes
  (:nicknames :py.bytes)
  (:use :cl)
  (:export #:new
	   #:p
	   #:exact-p
	   #:as-string
	   #:concat
	   #:repr
	   #:size))

(cl:in-package :clpy.bytes)

(clpy.type:define-type "PyBytes_Type" bytes)
(clpy.type:define-type "PyBytesIter_Type" bytes-iter)

(defun p (o)
  (or (clpy.type:of o :bytes)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :bytes))))

(defun exact-p (o)
  (clpy.type:of o :bytes))

(defun new (v)
  "Create a bytes object from ``V``.

``V`` can either be a string or a PyObject."
  (clpy.util:ensure-null-as-nil
      (if (stringp v)
	  (clpy.ffi.fns:py-bytes-from-string v)
	  (clpy.ffi.fns:py-bytes-from-object v))
    (error 'clpy.exception:generic-error)))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :bytes (car x))))
		     #'(lambda (x) (apply #'new (cdr x))))

(defun as-string (o)
  (multiple-value-bind (res ptr)
      (clpy.ffi.fns:py-bytes-as-string o)
    (when (cffi:null-pointer-p ptr)
      (clpy.exception:raise-generic-or-python-error))
    res))

(defun repr (o i)
  "Compute a string representation of ``O``.

This generates a string similar to that returned by ``repr()''"
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-bytes-repr o i)
    (clpy.exception:raise-generic-or-python-error)))

(defun size (o)
  (clpy.ffi.fns:py-bytes-size o))

(defun concat (bytes newpart &key (delete t))
  (if delete
      (clpy.ffi.fns:py-bytes-concat-and-del (autowrap:ptr bytes) newpart)
      (clpy.ffi.fns:py-bytes-concat (autowrap:ptr bytes) newpart)))
