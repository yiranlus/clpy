(cl:defpackage :clpy.bytes
  (:nicknames :py.bytes)
  (:use :cl :plus-c)
  (:export #:new
	   #:p
	   #:exact-p
	   #:as-string
	   #:concat
	   #:repr
	   #:len
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

(defun new (v &key escaped errors consumed)
  "Create a bytes object from ``V``.

``V`` can either be a string or a PyObject."
  
  (if (stringp v)
      (if escaped
	  (c-with ((consumed clpy.ffi:py-ssize-t))
		  (let ((res (clpy.util:ensure-null-as-nil
			      (clpy.ffi.fns:py-bytes-decode-escape v (length v) errors (consumed &))
			      (clpy.exception:raise-generic-or-python-error))))
		    (values res consumed)))
	  (clpy.util:ensure-null-as-nil
	   (clpy.ffi.fns:py-bytes-from-string v)
	   (clpy.exception:raise-generic-or-python-error)))
      (clpy.util:ensure-null-as-nil
       (clpy.ffi.fns:py-bytes-from-object v)
       (clpy.exception:raise-generic-or-python-error))))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :bytes (car x))))
		     #'(lambda (x) (apply #'new (cdr x))))

(defun as-string (o)
  (multiple-value-bind (res ptr)
      (clpy.ffi.fns:py-bytes-as-string o)
    (when (cffi:null-pointer-p ptr)
      (clpy.exception:raise-generic-or-python-error))
    res))

(defun repr (bytes i)
  "Compute a string representation of ``O``.

This generates a string similar to that returned by ``repr()''"
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-bytes-repr bytes i)
    (clpy.exception:raise-generic-or-python-error)))

(defun len (bytes)
  (clpy.ffi.fns:py-bytes-size bytes))

(defun size (bytes)
  (len bytes))

(defun concat (bytes newpart &key (delete t))
  (if delete
      (clpy.ffi.fns:py-bytes-concat-and-del (autowrap:ptr bytes) newpart)
      (clpy.ffi.fns:py-bytes-concat (autowrap:ptr bytes) newpart)))
