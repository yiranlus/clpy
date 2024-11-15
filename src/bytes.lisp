(cl:defpackage :clpy.bytes
  (:nicknames :py.bytes)
  (:use :cl)
  (:export #:new
	   #:as-string
	   #:concat
	   #:size))

(cl:in-package :clpy.bytes)

(clpy.type:define-type "PyBytes_Type" bytes)
(clpy.type:define-type "PyBytesIter_Type" bytes-iter)

(defun new (v)
  (clpy.util:ensure-null-as-nil
      (if (stringp v)
	  (clpy.ffi.fns:py-bytes-from-string v)
	  (clpy.ffi.fns:py-bytes-from-object v))
    (error 'py.exc:python-error)))

(defun as-string (o)
  (multiple-value-bind (res ptr)
      (clpy.ffi.fns:py-bytes-as-string o)
    (when (cffi:null-pointer-p ptr)
      (error 'py.exc:generic-error))
    res))

(defun size (o)
  (clpy.ffi.fns:py-bytes-size o))

(defun concat (bytes newpart &key (delete t))
  (if delete
      (clpy.ffi.fns:py-bytes-concat-and-del (autowrap:ptr bytes) newpart)
      (clpy.ffi.fns:py-bytes-concat (autowrap:ptr bytes) newpart)))
