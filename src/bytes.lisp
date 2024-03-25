(defpackage :clpy.bytes
  (:nicknames :py.bytes)
  (:use :cl))

(in-package :clpy.bytes)

(cffi:defcvar "PyBytes_Type" :int)
(defconstant +type+ (cffi:get-var-pointer '*pybytes-type*))

(defun from-string (v)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-bytes-from-string v)
    (error 'py.exc:python-error)))

(defun as-string (o)
  (multiple-value-bind (res ptr)
      (clpy.ffi.fns:py-bytes-as-string o)
    (when (cffi:null-pointer-p ptr)
      (error 'py.exc:type-error))
    res))

(defun from-object (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-bytes-from-object o)))

(defun concat (bytes newpart &key (del t))
  (if del
      (clpy.ffi.fns:py-bytes-concat-and-del (autowrap:ptr bytes) newpart)
      (clpy.ffi.fns:py-bytes-concat (autowrap:ptr bytes) newpart)))
