(defpackage :clpy.mapping
  (:nicknames :py.map)
  (:use :cl)
  (:shadow #:list #:count)
  (:export #:p))

(in-package :clpy.mapping)

(defun p (o)
  (plusp (clpy.ffi.fns:py-mapping-check o)))

(defun size (o)
  (py:ensure-non-negative
      (clpy.ffi.fns:py-mapping-size o)
    (error 'py.exc:generic-error)))

(defun get-item (o key)
  (py:ensure-null-as-nil
      (if (stringp key)
	  (clpy.ffi.fns:py-mapping-get-item-string o key)
	  (py.obj:set-item o key))
    (error 'py.exc:generic-error)))

(defun get-item (o key)
  (py:ensure-ze
