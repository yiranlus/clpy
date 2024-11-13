(defpackage :clpy.mapping
  (:nicknames :py.map)
  (:use :cl)
  (:shadow #:list #:count)
  (:export #:p
	   #:size
	   #:get-item
	   #:set-item
	   #:del-tem
	   #:has-key
	   #:keys
	   #:values
	   #:items))

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
	  (py.obj:get-item o key))
    (error 'py.exc:generic-error)))

(defun set-item (o key value)
  (py:ensure-zero
      (py:let ((-value (py:new value)))
	(if (stringp key)
	    (clpy.ffi.fns:py-mapping-set-item-string o key -value)
	    (py.obj:set-item o key -value)))
    (error 'py.exc:generic-error)))

(defun del-item (o key)
  (py:ensure-zero
      (if (stringp key)
	  (clpy.ffi.fns:py-mapping-del-item-string o key)
	  (py.obj:del-item o key))
    (error 'py.exc:generic-error)))

(defun has-key (o key)
  (plusp
   (if (stringp key)
       (clpy.ffi.fns:py-mapping-has-key-string o key)
       (py:let ((-key (py:new key)))
	 (clpy.ffi.fns:py-mapping-has-key o -key)))))

(defun keys (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-mapping-keys o)
    (error 'py.exc:generic-error)))

(defun values (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-mapping-values o)
    (error 'py.exc:generic-error)))

(defun items (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-mapping-items o)
    (error 'py.exc:generic-error)))
