(defpackage :clpy.mapping
  (:nicknames :py.map)
  (:use :cl)
  (:shadow #:values)
  (:export #:p
           #:size
           #:len
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

;; (defun size (o)
;;   (clpy.util:ensure-non-negative
;;       (clpy.ffi.fns:py-mapping-size o)
;;     (clpy.exception:raise-generic-or-python-error)))

(defun len (o)
  (clpy.util:ensure-non-negative
   (clpy.ffi.fns:py-mapping-length o)
   (clpy.exception:raise-generic-or-python-error)))

(defun get-item (o key)
  (clpy.util:ensure-null-as-nil
   (if (stringp key)
       (clpy.ffi.fns:py-mapping-get-item-string o key)
       (clpy.util:let ((-key (clpy.smart:new key)))
         (clpy.object:get-item o -key)))
   (clpy.exception:raise-generic-or-python-error)))

(defun set-item (o key value)
  (clpy.util:ensure-zero
   (if (stringp key)
       (clpy.ffi.fns:py-mapping-set-item-string o key value)
       (clpy.util:let ((-key (clpy.smart:new key)))
         (clpy.object:set-item o -key value)))
   (clpy.exception:raise-generic-or-python-error)))

(defun has-key (o key)
  (plusp
   (if (stringp key)
       (clpy.ffi.fns:py-mapping-has-key-string o key)
       (clpy.util:let ((-key (clpy.smart:new key)))
         (clpy.ffi.fns:py-mapping-has-key o -key)))))

(defun keys (o)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-mapping-keys o)
   (clpy.exception:raise-generic-or-python-error)))

(defun values (o)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-mapping-values o)
   (clpy.exception:raise-generic-or-python-error)))

(defun items (o)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-mapping-items o)
   (clpy.exception:raise-generic-or-python-error)))
