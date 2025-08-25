(defpackage :clpy.dict
  (:nicknames :py.dict)
  (:use :cl)
  (:shadow #:values #:merge)
  (:export #:new
	   #:p
	   #:exact-p
	   #:proxy-new
	   #:clear
	   #:contains
	   #:copy
	   #:set-item
	   #:del-item
	   #:get-item
	   #:items
	   #:keys
	   #:values
	   #:size
	   #:merge
	   #:update))

(in-package :clpy.dict)

(clpy.type:define-type "PyDict_Type" dict)
(clpy.type:define-type "PyDictKeys_Type" dict-keys)
(clpy.type:define-type "PyDictValues_Type" dict-values)
(clpy.type:define-type "PyDictItems_Type" dict-items)
(clpy.type:define-type "PyDictIterKey_Type" dict-iter-key)
(clpy.type:define-type "PyDictIterValue_Type" dict-iter-value)
(clpy.type:define-type "PyDictIterItem_Type" dict-iter-item)
(clpy.type:define-type "PyDictRevIterKey_Type" dict-rev-iter-key)
(clpy.type:define-type "PyDictRevIterValue_Type" dict-rev-iter-value)
(clpy.type:define-type "PyDictRevIterItem_Type" dict-rev-iter-item)

(clpy.type:define-type "PyDictProxy_Type" dict-proxy)

(defun p (o)
  (or (clpy.type:of o :dict)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :dict))))

(defun exact-p (o)
  (clpy.type:of o :dict))

(defun new (&rest mapping)
  (let ((d (clpy.util:ensure-null-as-nil
		  (clpy.ffi.fns:py-dict-new)
		(error 'py.exc:generic-error :message "Unable to create new dict."))))
    (when (and mapping
               (not (and (= 1 (length mapping))
                         (null (car mapping)))))
      (loop for (k . v) in mapping
	    do (clpy.util:let ((-k (clpy.smart:new k))
			(-v  (clpy.smart:new v)))
		 (set-item d -k -v))))
    d))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :dict (car x))))
                     #'(lambda (x) (apply #'new (cdr x))))


(defun proxy-new (mapping)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-proxy-new mapping)
    (error 'py.exc:generic-error)))

(defun clear (o)
  (clpy.ffi.fns:py-dict-clear o))

(defun contains (o key)
  (case (clpy.util:let ((-key (clpy.smart:new key)))
	  (clpy.ffi.fns:py-dict-contains o -key))
    (1 t)
    (0 nil)
    (-1 (clpy.exception:raise-generic-or-python-error))))

(defun copy (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-copy o)
    (clpy.exception:raise-generic-or-python-error)))

(defun set-item (o key value)
  (clpy.util:ensure-non-negative
      (if (stringp key)
	  (clpy.util:let ((-value (clpy.smart:new value)))
	    (clpy.ffi.fns:py-dict-set-item-string o key -value))
	  (clpy.util:let ((-key (clpy.smart:new key))
		   (-value (clpy.smart:new value)))
	    (clpy.ffi.fns:py-dict-set-item o -key -value)))
    (clpy.exception:raise-generic-or-python-error)))

(defun del-item (o key)
  (clpy.util:ensure-non-negative
      (if (stringp key)
	  (clpy.ffi.fns:py-dict-del-item-string o key)
	  (clpy.util:let ((-key (clpy.smart:new key)))
	    (clpy.ffi.fns:py-dict-del-item o -key)))
    (clpy.exception:raise-generic-or-python-error)))

(defun get-item (o key &optional suppress-error)
  (clpy.util:ensure-null-as-nil
      (if suppress-error
	  (if (stringp key)
	      (clpy.ffi.fns:py-dict-get-item-string o key)
	      (clpy.ffi.fns:py-dict-get-item o key))
	  (clpy.util:let ((o (clpy.smart:new key)))
	    (clpy.ffi.fns:py-dict-get-item-with-error o key)))
    (clpy.exception:raise-generic-or-python-error)))

(defun items (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-items o)
    (clpy.exception:raise-generic-or-python-error)))

(defun keys (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-keys o)
    (clpy.exception:raise-generic-or-python-error)))

(defun values (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-values o)
    (clpy.exception:raise-generic-or-python-error)))

(defun size (o)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-dict-size o)
    (clpy.exception:raise-generic-or-python-error)))

(defun merge (dict-a dict-or-seq &key override)
  (clpy.util:ensure-zero
      (let ((-override (if override 1 0)))
	(cond
	  ((py.type:of dict-or-seq :dict)
	   (clpy.ffi.fns:py-dict-merge dict-a dict-or-seq -override))
	  ((py.seq:p dict-or-seq)
	   (clpy.ffi.fns:py-dict-merge-from-seq2 dict-a dict-or-seq -override))
	  (t (error 'py.exc:generic-error
		    :message (format nil "Unsupported tyep ~A to merge in dict."
				     (type-of dict-or-seq))))))
    (clpy.exception:raise-generic-or-python-error)))

(defun update (o dict &key override)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-dict-update o dict)
    (clpy.exception:raise-generic-or-python-error)))


