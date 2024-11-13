(defpackage :clpy.dict
  (:nicknames :py.dict)
  (:use :cl)
  (:shadow #:values #:merge)
  (:export #:new
	   #:p
	   #:new-proxy
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
(clpy.type:define-type "PyDictvalues_Type" dict-values)
(clpy.type:define-type "PyDictItems_Type" dict-items)
(clpy.type:define-type "PyDictIterKey_Type" dict-iter-key)
(clpy.type:define-type "PyDictIterValue_Type" dict-iter-value)
(clpy.type:define-type "PyDictIterItem_Type" dict-iter-item)
(clpy.type:define-type "PyDictRevIterKey_Type" dict-rev-iter-key)
(clpy.type:define-type "PyDictRevIterValue_Type" dict-rev-iter-value)
(clpy.type:define-type "PyDictRevIterItem_Type" dict-rev-iter-item)

(clpy.type:define-type "PyDictProxy_Type" dict-proxy)

(defun new (&rest mapping)
  (let ((d (py:ensure-null-as-nil
		  (clpy.ffi.fns:py-dict-new)
		(error 'py.exc:generic-error :message "Unable to create new dict."))))
    (when mapping
      (loop for (k . v) in mapping
	    do (py:let ((-k (py:new k))
			(-v  (py:new v)))
		 (set-item d -k -v))))
    d))

(defun p (o)
  (clpy.type:of o :dict))

(defun new-proxy (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-proxy-new o)
    (error 'py.exc:generic-error)))

(defun clear (o)
  (clpy.ffi.fns:py-dict-clear o))

(defun contains (o key)
  (case (py:let ((-key (py:new key)))
	  (clpy.ffi.fns:py-dict-contains o -key))
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:generic-error))))

(defun copy (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-copy o)
    (error 'py.exc:generic-error)))

(defun set-item (o key value)
  (py:ensure-non-negative
      (if (stringp key)
	  (py:let ((-value (py:new value)))
	    (clpy.ffi.fns:py-dict-set-item-string o key -value))
	  (py:let ((-key (py:new key))
		   (-value (py:new value)))
	    (clpy.ffi.fns:py-dict-set-item o -key -value)))
    (error 'py.exc:generic-error
	   :message (format nil "Unable to set item with key ~A to ~A."
			    key value))))

(defun del-item (o key)
  (py:ensure-non-negative
      (if (stringp key)
	  (clpy.ffi.fns:py-dict-del-item-string o key)
	  (py:let ((-key (py:new key)))
	    (clpy.ffi.fns:py-dict-del-item o -key)))
    (error 'py.exc:generic-error)))

(defun get-item (o key &optional new-ref)
  (py:ensure-null-as-nil
      (if (stringp key)
	  (clpy.ffi.fns:py-dict-get-item-string o key)
	  (clpy.ffi.fns:py-dict-get-item o key))
    (error 'py.exc:generic-error)))

(defun items (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-items o)
    (error 'py.exc:generic-error)))

(defun keys (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-keys o)
    (error 'py.exc:generic-error)))

(defun values (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-dict-values o)
    (error 'py.exc:generic-error)))

(defun size (o)
  (py:ensure-non-negative
      (clpy.ffi.fns:py-dict-size o)
    (error 'py.exc:generic-error)))

(defun merge (dict-a dict-or-seq &key override)
  (py:ensure-zero
      (let ((-override (if override 1 0)))
	(cond
	  ((py.type:of dict-or-seq :dict)
	   (clpy.ffi.fns:py-dict-merge dict-a dict-or-seq -override))
	  ((py.seq:p dict-or-seq)
	   (clpy.ffi.fns:py-dict-merge-from-seq2 dict-a dict-or-seq -override))
	  (t (error 'py.exc:generic-error
		    :message (format nil "Unsupported tyep ~A to merge in dict."
				     (type-of dict-or-seq))))))
    (error 'py.exc:generic-error)))

(defun update (o dict &key override)
  (py:ensure-zero
      (clpy.ffi.fns:py-dict-update o dict)
    (error 'py.exc:generic-error)))

