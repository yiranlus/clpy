(defpackage :clpy.type
  (:nicknames :py.type)
  (:export #:*assoc-types*
	   #:define-type
	   #:from
	   #:get
	   #:of))

(in-package :clpy.type)

(cl:defparameter *assoc-types* '())

(cl:defun from (py-type)
  "Return the corresponding keyword to the Python exception."
  (cl:car (cl:rassoc py-type *assoc-types* :test #'cffi:pointer-eq)))

(cl:defun get (kw)
  (cl:cdr (cl:assoc kw *assoc-types*)))

(cl:defun of (o type)
  (cffi:pointer-eq (plus-c:c-ref o clpy.ffi:py-object :ob-type) (get type)))

(cl:defmacro define-type (cname name)
  (cl:let* ((name-str (cl:symbol-name name))
	    (const-sym (cl:intern (cl:format cl:nil "+~A-TYPE+" name-str)))
	    (kw-sym (cl:intern name-str :keyword)))
    `(cl:progn
       (cffi:defcvar (,cname ,const-sym :read-only cl:t) :int)
       (cl:push (cl:cons ,kw-sym (cffi:get-var-pointer ',const-sym)) *assoc-types*))))


(define-type "PyBaseObject_Type" base)

;;(defcvar "PyBytes_Type" bytes)
;;(defcvar "PyByteArray_Type" byte-array)
;;(defcvar "PyByteArrayIter_Type" byte-array-iter)

;;(defcvar "PyFrozenSet_Type" frozen-set)

;; Basic types



;;(defcvar "PyEnum_Type" enum)

;; Dict
;;(defcvar "PyDict_Type" dict)

;;(defcvar "PyDictItems_Type" dict-items)
;;(defcvar "PyDictIterItem_Type" dict-iter-item)
;;(defcvar "PyDictIterKey_Type" dict-iter-key)
;;(defcvar "PyDictIterValue_Type" dict-iter-value)
;;(defcvar "PyDictKeys_Type" dict-keys)

;;(defcvar "PyDictProxy_Type" dict-proxy)
;;(defcvar "PyDictRevIterItem_Type" dict-rev-iter-item)
;;(defcvar "PyDictRevIterKey_Type" dict-rev-iter-key)
;;(defcvar "PyDictRevIterValue_Type" dict-rev-iter-value)
;;(defcvar "PyDictValues_Type" dict-values)

;;(defcvar "PyCapsule_Type" capsule)

;;(defcvar "CFunction_Type" c-function)
;;(defcvar "CallIter_Type" call-iter)
;;(defcvar "PyClassMethodDescr_Type" class-method-descr)

;;(defcvar "PyFilter_Type" filter)

;; Misc
;;(defcvar "PyEllipsis_Type" ellipsis)
