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
  (cffi:pointer-eq (clpy.ffi.acc:py-object.ob-type o) (get type)))

(cl:defmacro define-type (cname name)
  (cl:let* ((name-str (cl:symbol-name name))
	    (const-sym (cl:intern (cl:format cl:nil "+~A-TYPE+" name-str)))
	    (kw-sym (cl:intern name-str :keyword)))
    `(cl:progn
       (cffi:defcvar (,cname ,const-sym :read-only cl:t) :int)
       (cl:push (cl:cons ,kw-sym (cffi:get-var-pointer ',const-sym)) *assoc-types*))))


(define-type "PyBaseObject_Type" base)

;;(defcvar "PyFrozenSet_Type" frozen-set)

;;(defcvar "CFunction_Type" c-function)
;;(defcvar "CallIter_Type" call-iter)
;;(defcvar "PyClassMethodDescr_Type" class-method-descr)

;; other types
(define-type "PyEnum_Type" enum)

(define-type "PyFilter_Type" filter)
(define-type "PyMap_Type" map)

(define-type "PyEllipsis_Type" ellipsis)
(define-type "PyRange_Type" range)
(define-type "PyRangeIter_Type" range-iter)
(define-type "PyReversed_Type" reversed)

