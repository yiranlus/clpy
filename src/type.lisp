(defpackage :clpy.type
  (:nicknames :py.type)
  (:use :cl)
  (:shadow :get)
  (:export #:*assoc-types*
	   #:define-type
	   #:from
	   #:get
	   #:of
	   #:+PY-LT+
	   #:+PY-LE+
	   #:+PY-EQ+
	   #:+PY-NE+
	   #:+PY-GT+
	   #:+PY-GE+

	   #:p
	   #:clear-cache
	   #:get-flags
	   #:modified
	   #:subtype-p
	   #:get-name
	   #:get-qual-name
	   #:get-module))

(in-package :clpy.type)

(cl:defparameter *assoc-types* '())

(defun from (py-type)
  "Return the corresponding keyword to the Python exception."
  (car (rassoc py-type *assoc-types* :test #'cffi:pointer-eq)))

(defun get (kw)
  (cdr (assoc kw *assoc-types*)))

(defun of (o type)
  (cffi:pointer-eq (clpy.ffi.acc:py-object.ob-type o) (get type)))

(defconstant +PY-LT+ 0)
(defconstant +PY-LE+ 1)
(defconstant +PY-EQ+ 2)
(defconstant +PY-NE+ 3)
(defconstant +PY-GT+ 4)
(defconstant +PY-GE+ 5)

(defmacro define-type (cname name)
  (let* ((name-str (symbol-name name))
	 (kw-sym (intern name-str :keyword)))
    `(push (cons ,kw-sym (cffi:foreign-symbol-pointer ,cname)) *assoc-types*)))

;; Type Object

(define-type "PyType_Type" type)

(defun p (o)
  (or (clpy.type:of o :type)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :type))))

(defun exact-p (o)
  (clpy.type:of o :type))

(defun clear-cache ()
  (clpy.ffi.fns:py-type-clear-cache))

(defun get-flags (type)
  "Return the ``tp_flags`` member of TYPE."
  (clpy.ffi.fns:py-type-get-flags type))

(defun modified (type)
  "Invalidate the internal lookup cache for the type and all of its subtypes."
  (clpy.ffi.fns:py-type-modified type))

(defun subtype-p (type-a type-b)
  "Return ``T`` if TYPE-A is a subtype of TYPE-B."
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-type-is-subtype type-a type-b)
    (clpy.exception:raise-generic-or-python-error)))

;; Properties

(defun get-name (type)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-type-get-name type)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-qual-name (type)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-type-get-qual-name type)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-module (type)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-type-get-module type)
    (clpy.exception:raise-generic-or-python-error)))


;; (define-type "PyBaseObject_Type" base)

;;(defcvar "PyFrozenSet_Type" frozen-set)

;;(defcvar "CFunction_Type" c-function)
;;(defcvar "CallIter_Type" call-iter)
;;(defcvar "PyClassMethodDescr_Type" class-method-descr)

;; other types
;; (define-type "PyEnum_Type" enum)

;; (define-type "PyFilter_Type" filter)
;; (define-type "PyMap_Type" map)

;; (define-type "PyEllipsis_Type" ellipsis)
;; (define-type "PyRange_Type" range)
;; (define-type "PyRangeIter_Type" range-iter)
;; (define-type "PyReversed_Type" reversed)

