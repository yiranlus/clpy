(defpackage :clpy.type
  (:nicknames :py.type)
  (:use :cl :plus-c)
  (:shadow :get)
  (:export #:*assoc-types*
	   #:define-type
	   #:define-type-from
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
	   #:get-module
	   #:get-module-state

	   #:ready))

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

(defun define-type-from (type-obj name)
  "Define a new type.

This function is used for defining a new type of named-tuple. The name
is automatically upper-cased."
  (let ((kw (intern (string-upcase name) :keyword)))
    (push (cons kw (autowrap:ptr type-obj)) *assoc-types*)))

;; Type Object

(define-type "PyType_Type" type)
(define-type "PyBaseObject_Type" base)
(define-type "PySuper_Type" super)

(define-type "PyEnum_Type" enum)

(define-type "PyFilter_Type" filter)
(define-type "PyMap_Type" map)
(define-type "PyZip_Type" zip)

(define-type "PyRange_Type" range)
(define-type "PyRangeIter_Type" range-iter)
(define-type "PyReversed_Type" reversed)

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
  (plusp
   (clpy.util:ensure-non-negative
       (let ((-type-a (if (keywordp type-a)
			  (get type-a)
			  type-a))
	     (-type-b (if (keywordp type-b)
			  (get type-b)
			  type-b)))
	 (clpy.ffi.fns:py-type-is-subtype -type-a -type-b))
     (clpy.exception:raise-generic-or-python-error))))

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

(defun get-module-state (type)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-type-get-module-state type)
    (clpy.exception:raise-generic-or-python-error)))

;; Create New Type

;; TODO: too many macro definition in C

;; (defun ready (type)
;;   (clpy.util:ensure-zero
;;       (clpy.ffi.fns:py-type-ready type)
;;     (clpy.exception:raise-generic-or-python-error)))

;; (defun make-flags (&rest flags)
;;   (labels ((interp (flag)
;;              (if (integerp flag)
;;                  flag
;;                  (case flag
;; 		   (:have-finalize                 #x00000000)
;; 		   ;; following flags are not supported by limited API
;;                    (:static-builtin                #x00000002)
;;                    (:managed-weakref               #x00000008)
;;                    (:managed-dict                  #x00000010)
;;                    (:pre-header                    (logior (interp :managed-weakref) (interp :managed-dict)))
;;                    (:sequence                      #x00000020)
;; 		   (:mapping                       #x00000040)
;; 		   ;; supported by limited API
;; 		   (:disallow-instantiation        #x00000080)
;; 		   (:immutable-type                #x00000100)
;; 		   (:heap-type                     #x00000200)
;; 		   (:base-type                     #x00000400)
;; 		   (:have-vector-call              #x00000800)
;; 		   (:ready                         #x00001000)
;; 		   (:readying                      #x00002000)
;; 		   (:have-gc                       #x00004000)
;; 		   (:have-stackless-extension      #x00000000) ; for stackless Python, not applicable
;; 		   (:method-descriptor             #x00020000)
;; 		   (:have-version-tag              #x00040000
;; 		   (:valid-version-tag             #x00080000)
;; 		   (:is-abstract                   #x00100000)
;; 		   (:match-self                    #x00400000)
;; 		   (:items-at-end                  #x00800000)
;; 		   (:long-subclass                 #x01000000)
;; 		   (:list-subclass                 #x02000000)
;; 		   (:tuple-subclass                #x04000000)
;; 		   (:bytes-subclass                #x08000000)
;; 		   (:unicode-subclass              #x1000000)
;; 		   (:dict-subclass                 #x20000000)
;; 		   (:base-exc-subclass             #x40000000)
;; 		   (:type-subclass                 #x80000000)
;; 		   (:default                       (logior (interp :have-stackless-extension) #x00000000))
;;                    (otherwise (error "Unsuppoerted Py_Buffer flags."))))))
;;     (reduce #'(lambda (a b)
;;                 (logior (interp a) (interp b)))
;;             flags :initial-value #x0000)))


;; (defun new-spec (name basicsize itemsize slots &rest flags)
;;   (c-let ((spec clpy.ffi:py-type-spec))
;;     (setf (spec :name) name
;; 	  (spec :basicsize) basicsize
;; 	  (spec :itemsize) itemsize
;; 	  (spec :flags) (apply #'make-flags flags))
;;     ))


;; (defun new (spec &optional bases module))


