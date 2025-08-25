(defpackage :clpy.set
  (:nicknames :py.set)
  (:use :cl)
  (:shadow #:pop)
  (:export #:p
	   #:exact-p
	   #:fronzen-p
	   #:frozen-exact-p
	   #:any-p
	   #:any-exact-p
	   #:new
	   #:new-from
	   #:frozen-new
	   #:frozen-new-from
	   #:size
	   #:add
	   #:clear
	   #:discard
	   #:contains
	   #:pop))

(in-package :clpy.set)

(clpy.type:define-type "PySet_Type" set)
(clpy.type:define-type "PyFrozenSet_Type" frozen-set)

(defun p (o)
  (or (clpy.type:of o :set)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :set))))

(defun exact-p (o)
  (clpy.type:of o :set))

(defun frozen-p (o)
  "Check if ``O`` is a fronzen set."
  (or (clpy.type:of o :frozen-set)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :frozen-set))))

(defun frozen-exact (o)
  (clpy.type:of o :frozen-set))

(defun any-p (o)
  "Check if ``O`` is either a set or a frozen set."
  (or (p o) (frozen-p o)))

(defun any-exact (o)
  (or (exact-p o) (frozen-exact-p o)))

;; Set creation
(defun new (&rest items)
  (clpy.util:ensure-null-as-nil
      (if items
	  (clpy.util:let ((l (apply #'py.list:new items)))
	    (clpy.ffi.fns:py-set-new l))
	  (clpy.ffi.fns:py-set-new (cffi:null-pointer)))
    (clpy.exception:raise-generic-or-python-error)))

(defun new-from (iterable)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-set-new iterable)
    (clpy.exception:raise-generic-or-python-error)))
      

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :set (car x))))
		     #'(lambda (x) (apply #'new (cdr x))))

(defun frozen-new (&rest items)
  (clpy.util:ensure-null-as-nil
      (if items
	  (clpy.util:let ((l (apply #'py.list:new items)))
	    (clpy.ffi.fns:py-set-new l))
	  (clpy.ffi.fns:py-frozen-set-new (cffi:null-pointer)))
    (clpy.exception:raise-generic-or-python-error)))

(defun frozen-new-from (iterable)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-frozen-set-new iterable)
    (clpy.exception:raise-generic-or-python-error)))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :frozen-set (car x))))
		     #'(lambda (x) (apply #'clpy.set:frozen-new (cdr x))))

;; Set access
(defun clear (set)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-set-clear set)
    (clpy.exception:raise-generic-or-python-error)))

(defun size (set)
  (clpy.ffi.fns:py-set-size set))

(defun contains (set key)
  (case (clpy.ffi.fns:py-set-contains set key)
    (1 t)
    (0 nil)
    (-1 (clpy.exception:raise-generic-or-python-error))))

(defun add (set key)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-set-add set key)
    (clpy.exception:raise-generic-or-python-error)))

(defun discard (set key)
  (case (clpy.ffi.fns:py-set-discard set key)
    (1 (values t :found))
    (0 (values t :not-found))
    (-1 (clpy.exception:raise-generic-or-python-error))))

(defun pop (set)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-set-pop set)
    (clpy.exception:raise-generic-or-python-error)))
