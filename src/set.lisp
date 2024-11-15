(defpackage :clpy.set
  (:nicknames :py.set)
  (:use :cl)
  (:shadow #:pop)
  (:export #:new
	   #:new-from
	   #:size
	   #:add
	   #:clear
	   #:discard
	   #:contains
	   #:pop))

(in-package :clpy.set)

(clpy.type:define-type "PySet_Type" set)

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :s (car x))))
                       #'(lambda (x) (apply #'new (cdr x))))

(defun new (&rest items)
  (py:ensure-null-as-nil
   (if items
       (py:let ((l (apply #'py.list:new items)))
	 (clpy.ffi.fns:py-set-new l))
       (clpy.ffi.fns:py-set-new (cffi:null-pointer)))
    (error 'py.exc:generic-error)))

(defun new-from (iterable)
  (py:ensure-null-as-nil
   (clpy.ffi.fns:py-set-new iterable)
   (error 'py.exc:generic-error)))

(defun clear (set)
  (py:ensure-non-negative
   (clpy.ffi.fns:py-set-clear set)
   (error 'py.exc:generic-error)))

(defun size (set)
  (clpy.ffi.fns:py-set-size set))

(defun contains (set key)
  (case (clpy.ffi.fns:py-set-contains set key)
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:generic-error))))

(defun add (set key)
  (py:ensure-zero
   (clpy.ffi.fns:py-set-add set key)
   (error 'py.exc:generic-error)))

(defun discard (set key)
  (case (clpy.ffi.fns:py-set-discard set key)
    (1 (values t :found))
    (0 (values t :not-found))
    (-1 (error 'py.exc:generic-error))))

(defun pop (set)
  (py:ensure-null-as-nil
   (clpy.ffi.fns:py-set-pop set)
   (error 'py.exc:generic-error)))

