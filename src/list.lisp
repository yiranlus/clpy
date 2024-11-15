(defpackage :clpy.list
  (:nicknames :py.list)
  (:use :cl)
  (:shadow #:append #:sort #:reverse)
  (:export #:p
	   #:new-of
	   #:new
	   #:size
	   #:get-item
	   #:set-item
	   #:insert
	   #:append
	   #:get-slice
	   #:set-slice
	   #:sort
	   #:reverse
	   #:as-tuple))

(in-package :clpy.list)

(clpy.type:define-type "PyList_Type" list)
(clpy.type:define-type "PyListIter_Type" list-iter)
(clpy.type:define-type "PyListRevIter_Type" list-rev-iter)

(defun p (o)
  (clpy.type:of o :list))

(defun new-of (size)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-new (or size 0))
    (error 'py.exc:generic-error)))

(defun new (&rest items)
  (let ((l (new-of (length items))))
    (loop for i in items
	  for index from 0
	  do (let ((py-i (clpy.smart:new i)))
	       (set-item l index py-i)))
    l))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :l (car x))))
                       #'(lambda (x) (apply #'new (cdr x))))

(defun size (o)
  (clpy.ffi.fns:py-list-size o))

(defun get-item (o index)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-get-item o index)
    (error 'py.exc:generic-error)))

(defun set-item (o index value)
  (clpy.util:ensure-zero
      (clpy.pylet:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-set-item o index -value))
    (error 'py.exc:generic-error)))

(defun insert (o index value)
  (clpy.util:ensure-zero
      (clpy.pylet:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-insert o index -value))
    (error 'py.exc:generic-error)))

(defun append (o value)
  (clpy.util:ensure-zero
      (clpy.pylet:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-append o -value))
    (error 'py.exc:generic-error)))

(defun get-slice (o low high)
  (clpy.ffi.fns:py-list-get-slice o low high))

(defun set-slize (o low high value)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-set-slice o low high value)
    (error 'py.exc:generic-error)))

(defun sort (o)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-sort o)
    (error 'py.exc:generic-error)))

(defun reverse (o)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-reverse o)
    (error 'py.exc:generic-error)))

(defun as-tuple (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-as-tuple o)))
