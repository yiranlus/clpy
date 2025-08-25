(defpackage :clpy.list
  (:nicknames :py.list)
  (:use :cl)
  (:shadow #:append #:sort #:reverse)
  (:export #:p
	   #:exact-p
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
  (or (clpy.type:of o :list)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :list))))

(defun exact-p (o)
  (clpy.type:of o :list))

;; List creation
(defun new-of (n)
  "Create en empty list of ``N`` elements."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-new (or n 0))
    (error 'py.exc:generic-error)))

(defun new (&rest items)
  "Create a list from ITEMS."
  (let ((l (new-of (length items))))
    (loop for i in items
	  for index from 0
	  do (let ((py-i (clpy.smart:new i)))
	       (set-item l index py-i)))
    l))

(clpy.smart:new-hook #'(lambda (x) (and (listp x) (eq :list (car x))))
		     #'(lambda (x) (apply #'new (cdr x))))

(defun size (o)
  "Get the size of the list object ``O``."
  (clpy.ffi.fns:py-list-size o))

;; List access
(defun get-item (o index)
  "Get the element at the INDEX in the list object ``O``."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-get-item o index)
    (error 'py.exc:generic-error)))

(defun set-item (o index value)
  "Set the element at the INDEX to VALUE in the list object ``O``."
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-set-item o index -value))
    (error 'py.exc:generic-error)))

(defun insert (o index value)
  "Insert a VALUE at the INDEX in the list object ``O``."
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-insert o index -value))
    (error 'py.exc:generic-error)))

(defun append (o value)
  "Append a VALUE to the list object ``O``."
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-append o -value))
    (error 'py.exc:generic-error)))

(defun get-slice (o low high)
  "Get a slice of list object ``O``. The index are from LOW to HIGH."
  (clpy.ffi.fns:py-list-get-slice o low high))

(defun set-slice (o low high items)
  "Set the slice of list object ``O`` from LOW to HIGh to items.

This is akin to Python `list[low:high]=items`, the ITEMS can be NIL."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-set-slice o low high items)
    (error 'py.exc:generic-error)))

(defun sort (o)
  "Sort the list object ``O`` in place."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-sort o)
    (error 'py.exc:generic-error)))

(defun reverse (o)
  "Reverse the list object ``O`` in place."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-reverse o)
    (error 'py.exc:generic-error)))

(defun as-tuple (o)
  "Return a new tuple object containing the contents of list.

This is equivalent to Python ``tuple(o)``."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-as-tuple o)))
