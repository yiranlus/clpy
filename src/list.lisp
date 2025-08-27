(defpackage :clpy.list
  (:nicknames :py.list)
  (:use :cl)
  (:shadow #:append #:sort #:reverse)
  (:export #:p
	   #:exact-p
	   #:new-of
	   #:new
	   #:len
	   #:size
	   #:get-item
	   #:set-item
	   #:insert
	   #:append
	   #:get-slice
	   #:set-slice
	   #:sort
	   #:reverse
	   #:as-tuple
	   #:clpy.obje))

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
  (clpy.util:ensure-null-as-nil
      (let ((l (new-of (length items))))
	(loop for i in items
	      for index from 0
	      do ;; set-item steal the reference
		 (let ((py-i (clpy.smart:new i)))
		   (set-item l index py-i)))
	l)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to create the list from~%~%~A~%~%" items)))

(clpy.smart:new-hook #'(lambda (x)
			 (and (listp x)
			      (eq :list (car x))))
		     #'(lambda (x)
			 (apply #'new (cdr x))))

(defun len (list)
  "Get the size of the list object ``LIST``."
  (clpy.ffi.fns:py-list-size list))

(defun size (list)
  (len list))

;; List access
(defun get-item (list index)
  "Get the element at the INDEX in the list object ``LIST``."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-get-item list index)
    (error 'py.exc:generic-error)))

(defun set-item (list index value)
  "Set the element at the INDEX to VALUE in the list object ``LIST``."
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-set-item list index -value))
    (error 'py.exc:generic-error)))

(defun insert (list index value)
  "Insert a VALUE at the INDEX in the list object ``LIST``."
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-insert list index -value))
    (error 'py.exc:generic-error)))

(defun append (list value)
  "Append a VALUE to the list object ``LIST``."
  (clpy.util:ensure-zero
      (clpy.util:let ((-value (clpy.smart:new value)))
	(clpy.ffi.fns:py-list-append list -value))
    (error 'py.exc:generic-error)))

(defun get-slice (list low high)
  "Get a slice of list object ``LIST``. The index are from LOW to HIGH."
  (clpy.ffi.fns:py-list-get-slice list low high))

(defun set-slice (list low high items)
  "Set the slice of list object ``LIST`` from LOW to HIGh to items.

This is akin to Python `list[low:high]=items`, the ITEMS can be NIL."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-set-slice list low high items)
    (error 'py.exc:generic-error)))

(defun sort (list)
  "Sort the list object ``LIST`` in place."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-sort list)
    (error 'py.exc:generic-error)))

(defun reverse (list)
  "Reverse the list object ``LIST`` in place."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-list-reverse list)
    (error 'py.exc:generic-error)))

(defun as-tuple (list)
  "Return a new tuple object containing the contents of list.

This is equivalent to Python ``tuple(list)``."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-list-as-tuple list)))
