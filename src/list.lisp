(cl:defpackage :clpy.list
  (:nicknames :py.list)
  (:use :cl)
  (:shadow #:append #:sort #:reverse)
  (:export #:new-of
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

(cl:in-package :clpy.list)

(clpy.type:define-type "PyList_Type" list)
(clpy.type:define-type "PyListIter_Type" list-iter)
(clpy.type:define-type "PyListRevIter_Type" list-rev-iter)

(defun size (o)
  (clpy.ffi.fns:py-list-size o))

(defun get-item (o i)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-list-get-item o i)
    (error 'py.exc:generic-error)))

(defun set-item (o i v)
  (py:ensure-zero
      (py:let ((-v (py:new v)))
	(clpy.ffi.fns:py-list-set-item o i -v))
    (error 'py.exc:generic-error)))

(defun insert (o i v)
  (py:ensure-zero
      (py:let ((-v (py:new v)))
	(clpy.ffi.fns:py-list-insert o i -v))
    (error 'py.exc:generic-error)))

(defun append (o v)
  (py:ensure-zero
      (py:let ((-v (py:new v)))
	(clpy.ffi.fns:py-list-append o -v))
    (error 'py.exc:generic-error)))

(defun get-slice (o l h)
  (clpy.ffi.fns:py-list-get-slice o l h))

(defun set-slize (o l h v)
  (py:ensure-zero
      (clpy.ffi.fns:py-list-set-slice o l h v)
    (error 'py.exc:generic-error)))

(defun sort (o)
  (py:ensure-zero
      (clpy.ffi.fns:py-list-sort o)
    (error 'py.exc:generic-error)))

(defun reverse (o)
  (py:ensure-zero
      (clpy.ffi.fns:py-list-reverse o)
    (error 'py.exc:generic-error)))

(defun as-tuple (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-list-as-tuple o)))

(defun new-of (size)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-list-new (or size 0))
    (error 'py.exc:generic-error)))

(defun new (&rest items)
  (let ((l (new-of (length items))))
    (loop for i in items
	  for index from 0
	  do (py:let ((py-i (py:new i)))
	       (set-item l index py-i)))
    l))
