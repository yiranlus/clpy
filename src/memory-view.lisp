(defpackage :clpy.memory-view
  (:nicknames :py.mv)
  (:use :cl)
  (:export #:new-from
	   #:get-contiguous))

(in-package :clpy.memory-view)

(clpy.type:define-type "PyMemoryView_Type" memory-view)

(defun new-from (obj &optional size flags)
  (clpy.util:ensure-null-as-nil
      (cond
	((typep obj 'clpy.ffi:py-buffer) (clpy.ffi.fns:py-memory-view-from-buffer))
	((clpy.object:p obj) (clpy.ffi.fns:py-memory-view-from-object obj))
	(t (clpy.ffi.fns:py-memory-view-from-memory obj size
						    (case flags
						      (:read  #x0100)
						      (:write #x0200)))))
    (clpy.exception:raise-generic-or-python-error)))

(defun get-contiguous (view buffer-type order)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-memory-view-get-contiguous view
						  (case buffer-type
						    (:read  #x0100)
						    (:write #x0200))
						  (case order
						    (:c (char-code #\C))
						    (:f (char-code #\F))))
    (clpy.exception:raise-generic-or-python-error)))
