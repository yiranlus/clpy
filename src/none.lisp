(defpackage clpy.none
  (:nicknames :py.none)
  (:use :cl)
  (:export #:new))

(in-package clpy.none)

(defun new ()
  (new-ref (plus-c:c-ref
	    (cffi:foreign-symbol-pointer "_Py_NoneStruct")
	    clpy.ffi:py-object)))

(clpy.smart:new-hook #'(lambda (x) (eq x :none))
		     #'(lambda (x) (new)))
