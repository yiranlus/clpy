;; This package is used to create a C module
;; Not needed for now

(defpackage :clpy.module-def
  (:nicknames :py.module-def)
  (:use :cl :plus-c)
  (:export #:create))

(in-package :clpy.module-def)

(defun create (&key
		 name
		 docstring
		 size
		 methods
		 slots
		 traverse
		 clear
		 free)
  (c-let ((module-def clpy.ffi:py-module-def :calloc t))
    (initialize))
