(defpackage :clpy.memory
  (:nicknames :py.mem)
  (:use :cl)
  (:export #:malloc
	   #:calloc
	   #:realloc
	   #:free))

(in-package :clpy.memory)

(defun malloc (n)
  "Allocates N bytes and return the pointer."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-mem-malloc n)
    (clpy.exception:raise-generic-or-python-error)))

(defun calloc (nelem elsize)
  "Allocates NELEM elements each whose size in bytes is ELSIZE and return the pointer."
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-mem-calloc nelem elsize)
    (clpy.exception:raise-generic-or-python-error)))

(defun realloc (p n)
  "Resize the memory block pointed to P to n bytes and return the pointer."
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-mem-realloc p n)
    (clpy.exception:raise-generic-or-python-error)))

(defun free (p)
  "Frees the memory block pointed to by P."
  (clpy.ffi.fns:py-mem-free p))
