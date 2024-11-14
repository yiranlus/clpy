(defpackage :clpy.memory
  (:nicknames :py.mem)
  (:use :cl)
  (:export))

(in-package :clpy.memory)

(defun malloc (n)
  "Allocates N bytes and return the pointer."
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-mem-malloc n)
    (error 'py.exc:generic-error)))

(defun calloc (nelem elsize)
  "Allocates NELEM elements each whose size in bytes is ELSIZE and return the pointer."
  (py:ensure-non-negative
      (clpy.ffi.fns:py-mem-calloc nelem elsize)
    (error 'py.exc:generic-error)))

(defun realloc (p n)
  "Resize the memory block pointed to P to n bytes and return the pointer."
  (py:ensure-non-negative
      (clpy.ffi.fns:py-mem-realloc p n)
    (error 'py.exc:generic-error)))

(defun free (p)
  "Frees the memory block pointed to by P."
  (clpy.ffi.fns:py-mem-free p))

(cl:in-package :clpy.object)

(defun malloc (n)
  "Allocates N bytes and return the pointer."
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-malloc n)
    (error 'py.exc:generic-error)))

(defun calloc (nelem elsize)
  "Allocates NELEM elements each whose size in bytes is ELSIZE and return the pointer."
  (py:ensure-non-negative
      (clpy.ffi.fns:py-object-calloc nelem elsize)
    (error 'py.exc:generic-error)))

(defun realloc (p n)
  "Resize the memory block pointed to P to n bytes and return the pointer."
  (py:ensure-non-negative
      (clpy.ffi.fns:py-object-realloc p n)
    (error 'py.exc:generic-error)))

(defun free (p)
  "Frees the memory block pointed to by P."
  (clpy.ffi.fns:py-object-free p))

