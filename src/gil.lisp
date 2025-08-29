(defpackage :clpy.gil
  (:nicknames :py.gil)
  (:use :cl)
  (:export #:ensure
           #:release
           #:get-this-thread-state))

(in-package :clpy.gil)

(defun ensure ()
  "Ensure the current thread is ready to call the Python C API
Regardless of the current state of Python, or GIL.

Return a PyGILState_STATE."
  (clpy.ffi.fns:py-gil-state-ensure))

(defun release (state)
  (clpy.ffi.fns:py-gil-state-release state))

(defun get-this-thread-state ()
  (clpy.ffi.fns:py-gil-state-get-this-thread-state))
