(defpackage :clpy.thread
  (:nicknames :py.th)
  (:use :cl)
  (:shadow #:get)
  (:export #:state-new
           #:state-get
           #:state-get-id
           #:state-get-dict
           #:state-get-frame
           #:state-get-interpreter
           #:state-swap
           #:state-clear
           #:state-delete
           #:state-set-async-exc))

(in-package :clpy.thread)

(defun state-new (interp)
  (clpy.ffi.fns:py-thread-state-new interp))

(defun state-get ()
  (clpy.ffi.fns:py-thread-state-get))

(defun state-swap (tstate)
  (clpy.ffi.fns:py-thread-state-swap tstate))

(defun state-get-dict ()
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-thread-state-get-dict)))

(defun state-get-frame (tstate)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-thread-state-get-frame tstate)))

(defun state-get-id (tstate)
  (clpy.ffi.fns:py-thread-state-get-id tstate))

(defun state-get-interpreter (tstate)
  (clpy.ffi.fns:py-thread-state-get-interpreter tstate))

(defun state-set-async-exc (id exc)
  (clpy.ffi.fns:py-thread-state-set-async-exc id exc))

(defun state-clear (tstate)
  (clpy.ffi.fns:py-thread-state-clear tstate))

(defun state-delete (tstate)
  (clpy.ffi.fns:py-thread-state-delete tstate))
