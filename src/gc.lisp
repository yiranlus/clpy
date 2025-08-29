(defpackage :clpy.gc
  (:nicknames :py.gc)
  (:use :cl)
  (:export #:collect
           #:enable
           #:disable
           #:enabled-p

           #:track
           #:tracked-p
           #:finalized-p
           #:del
           #:untrack))

(in-package :clpy.gc)

(defun collect ()
  (clpy.ffi.fns:py-gc-collect))

(defun enable ()
  (clpy.ffi.fns:py-gc-enable))

(defun disable ()
  (clpy.ffi.fns:py-gc-disable))

(defun enabled-p ()
  (plusp (clpy.ffi.fns:py-gc-is-enabled)))

(defun track (obj)
  "Adds the object OBJ to the set of container objects track by the collector."
  (clpy.ffi.fns:py-object-gc-track obj))

(defun tracked-p (obj)
  "Check if OBJ is tracked by GC."
  (clpy.ffi.fns:py-object-gc-is-tracked obj))

(defun finalized-p (obj)
  "Check if OP has been already finalized by GC."
  (clpy.ffi.fns:py-object-gc-is-finalized obj))

(defun del (obj)
  "Release memory allocated to an object."
  (clpy.ffi.fns:py-object-gc-del obj))

(defun untrack (obj)
  "Untrack the object OBJ."
  (clpy.ffi.fns:py-object-gc-un-track obj))
