(defpackage :clpy.os
  (:nicknames :py.os)
  (:use :cl)
  (:export #:fs-path
	   #:interrupt-occurred
	   #:before-fore
	   #:after-fork-parent
	   #:after-fork-child))

(in-package :clpy.os)

(defparameter *input-hook*
  (plus-c:c-ref
   (cffi:foreign-symbol-pointer "PyOS_InputHook")
   :pointer))

(defun fs-path (path)
  (clpy.util:ensure-null-as-nil
      (clpy.util:let ((path-str (clpy.str:new path)))
	(clpy.ffi.fns:py-os-fs-path path-str))
    (clpy.exception:raise-generic-or-python-error
     :message "Failed to get the FS representation for the path.")))

(defun interrupt-occurred ()
  (plusp (clpy.ffi.fns:py-os-interrupt-occurred)))

(defun before-fork ()
  (clpy.ffi.fns:py-os-before-fork))

(defun after-fork-parent ()
  (clpy.ffi.fns:py-os-after-fork-parent))

(defun after-fork-child ()
  (clpy.ffi.fns:py-os-after-fork-child))
