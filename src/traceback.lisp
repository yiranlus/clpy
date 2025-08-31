(defpackage :clpy.traceback
  (:nicknames :py.tb)
  (:use :cl)
  (:shadow #:here
           #:print))

(in-package :clpy.traceback)

(clpy.type:define-type "PyTraceBack_Type" traceback)

(defun here (frame)
  (clpy.util:ensure-zero
   (clpy.ffi.fns:py-trace-back-here frame)
   (clpy.exception:raise-generic-or-python-error
    :message "Unable to add an etry to the traceback.")))

(defun print (tb file)
  "Print the tracebak.

FILE is a file-like object (e.g. ``sys.stderr`` or an object support
``.write()`` method."
  (clpy.util:ensure-zero
   (clpy.ffi.fns:py-trace-back-print tb file)
   (clpy.exception:raise-generic-or-python-error
    :message "Unable to print the trackback.")))
