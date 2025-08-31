(defpackage :clpy.bool
  (:nicknames :py.bool)
  (:use :cl)
  (:export #:new
           #:p))

(in-package :clpy.bool)

(clpy.type:define-type "PyBool_Type" bool)

(defun p (o)
  (clpy.type:of o :bool))


(defun new (&optional (v nil))
  "Create a new bool from value ``V``.

``V`` can be any value, but it will be evaluated to ``T`` or NIL."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-bool-from-long (if v 1 0))
    (error 'py.exc:generic-error)))


(clpy.smart:new-hook #'(lambda (x)
                         (or (eq x :true)
                             (eq x :false)))
                     #'(lambda (x)
                         (if (eq x :true)
                             (new t)
                             (new nil))))

(clpy.smart:print-hook #'p
                       #'(lambda (x)
                           (if (clpy.object:true-p x t)
                               "TRUE"
                               "FALSE")))
