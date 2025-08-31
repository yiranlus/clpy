(defpackage :clpy.smart
  (:nicknames :py.smart)
  (:use :cl)
  (:shadow #:print)
  (:export #:new
           #:new-hook
           #:print
           #:print-hook))

(in-package :clpy.smart)

;; Create a Python object smartly

(defparameter *new-mapping* '())

(defun new-hook (pred new-func)
  "Add a smart type to the mapping.

PRED should be a function takes one argument and return T or NIL. When
PRED returns t, NEW-FUNC takes that argument to create a new value."
  (push (cons pred new-func) *new-mapping*))

(new-hook #'null (lambda (x) nil))

(defun new (x)
  "Create a new PyObject depends on ``X``.

A list of predictors registered by :cl:function:`new-hook` will be applied on
``X`` and the corresponding creation function will be applied to ``X`` for the
first predictor that return ``T``."
  (when (clpy.object:p x)
    (return-from new (clpy.object:new-ref x)))
  (loop for (pred . new-func) in *new-mapping*
        when (funcall pred x)
          do (return-from new (funcall new-func x)))
  (error (format nil "Unsupported type ~A." (type-of x))))

;; Print a Python object smartly

(defparameter *print-mapping* '())

(defun print-hook (pred print-func)
  "Add a smart type to the mapping."
  (push (cons pred print-func) *print-mapping*))

(defun print (x &optional (stream *standard-output*))
  "Similar to :cl:function:`new`, but used for print the object."
  (loop for (pred . print-func) in *print-mapping*
        when (funcall pred x)
          do (return-from print
               (values t
                       (let ((res (funcall print-func x)))
                         (if *print-escape*
                             (format stream "~S" res)
                             (format stream "~A" res)))))))
