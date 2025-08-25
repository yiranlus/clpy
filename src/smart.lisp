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
  (loop for (pred . new-func) in *new-mapping*
        when (funcall pred x)
          do (return-from new (funcall new-func x)))
  (error (format nil "Unsupported type ~A." (type-of x))))

;; Print a Python object smartly

;; (defparameter *print-mapping* (make-hash-table))

;; (defun print-hook (type print-func)
;;   "Add a smart type to the mapping."
;;   (setf (gethash type *print-mapping*) print-func))

;; (defun print (x &optional stream)
;;   (if (clpy.object:p x)
;;       (let ((type (clpy.object:ob-type x)))
;;         (write-string (if (and (keywordp type)
;;                                (gethash type *print-mapping*))
;;                           (funcall (gethash type *print-mapping*) x)
;;                           (clpy.util:let* ((repr (clpy.obj:repr x))
;;                                            (code (clpy.str:encode repr)))
;;                             (py.bytes:as-string code)))
;;                       stream))
;;       (princ x stream)))
