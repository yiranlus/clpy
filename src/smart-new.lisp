(in-package :clpy.smart)

(defparameter *new-mapping* '())

(defun new-hook (pred new-func)
  "Add a smart type to the mapping. PRED should be a function takes one argument and return T or NIL. When PRED returns t, NEW-FUNC takes that argument to create a new value."
  (push (cons pred new-func) *new-mapping*))

(new-hook #'null (lambda (x) nil))

(defun new (x)
  (loop for (pred . new-func) in *new-mapping*
        when (funcall pred x)
          do (return-from new (funcall new-func x)))
  (error (format nil "Unsupported type ~A." (type-of x))))
