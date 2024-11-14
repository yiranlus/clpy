(defpackage :clpy.smart
  (:nicknames :py.smart)
  (:use :cl)
  (:export #:new
           #:smart-hook))

(cl:in-package :clpy.smart)

(defparameter *new-mapping* '())

(defun smart-hook (pred new-func)
  "Add a smart type to the mapping. PRED should be a function takes one argument and return T or NIL. When PRED returns t, NEW-FUNC takes that argument to create a new value."
  (push (cons pred new-func) *new-mapping*))

(smart-hook #'null (lambda (x) nil))

(defun new (x)
  (loop for (pred . new-func) in *new-mapping*
        when (funcall pred x)
          do (return-from new (funcall new-func x)))
  (error (format nil "Unsupported type ~A." (type-of x))))
      
    ;; ((stringp x) (py.str:new x))
    ;; ((numberp x) (py.num:new x))
    ;; ((py.obj:object-p x) (py:inc-xref x) x)
    ;; ((and (listp x)
    ;;       (>= (length x) 2))
    ;;  (case (car x)
    ;;    (:dict (apply #'py.dict:new (cdr x)))
    ;;    (:set (apply #'py.set:new (cdr x)))
    ;;    (:list (apply #'py.list:new (cdr x)))
    ;;    (otherwise (error (format nil "Unsupported type ~A." (car x))))))
    ;; (t (error (format nil "Unsupported type ~A." (type-of x))))))
