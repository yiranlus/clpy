(cl:in-package :clpy.smart)

(defun new (x)
  (cond
    ((stringp x) (py.str:new x))
    ((numberp x) (py.num:new x))
    ((py:object-p x) (py:inc-xref x) x)
    ((and (listp x)
	  (>= (length x) 2))
     (case (car x)
       (:list (apply #'py.list:new (cdr x)))
       (:dict (apply #'py.dict:new (cdr x)))
       (:set (apply #'py.set:new (cdr x)))
       (otherwise
	(error 'py.exc:generic-error
	       :message (format nil "Unsupported type ~A." (type-of x))))))
    (t (error 'py.exc:generic-error
	      :message (format nil "Unsupported type ~A." (type-of x))))))
