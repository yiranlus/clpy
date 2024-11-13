(cl:in-package :clpy.smart)

(defun new (x)
  (cond
    ((stringp x) (py.str:new x))
    ((numberp x) (py.number:new x))
    ((py:object-p x) (py:inc-xref x) x)
    (t (error 'py.exc:generic-error
	      :message (format nil "Unsupported type ~A." (type-of x))))))
