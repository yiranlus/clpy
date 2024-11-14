(defpackage :clpy.pylet
  (:use :cl)
  (:shadow #:let #:let*)
  (:export #:let #:let*))

(cl:in-package :clpy.pylet)

(defmacro let (varlist &body body)
  (cl:let ((res (gensym)))
    `(cl:let ,varlist
       (cl:let ((,res (progn ,@body)))
	 ,@(loop for i in varlist
		 collect (if (atom i)
			     `(py.obj:dec-xref ,i)
			     `(py.obj:dec-xref ,(car i))))
	 ,res))))

(defmacro let* (varlist &body body)
  (cl:let ((res (gensym)))
    `(cl:let* ,varlist
       (cl:let ((,res (progn ,@body)))
	 ,@(loop for i in varlist
		 collect (if (atom i)
			     `(py.obj:dec-xref ,i)
			     `(py.obj:dec-xref ,(car i))))
	 ,res))))
