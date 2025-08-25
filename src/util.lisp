(defpackage :clpy.util
  (:use :cl)
  (:shadow #:let #:let*)
  (:export #:ensure-null-as-nil
	   #:ensure-zero
	   #:ensure-non-negative
	   #:let
	   #:let*))

(in-package :clpy.util)

;;(defmacro auto-dec-ref (varlist &body body)
;;  `(let (,@varlist)
;;     ,@body
;;     ,@(loop for i in `(,@varlist)
;;            if (listp i)
;;              collect `(dec-ref ,(first i))
;;            else
;;              collect `(dec-ref ,i))))

(defmacro ensure-null-as-nil (value &body body)
  (cl:let ((res (gensym)))
    `(cl:let ((,res ,value))
       (if (not (cffi:null-pointer-p (autowrap:ptr ,res)))
           ,res
	   (progn ,@body)))))

(defmacro ensure-zero (value &body body)
  (cl:let ((res (gensym)))
    `(cl:let ((,res (zerop ,value)))
       (or ,res
           (progn ,@body)))))

(defmacro ensure-non-negative (value &body body)
  (cl:let ((res (gensym)))
    `(cl:let ((,res ,value))
       (if (not (minusp ,res))
           ,res
           (progn ,@body)))))

(defmacro let (varlist &body body)
  (cl:let ((res (gensym)))
    `(cl:let ,varlist
       (cl:let ((,res (progn ,@body)))
	 ,@(loop for i in varlist
		 collect (if (atom i)
			     `(clpy.object:dec-xref ,i)
			     `(clpy.object:dec-xref ,(car i))))
	 ,res))))

(defmacro let* (varlist &body body)
  (cl:let ((res (gensym)))
    `(cl:let* ,varlist
       (cl:let ((,res (progn ,@body)))
	 ,@(loop for i in varlist
		 collect (if (atom i)
			     `(clpy.object:dec-xref ,i)
			     `(clpy.object:dec-xref ,(car i))))
	 ,res))))
