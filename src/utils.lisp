(defpackage :clpy.util
  (:use :cl)
  (:export #:ensure-null-as-nil
	   #:ensure-zero
	   #:ensure-non-negative))

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
  (let ((res (gensym)))
    `(let ((,res ,value))
       (if (not (cffi:null-pointer-p (autowrap:ptr ,res)))
           ,res
	   (progn ,@body)))))

(defmacro ensure-zero (value &body body)
  (let ((res (gensym)))
    `(let ((,res (zerop ,value)))
       (or ,res
           (progn ,@body)))))

(defmacro ensure-non-negative (value &body body)
  (let ((res (gensym)))
    `(let ((,res ,value))
       (if (not (minusp ,res))
           ,res
           (progn ,@body)))))
