(defpackage :clpy.util
  (:nicknames :py.u)
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

;; utility for attribute access and indexing

(defmacro dot (o &rest subs)
  "Utility function to access attributes and items.

If you want to access an attribute, pass a an atom (a string), it will
return the attribute; if you pass a list, it will access the item. For
example,

.. code::common-lis

   (@ a \"c\" \"d\" (1) (\"f\"))

will be translated to

.. code::python

   a.c.d[1][\"f\"]

The there are three returned values. The first is the value of the
accessed attribute or item. The second is the last object being
accessed. For the example above, it is ``a.c.d[1]``. The last one is
the last attribute or item index being accessed. For the example
above, it is ``[\"f\"]``."
  (if (= 1 (length subs))
      (if (listp (car subs))
          `(values (clpy.object:get-item ,o ,(caar subs) ,o ,(caar subs)))
          `(values (clpy.object:get-attr ,o ,(car subs)) ,o ,(car subs)))
      (cl:let ((sub-obj (gensym)))
        `(let ((,sub-obj ,(if (listp (car subs))
                              `(clpy.object:get-item ,o ,(caar subs))
                              `(clpy.object:get-attr ,o ,(car subs)))))
           (dot ,sub-obj ,@(cdr subs))))))
