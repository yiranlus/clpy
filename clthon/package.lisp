(defpackage :clthon
  (:use :cl)
  (:export #:code
           #:from
           #:call
           #:ncall
           #:@))

(in-package :clthon)

(defparameter *object-registry* '())
(defparameter *object-hash-registry* (make-hash-table))

(defun free-registry ()
  (dolist (object *object-registry*)
    (py:dec-xref (cdr object)))
  (setf *object-registry* '())
  (clrhash *object-hash-registry*))

(defun import-attr (alias module name)
  (if (nth-value 1 (gethash alias *object-hash-registry*))
      (error (format nil "Symbol ~A was already used." alias))
      (let ((obj (py.obj:get-attr module name)))
        (push (cons alias obj) *object-registry*)
        (setf (gethash alias *object-hash-registry*) obj))))

(defmacro -import (module import-list)
  `(progn
     ,(loop for attr in import-list
            collect
            (multiple-value-bind (name alias)
                (if (listp attr)
                    (if (eq :as (second attr))
                        (values (symbol-name (first attr))
                                (third attr))
                        (error "Syntax not correct, please use :as keyword."))
                    (values (symbol-name attr)
                            (intern (string-upcase (symbol-name attr)))))
              `(import-attr ',alias ,module ,name)))))

(defun import-module (alias name)
  (if (nth-value 1 (gethash alias *object-hash-registry*))
      (error (format nil "Symbol ~A was already used." alias))
      (let ((obj (py:import name)))
        (push (cons alias obj) *object-registry*)
        (setf (gethash alias *object-hash-registry*) obj)
        obj)))

(defmacro from (module &optional import-key attrs)
  (multiple-value-bind (name alias)
      (if (listp module)
          (if (eq :as (second module))
              (values (symbol-name (first module))
                      (third module))
              (error "Syntax not correct, please use :as."))
          (values (symbol-name module)
                  (if import-key
                      (gensym)
                      module)))
    (let ((obj (gensym)))
      `(progn
         (let ((,obj (import-module ',alias ,name)))
           ,(if import-key
                (if (eq import-key :import)
                    `(-import ,obj ,attrs)
                    (error "Syntax not correct, please use :import"))
                `(declare (ignore ,obj))))))))

(defmacro g (o-name)
  (if (and (listp o-name)
           (eq 'quote (first o-name))
           (symbolp (second o-name)))
      `(gethash ,o-name *object-hash-registry*)
      o-name))


(defmacro call (obj &optional args kwargs)
  (let ((-args (gensym))
        (-kwargs (gensym)))
    `(py:let ((,-args ,(when args
                         `(py.tuple:new ,@args)))
              (,-kwargs ,(when kwargs
                           `(py.dict:new
                             ,@(mapcar (lambda (x)
                                         `(cons ,(symbol-name (car x)) ,(cdr x)))
                                       kwargs)))))
       (py:call (g ,obj) :args ,-args :kwargs ,-kwargs))))

(defmacro ncall (obj &optional args kwargs)
  (let ((res (gensym)))
    `(py:let ((,res (call ,obj ,args ,kwargs))))))

(defmacro -@ (o &rest attrs)
  (if (null attrs)
      `(py:new-xref ,o)
      (cl:let ((sub-obj (gensym)))
        `(py:let ((,sub-obj ,(if (listp (car attrs))
                              `(clpy.object:get-item ,o ,(caar attrs))
                              `(clpy.object:get-attr ,o ,(symbol-name (car attrs))))))
           (-@ ,sub-obj ,@(cdr attrs))))))

(defmacro @ (o &rest attrs)
  `(-@ (g ,o) ,@attrs))

(define-setf-expander @ (obj &rest attrs &environment env)
  (declare (ignore env))
  (let ((obj-var (gensym))
        (store-var (gensym)))
    (values `()
            `()
            `(,store-var)
            (let ((o (gensym))
                  (v (gensym)))
              `(py:let ((,o (@ ,obj ,@(butlast attrs)))
                        (,v (py:new ,store-var)))
                 ,(let ((attr (car (last attrs))))
                    (if (listp attr)
                        `(py.obj:set-item ,o ,(car attr) ,v)
                        `(py.obj:set-attr ,o ,(symbol-name attr) ,v)))
                 ))
            `(error "@ doesn't provide accessing form, because this will break Python's reference counting."))))

(defmacro code (options &body body)
  `(progn
     (unless (py:is-initialized) (py:initialize))
     ,@body
     ,@(when (getf options :finalize t)
         '((free-registry)
           (py:finalize)))))
