(defpackage :clpy.member
  (:nicknames :py.mbr)
  (:use :cl :plus-c)
  (:export #:defgetter
	   #:defsetter
	   #:new-get-set-def
	   #:free-registry))

(in-package :clpy.member)

(defmacro defgetter (name lambda-list &body body)
  "Define a a getter.

The function signature is

.. code-block:: c

   PyObject *getter(PyObject *self, void *closure)``.

It is recommended to use fixed name for the arguments, for example

.. code-block:: common-lisp

   (defcfunction my-getter (self closure)
     (print 'do-some-thing))"
  (when (not (= 2 (length lambda-list)))
    (error "The lambda-list should have exactly two argument."))
  `(autowrap:defcallback ,name clpy.ffi:py-object
       ((,(first lambda-list) clpy.ffi:py-object)
	(,(second lambda-list) :pointer))
     ,@body))

(defmacro defsetter (name lambda-list &body body)
  "Define a a getter.

The function signature is

.. code-block:: c

   PyObject *getter(PyObject *self, PyObject *value, void *closure)``.

It is recommended to use fixed name for the arguments, for example

.. code-block:: common-lisp

   (defcfunction my-setter (self value closure)
     (print 'do-some-thing))"
  (when (not (= 3 (length lambda-list)))
    (error 'clpy.exception:generic-error
	   :message "The lambda-list should have exactly two argument."))
  `(autowrap:defcallback ,name :int
       ((,(first lambda-list) clpy.ffi:py-object)
	(,(second lambda-list) clpy.ffi:py-object)
	(,(third lambda-list) :pointer))
     ,@body))

(defparameter *get-set-def-registry* '())

(defun new-get-set-def (name &key getter setter doc closure)
  (unless getter
    (error "Getter cannot be NIL"))
  (c-let ((get-set-def clpy.ffi:py-get-set-def))
    (setf (get-set-def :name) name
	  (get-set-def :get) getter
	  (get-set-def :set) setter
	  (get-set-def :doc) doc
	  (get-set-def :closure) closure)
    get-set-def))


;; PyMemberDef


(defun free-registry ()
  (dolist (get-set-def *get-set-def-registry*)
    (autowrap:free get-set-def))
  (setf *get-set-def-registry* '()))


