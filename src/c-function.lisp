(defpackage :clpy.c-function
  (:nicknames :py.c-func)
  (:use :cl :plus-c)
  (:export #:defcfunction
	   #:defcfunction-with-keywords
	   #:new-method-def
	   #:new
	   #:free-registry))

(in-package :clpy.c-function)

(clpy.type:define-type "PyCFunction_Type" c-function)

(defmacro defcfunction (name lambda-list &body body)
  "Define a PyCFunction.

The function signature is

.. code-block:: c

   PyObject *PyCFunction(PyObject *self, PyObject *args)``.

It is recommended to use fixed name for the arguments, for example

.. code-block:: common-lisp

   (defcfunction my-cfunction (self args)
     (print 'do-some-thing))"
  (when (not (= 2 (length lambda-list)))
    (error "The lambda-list should have exactly two argument."))
  `(autowrap:defcallback ,name clpy.ffi:py-object
       ((,(first lambda-list) clpy.ffi:py-object)
	(,(second lambda-list) clpy.ffi:py-object))
     ,@body))


(defmacro defcfunction-with-keywords (name lambda-list &body body)
  "Define a PyCFunction.

The function signature is

.. code-block:: c

   PyObject *PyCFunction(PyObject *self, PyObject *args,
                         PyObject *kwargs)``.

It is recommended to use fixed name for the arguments, for example

.. code-block:: common-lisp

   (defcfunction my-cfunction (self args kwargs)
     (print 'do-some-thing))"
  (when (not (= 2 (length lambda-list)))
    (error "The lambda-list should have exactly one argument."))
  `(autowrap:defcallback ,name clpy.ffi:py-object
       ((,(first lambda-list) clpy.ffi:py-object)
	(,(second lambda-list) clpy.ffi:py-object))
     ,@body))


(defun make-flags (&rest flags)
  (labels ((interp (flag)
             (if (integerp flag)
                 flag
                 (case flag
                   (:var-args       #x0001)
                   (:keywords       #x0002)
                   (:no-args        #x0004)
                   (:o              #x0008)
		   (:class          #x0010)
		   (:static         #x0020)
		   (:coexist        #x0040)
		   (:fast-call      #x0080)
		   (:stackless      #x0100) ;; unsupported
		   (:method         #x0200)
                   (otherwise (error "Unsuppoerted Py_Buffer flags."))))))
    (reduce #'(lambda (a b)
                (logior (interp a) (interp b)))
            flags :initial-value #x0000)))

(defvar *method-def-registry* '())

(defun new-method-def (name method doc &rest flags)
  (c-let ((method-def clpy.ffi:py-method-def))
    (setf (method-def :ml-name) name
	  (method-def :ml-meth) method
	  (method-def :ml-flags) (apply #'make-flags flags)
	  (method-def :ml-doc) doc)
    (push method-def *method-def-registry*)
    method-def))

(defun free-registry ()
  (dolist (method-def *method-def-registry*)
    (autowrap:free method-def))
  (setf *method-def-registry* '()))

(defun new (ml self &optional module cls)
  "Turn ML into a callable object."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-c-method-new (ml self module cls))
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to create the C method.")))
  
