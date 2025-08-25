(defpackage :clpy.eval
  (:nicknames :py.eval)
  (:use :cl)
  (:shadow #:compile #:eval)
  (:export #:compile
	   #:eval

	   #:get-builtins
	   #:get-locals
	   #:get-globals
	   #:get-frame
	   #:get-func-name
	   #:get-func-desc))

(in-package :clpy.eval)

(defun compile (code &optional (filename "<string>") (start :eval))
  "Return the code object compiled from a string CODE.

FILENAME can be NIL; in this case, a temporary filename will be given."
  (clpy.util:ensure-null-as-nil
      (let ((-start (case start
		      (:single 256) ; #define Py_single_input 256
		      (:file 257)   ; #define Py_file_input 257
		      (:eval 258)   ; #define Py_eval_input 258
		      )))
	(clpy.ffi.fns:py-compile-string code filename -start))
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to compile the code")))


(defun eval (code &key
		  (globals nil globals-p)
		  (locals nil locals-p))
  (clpy.util:ensure-null-as-nil
      (clpy.util:let ((-globals (if globals-p
				    (clpy.object:new-ref globals)
				    (clpy.dict:new)))
		      (-locals (if locals-p
				   (clpy.object:new-ref locals)
				   (clpy.dict:new))))
	(unless globals-p
	  (clpy.dict:set-item -globals "__builtins__" (get-builtins)))
	(if (stringp code)
	    (clpy.util:let ((-code (compile code)))
	      (clpy.ffi.fns:py-eval-eval-code -code -globals -locals))
	    (clpy.ffi.fns:py-eval-eval-code code -globals -locals)))
    (clpy.exception:raise-generic-or-python-error
     :message (format nil "Unable to eval the code"))))

(defun get-builtins ()
  (clpy.ffi.fns:py-eval-get-builtins))

(defun get-locals ()
  (clpy.ffi.fns:py-eval-get-locals))

(defun get-globals ()
  (clpy.ffi.fns:py-eval-get-globals))

(defun get-frame ()
  (clpy.ffi.fns:py-eval-get-frame))

(defun get-func-name (func)
  "Return the name of FUNC if it is a function, class or instance object, else the name of FUNC's type."
  (clpy.ffi.fns:py-eval-get-func-name func))

(defun get-func-desc (func)
  "Return a description string, depending on the type of FUNC."
  (clpy.ffi.fns:py-eval-get-func-desc func))

