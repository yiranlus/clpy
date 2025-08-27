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
	   #:get-func-desc

	   #:release-thread
	   #:acquire-thread
	   #:save-thread
	   #:restore-thread

	   #:allow-threads))

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
		    (globals nil globals-p) (locals nil locals-p) ; for PyEval_EvalCode
		    args argcount kws kwcount defs defcount kwdefs closure ; for PyEval_EvalCodeEx
		    throwflag)
  "Eval a code.

ARGS should be an C-array of Python object and the size is indicated by
ARGCOUNT. KWS should be a C-arry where the keyword names come first and
then the values. The number of keywords are indicated by KWCOUNT, so
KWCOUNT should be half of the size of KWS. DEFS and DEFCOUNT are for
default values for positional arguments. KWDEFS is a dictionary store
the default values for keyword arguments. CLOSURE is a tuple of
PyCellObject."
  (clpy.util:ensure-null-as-nil
      (if (clpy.frame:p code)
	  (if throwflag
	      (clpy.ffi.fns:py-eval-eval-frame-ex code throwflag)
	      (clpy.ffi.fns:py-eval-eval-frame code))
	  (clpy.util:let ((-globals (if globals-p
					(clpy.object:new-ref globals)
					(clpy.dict:new)))
			  (-locals (if locals-p
				       (clpy.object:new-ref locals)
				       (clpy.dict:new))))
	    (unless globals-p
	      (clpy.dict:set-item -globals "__builtins__" (get-builtins)))
	    (clpy.util:let ((-code (if (stringp code)
				       (compile code)
				       (clpy.object:new-ref code))))
	      (if (or args kws defs kwdefs closure)
		  (clpy.ffi.fns:py-eval-eval-code-ex -code -globals -locals
						     args (or argcount 0)
						     kws (or kwcount 0)
						     defs (or defcount 0)
						     kwdefs
						     closure)
		(clpy.ffi.fns:py-eval-eval-code -code -globals -locals)))))
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

;; thread

(defun release-thread (tstate)
  "Reset the current thread state to NULL and release the GIL.

TSTATE must be created beforehand. You can use :cl:function:`save-thread`
which is a higher-level function."
  (clpy.ffi.fns:py-eval-release-thread tstate))

(defun acquire-thread (tstate)
  "Acquire the GIL and set the current thread state to TSTATE.

TSTATE must be created beforehand. You can use :cl:function:`restore-thread`
which is a higher-level function."
  (clpy.ffi.fns:py-eval-acquire-thread tstate))

(defun save-thread ()
  "Release the global interpreter lock and reset the thread state to NULL."
  (clpy.ffi.fns:py-eval-save-thread))

(defun restore-thread (tstate)
  (clpy.ffi.fns:py-eval-restore-thread tstate))

;; Utility Macros

(defmacro allow-threads (&body body)
  (let ((saved-state (gensym)))
    `(progn
       (let ((,saved-state (save-thread)))
	 (flet ((block-threads ()
		  (restore-thread ,saved-state))
		(unblock-threads ()
		  (setf ,saved-state (save-thread))))
	   ,@body
	   (restore-thread ,saved-state))))))
