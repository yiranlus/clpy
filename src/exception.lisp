(defpackage :clpy.exc
  (:nicknames :py.exc)
  (:export #:generic-error
	   #:python-error
	   #:type
	   #:value
	   #:*assoc-excs*
	   #:from
	   #:get))
	   

(in-package :clpy.exc)

(cl:define-condition generic-error (cl:error)
  ())

(cl:define-condition python-error (cl:error)
  ((type :initarg :type
         :initform cl:nil
         :reader type)
   (value :initarg :value
	 :initform cl:nil
	 :reader value)))
;; Exception types

(cl:defparameter *assoc-excs* '())

(cl:defun from (py-exc)
  "Return the corresponding keyword to the Python exception."
  (cl:when py-exc
    (cl:car (cl:rassoc py-exc *assoc-excs* :test #'cffi:pointer-eq))))

(cl:defun get (kw)
  (cl:cdr (cl:assoc kw *assoc-excs*)))

(cl:defmacro defcvar (cname name)
  (cl:let* ((name-str (cl:symbol-name name))
	    (const-sym (cl:intern (cl:format cl:nil "+~A+" name-str)))
	    (kw-sym (cl:intern name-str :keyword)))
    `(cl:progn
       (cffi:defcvar (,cname ,const-sym :read-only cl:t) :pointer)
       (cl:push (cl:cons ,kw-sym ,const-sym) *assoc-excs*))))

;; Standard exception
(defcvar "PyExc_Exception" exception)
(defcvar "PyExc_BaseException" base-exception)
(defcvar "PyExc_BaseExceptionGroup" base-exception-group)

(defcvar "PyExc_ArithmeticError" arithmetic-error)
(defcvar "PyExc_AssertionError" assertion-error)
(defcvar "PyExc_AttributeError" attribute-error)
(defcvar "PyExc_BlockingIOError" blocking-io-error)
(defcvar "PyExc_BrokenPipeError" broken-pipe-error)
(defcvar "PyExc_BufferError" buffererror)
(defcvar "PyExc_ChildProcessError" child-process-error)
(defcvar "PyExc_ConnectionAbortedError" connection-abort-ederror)
(defcvar "PyExc_ConnectionError" connection-error)
(defcvar "PyExc_ConnectionRefusedError" connection-refused-error)
(defcvar "PyExc_ConnectionResetError" connection-reset-error)
(defcvar "PyExc_EOFError" eof-error)
(defcvar "PyExc_EnvironmentError" environment-error)
(defcvar "PyExc_FileExistsError" file-exists-error)
(defcvar "PyExc_FileNotFoundError" file-not-found-error)
(defcvar "PyExc_FloatingPointError" floating-point-error)
(defcvar "PyExc_GeneratorExit" generator-exit)
(defcvar "PyExc_IOError" io-error)
(defcvar "PyExc_ImportError" import-error)
(defcvar "PyExc_IndentationError" indentation-error)
(defcvar "PyExc_IndexError" index-error)
(defcvar "PyExc_InterruptedError" interrupted-error)
(defcvar "PyExc_IsADirectoryError" is-a-directory-error)
(defcvar "PyExc_KeyError" key-error)
(defcvar "PyExc_KeyboardInterrupt" keyboard-interrupt)
(defcvar "PyExc_LookupError" lookup-error)
(defcvar "PyExc_MemoryError" memory-error)
(defcvar "PyExc_ModuleNotFoundError" module-not-found-error)
(defcvar "PyExc_NameError" name-error)
(defcvar "PyExc_NotADirectoryError" not-a-directory-error)
(defcvar "PyExc_NotImplementedError" not-implemented-error)
(defcvar "PyExc_OSError" os-error)
(defcvar "PyExc_OverflowError" overflow-error)
(defcvar "PyExc_PermissionError" permission-error)
(defcvar "PyExc_ProcessLookupError" process-lookup-error)
(defcvar "PyExc_RecursionError" recursion-error)
(defcvar "PyExc_ReferenceError" reference-error)
(defcvar "PyExc_RuntimeError" runtime-error)
(defcvar "PyExc_StopAsyncIteration" stop-async-iteration)
(defcvar "PyExc_StopIteration" stop-iteration)
(defcvar "PyExc_SyntaxError" syntax-error)
(defcvar "PyExc_SystemError" system-error)
(defcvar "PyExc_SystemExit" system-exit)
(defcvar "PyExc_TabError" tab-error)
(defcvar "PyExc_TimeoutError" timeout-error)
(defcvar "PyExc_TypeError" type-error)
(defcvar "PyExc_UnboundLocalError" unbound-local-error)
(defcvar "PyExc_UnicodeDecodeError" unicode-decode-error)
(defcvar "PyExc_UnicodeEncodeError" unicode-encode-error)
(defcvar "PyExc_UnicodeError" unicode-error)
(defcvar "PyExc_UnicodeTranslateError" unicode+translate-error)
(defcvar "PyExc_ValueError" value-error)
(defcvar "PyExc_WindowsError" windows-error)
(defcvar "PyExc_ZeroDivisionError" zero-division-error)

;; Standard warngins
(defcvar "PyExc_BytesWarning" bytes-warning)
(defcvar "PyExc_DeprecationWarning" deprecation-warning)
(defcvar "PyExc_EncodingWarning" encoding-warning)
(defcvar "PyExc_FutureWarning" future-warning)
(defcvar "PyExc_ImportWarning" import-warning)
(defcvar "PyExc_PendingDeprecationWarning" pending-deprecation-warning)
(defcvar "PyExc_ResourceWarning" resource-warning)
(defcvar "PyExc_RuntimeWarning" runtime-warning)
(defcvar "PyExc_SyntaxWarning" syntax-warning)
(defcvar "PyExc_UnicodeWarning" unicode-warning)
(defcvar "PyExc_UserWarning" user-warning)
(defcvar "PyExc_Warning" warning)



