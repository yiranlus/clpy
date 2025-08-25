(defpackage :clpy.error
  (:nicknames :py.err)
  (:use :cl :plus-c)
  (:export #:fetch
	   #:fetch-as-string
	   #:restore))

(defpackage :clpy.exception
  (:nicknames :py.exc)
  (:use :cl)
  (:shadow #:type #:get)
  (:export #:error-occurred
	   #:clear-error
	   #:print-error
	   #:return-or-raise-python-error
	   #:raise-generic-or-python-error
	   #:generic-error
	   #:python-error
	   #:type
	   #:value
	   #:*assoc-excs*
	   #:from
	   #:get))

(in-package :clpy.exception)

(defun error-occurred ()
  "Test whether the error indicator is set.

If set, return the exception TYPE. You not own a reference to the
return value, so you do not need to PY:DEC-REF it."
  (from (clpy.util:ensure-null-as-nil
	    (clpy.ffi.fns:py-err-occurred))))


(defun clear-error ()
  "Clear the error indicator.

If the error indicator is not set, there is no effect."
  (clpy.ffi.fns:py-err-clear))

(defun -with-stderr-captured (thunk)
  "Run THUNK (a function of no args) with stderr redirected to a pipe.
Returns two values: the result of THUNK and the captured stderr as a string."
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    ;; Save original stderr
    (let ((saved-fd (sb-posix:dup 2)))
      (unwind-protect
           (progn
             ;; Redirect stderr to our pipe
             (sb-posix:dup2 write-fd 2)
             (sb-posix:close write-fd)
             ;; Create a Lisp stream for reading from the pipe
             (let ((stream (sb-sys:make-fd-stream read-fd :input t :element-type 'character)))
               ;; Run the user code
               (funcall thunk)
               ;; Close write end so read sees EOF
               (sb-posix:close 2)
               ;; Read captured output
               (with-output-to-string (out)
                 (loop for line = (read-line stream nil nil)
                       while line
                       do (format out "~%~A~%" line)))))
        ;; Always restore stderr
        (ignore-errors
         (sb-posix:dup2 saved-fd 2)
         (sb-posix:close saved-fd))))))

(defmacro with-stderr-captured (&body body)
  `(-with-stderr-captured (lambda () ,@body)))


(defun print-error (&optional (set-sys-last-vars 1) &rest rest &key (capture nil))
  "Print a standard tracebak to ``sys.stderr`` and clear error indicator.

Call this function only when the error indicator is set. Otherwise it
will case a fatal error. If CAPTURE is ``T``, the printed error will
be return as a string."
  (if capture
      (with-stderr-captured
	(clpy.ffi.fns:py-err-print-ex set-sys-last-vars))
      (clpy.ffi.fns:py-err-print-ex set-sys-last-vars)))


(defun return-or-raise-python-error (v)
  "Return the value if no Python error occurs.

If :cl:function:`error-occurred` is ``T``, this function will raise
an error; otherwise, ``V`` is return."
  (let ((exc (error-occurred)))
    (if (cffi:null-pointer-p (autowrap:ptr exc))
	(cl:error 'python-error :type (from exc))
	v)))


(defun raise-generic-or-python-error (&key message)
  "Raise a generic error or a Python error.

If :cl:function:`error-occurred` return NULL, a GENERIC-ERROR will be
raised; Otherwise, a Python error will be raised."
  (let ((exc (error-occurred)))
    (if exc
	(error 'python-error :type exc :message message)
	(error 'generic-error :message message))))


(define-condition generic-error (error)
  ((message :initarg :message
	    :initform nil
	    :reader message))
  (:report (lambda (condition stream)
	     (format stream "GENERIC-ERROR: ~A" (message condition)))))

(define-condition python-error (error)
  ((type :initarg :type
         :initform nil
         :reader type)
  (value :initarg :message
	 :initform nil
	 :reader message))
  (:report (lambda (condition stream)
	     (format stream "~A: ~A~%" (type condition) (message condition))
	     (format stream "~A~%" (print-error 1 :capture t)))))

;; Exception types
(defparameter *assoc-excs* '()
  "An association list store the keywords and corresponding Python exceptions")

(defun from (py-exc)
  "Return the corresponding keyword to the Python exception PY-EXC."
  (when py-exc
    (let ((-py-exc (if (cffi:pointerp py-exc)
		       py-exc
		       (autowrap:ptr py-exc))))
      (car (rassoc -py-exc *assoc-excs* :test #'cffi:pointer-eq)))))

(defun get (kw)
  "Return the corresponding Python exception to the keyword KW.

For example, (GET :IO-ERROR) will return `PyExc_IOError`. This function
is the reverse of :cl:function:`from`."
  (cdr (assoc kw *assoc-excs*)))

(defmacro define-exception (cname name)
  (let* ((name-str (symbol-name name))
	 (const-sym (intern (format nil "+~A+" name-str)))
	 (kw-sym (intern name-str :keyword)))
    `(progn
       (cffi:defcvar (,cname ,const-sym :read-only t) :pointer)
       (push (cons ,kw-sym ,const-sym) *assoc-excs*))))

;; Standard exception
(define-exception "PyExc_Exception" exception)
(define-exception "PyExc_BaseException" base-exception)
(define-exception "PyExc_BaseExceptionGroup" base-exception-group)

;; Standard error
(define-exception "PyExc_ArithmeticError" arithmetic-error)
(define-exception "PyExc_AssertionError" assertion-error)
(define-exception "PyExc_AttributeError" attribute-error)
(define-exception "PyExc_BlockingIOError" blocking-io-error)
(define-exception "PyExc_BrokenPipeError" broken-pipe-error)
(define-exception "PyExc_BufferError" buffererror)
(define-exception "PyExc_ChildProcessError" child-process-error)
(define-exception "PyExc_ConnectionAbortedError" connection-abort-ederror)
(define-exception "PyExc_ConnectionError" connection-error)
(define-exception "PyExc_ConnectionRefusedError" connection-refused-error)
(define-exception "PyExc_ConnectionResetError" connection-reset-error)
(define-exception "PyExc_EOFError" eof-error)
(define-exception "PyExc_EnvironmentError" environment-error)
(define-exception "PyExc_FileExistsError" file-exists-error)
(define-exception "PyExc_FileNotFoundError" file-not-found-error)
(define-exception "PyExc_FloatingPointError" floating-point-error)
(define-exception "PyExc_GeneratorExit" generator-exit)
(define-exception "PyExc_IOError" io-error)
(define-exception "PyExc_ImportError" import-error)
(define-exception "PyExc_IndentationError" indentation-error)
(define-exception "PyExc_IndexError" index-error)
(define-exception "PyExc_InterruptedError" interrupted-error)
(define-exception "PyExc_IsADirectoryError" is-a-directory-error)
(define-exception "PyExc_KeyError" key-error)
(define-exception "PyExc_KeyboardInterrupt" keyboard-interrupt)
(define-exception "PyExc_LookupError" lookup-error)
(define-exception "PyExc_MemoryError" memory-error)
(define-exception "PyExc_ModuleNotFoundError" module-not-found-error)
(define-exception "PyExc_NameError" name-error)
(define-exception "PyExc_NotADirectoryError" not-a-directory-error)
(define-exception "PyExc_NotImplementedError" not-implemented-error)
(define-exception "PyExc_OSError" os-error)
(define-exception "PyExc_OverflowError" overflow-error)
(define-exception "PyExc_PermissionError" permission-error)
(define-exception "PyExc_ProcessLookupError" process-lookup-error)
(define-exception "PyExc_RecursionError" recursion-error)
(define-exception "PyExc_ReferenceError" reference-error)
(define-exception "PyExc_RuntimeError" runtime-error)
(define-exception "PyExc_StopAsyncIteration" stop-async-iteration)
(define-exception "PyExc_StopIteration" stop-iteration)
(define-exception "PyExc_SyntaxError" syntax-error)
(define-exception "PyExc_SystemError" system-error)
(define-exception "PyExc_SystemExit" system-exit)
(define-exception "PyExc_TabError" tab-error)
(define-exception "PyExc_TimeoutError" timeout-error)
(define-exception "PyExc_TypeError" type-error)
(define-exception "PyExc_UnboundLocalError" unbound-local-error)
(define-exception "PyExc_UnicodeDecodeError" unicode-decode-error)
(define-exception "PyExc_UnicodeEncodeError" unicode-encode-error)
(define-exception "PyExc_UnicodeError" unicode-error)
(define-exception "PyExc_UnicodeTranslateError" unicode+translate-error)
(define-exception "PyExc_ValueError" value-error)
#+win32
(define-exception "PyExc_WindowsError" windows-error)
(define-exception "PyExc_ZeroDivisionError" zero-division-error)

;; Standard warngins
(define-exception "PyExc_BytesWarning" bytes-warning)
(define-exception "PyExc_DeprecationWarning" deprecation-warning)
(define-exception "PyExc_EncodingWarning" encoding-warning)
(define-exception "PyExc_FutureWarning" future-warning)
(define-exception "PyExc_ImportWarning" import-warning)
(define-exception "PyExc_PendingDeprecationWarning" pending-deprecation-warning)
(define-exception "PyExc_ResourceWarning" resource-warning)
(define-exception "PyExc_RuntimeWarning" runtime-warning)
(define-exception "PyExc_SyntaxWarning" syntax-warning)
(define-exception "PyExc_UnicodeWarning" unicode-warning)
(define-exception "PyExc_UserWarning" user-warning)
(define-exception "PyExc_Warning" warning)

