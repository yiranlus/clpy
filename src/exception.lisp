(defpackage :clpy.exc
  (:nicknames :py.exc)
  ;;(:use :cl)
  (:export #:python-error
           #:overflow-error
           #:type-error
           #:import-error
           #:system-error
           #:index-error
           #:attribute-error))

(in-package :clpy.exc)

(cl:define-condition python-error (cl:error)
  ((raise :initarg :raise
          :initform cl:nil
          :reader raise)))

(cl:define-condition overflow-error (python-error)
  ())

(cl:define-condition type-error (python-error)
  ())

(cl:define-condition import-error (python-error)
  ())

(cl:define-condition system-error (python-error)
  ())

(cl:define-condition index-error (python-error)
  ())

(cl:define-condition attribute-error (python-error)
  ())

;; Exception types

;; Standard exception
(cffi:defcvar ("PyExc_ArithmeticError" +arithmetic-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_AssertionError" +assertion-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_AttributeError" +attribute-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_BaseException" +base-exception+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_BaseExceptionGroup" +base-exception-group+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_BlockingIOError" +blocking-io-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_BrokenPipeError" +broken-pipe-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_BufferError" +buffererror+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ChildProcessError" +child-process-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ConnectionAbortedError" +connection-abort-ederror+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ConnectionError" +connection-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ConnectionRefusedError" +connection-refused-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ConnectionResetError" +connection-reset-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_EOFError" +eof-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_EnvironmentError" +environment-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_Exception" +exception+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_FileExistsError" +file-exists-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_FileNotFoundError" +file-not-found-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_FloatingPointError" +floating-point-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_GeneratorExit" +generator-exit+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_IOError" +io-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ImportError" +import-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_IndentationError" +indentation-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_IndexError" +index-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_InterruptedError" +interrupted-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_IsADirectoryError" +is-a-directory-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_KeyError" +key-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_KeyboardInterrupt" +keyboard-interrupt+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_LookupError" +lookup-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_MemoryError" +memory-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ModuleNotFoundError" +module-not-found-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_NameError" +name-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_NotADirectoryError" +not-a-directory-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_NotImplementedError" +not-implemented-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_OSError" +os-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_OverflowError" +overflow-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_PermissionError" +permission-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ProcessLookupError" +process-lookup-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_RecursionError" +recursion-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ReferenceError" +reference-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_RuntimeError" +runtime-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_StopAsyncIteration" +stop-async-iteration+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_StopIteration" +stop-iteration+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_SyntaxError" +syntax-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_SystemError" +system-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_SystemExit" +system-exit+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_TabError" +tab-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_TimeoutError" +timeout-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_TypeError" +type-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_UnboundLocalError" +unbound-local-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_UnicodeDecodeError" +unicode-decode-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_UnicodeEncodeError" +unicode-encode-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_UnicodeError" +unicode-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_UnicodeTranslateError" +unicode+translate-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ValueError" +value-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_WindowsError" +windows-error+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ZeroDivisionError" +zero-division-error+ :read-only t) :pointer)

;; Standard warngins
(cffi:defcvar ("PyExc_BytesWarning" +bytes-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_DeprecationWarning" +deprecation-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_EncodingWarning" +encoding-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_FutureWarning" +future-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ImportWarning" +import-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_PendingDeprecationWarning" +pending-deprecation-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_ResourceWarning" +resource-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_RuntimeWarning" +runtime-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_SyntaxWarning" +syntax-warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_UnicodeWarning" +unicode+warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_UserWarning" +user+warning+ :read-only t) :pointer)
(cffi:defcvar ("PyExc_Warning" +warning+ :read-only t) :pointer)
