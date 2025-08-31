(defpackage :clpy.interpreter
  (:nicknames :py.inter)
  (:use :cl :plus-c)
  (:export #:initialize
           #:is-initialized
           #:finalize
           ;; runtime
           #:encode-locale
           #:decode-locale
           #:set-program-name
           #:get-program-name
           #:get-prefix
           #:get-exec-prefix
           #:get-path
           #:get-program-full-path
           #:get-version
           #:get-platform
           #:get-copyright
           #:get-compiler
           #:get-build-info
           #:set-python-home
           #:get-python-home
           #:get-recursion-limit

           #:enter-recursive-call
           #:leave-recursive-call
           #:repr-enter
           #:repr-leave

           #:state-new
           #:state-get
           #:state-get-id
           #:state-get-dict
           #:state-clear
           #:state-delete

           #:fatal-error
           #:main
           #:exit
           #:at-exit

           #:new-interpreter
           #:end-interpreter

           #:add-pending-call
           #:make-panding-calls))

(in-package :clpy.interpreter)

;; Initializing and finalizing the interpreter

(defun initialize (&optional (initsigs t))
  "Initialize the Python interpreter. If INITSIGS is set to NIL, it
skips initialization registration of signal handlers, which migh be
useful when Python is embedded."
  (let ((-initsigs (if initsigs 1 0)))  
    (clpy.ffi.fns:py-initialize-ex -initsigs)))

(defun is-initialized ()
  "Return T when the Python interpreter has been initialized, NIL if
not."
  (plusp (clpy.ffi.fns:py-is-initialized)))

(defun finalize ()
  "Undo all initializations and subsequent use of Python/C API
functions, and destroy all sub-interpreters that were created and not
yet destroyed since the last call to INITILIZE. Return t normally,
otherwise NIL."
  (zerop (clpy.ffi.fns:py-finalize-ex)))

;; Encode and decode wide characters

(defun encode-locale (text)
  "Encode a wide character string to the filesystem encoding."
  (c-with ((error-pos clpy.ffi:size-t))
    (multiple-value-bind (res ptr)
        (clpy.ffi.fns:py-encode-locale text (error-pos &))
      (unless (cffi:null-pointer-p ptr)
        (clpy.ffi.fns:py-mem-free ptr))
      res)))

(defun decode-locale (arg)
  "Reverse of :cl:function:`encode-locale`"
  (c-with ((error-pos clpy.ffi:size-t))
    (clpy.ffi.fns:py-decode-locale arg (error-pos &))))

;; Process-wide parameters

(defun set-program-name (name)
  "Set the program name.

Deprecated since Python 3.11."
  (warn "SET-PROGRAM-NAME: Deprecated since Python 3.11")
  (clpy.ffi.fns:py-set-program-name
   (if (stringp name) (decode-locale name) name)))

(defun get-program-name ()
  "Return the program name set with SET-PROGRAM-NAME."
  (let ((ptr (clpy.ffi.fns:py-get-program-name)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-prefix ()
  "Return the prefix for installed platform-independent files."
  (let ((ptr  (clpy.ffi.fns:py-get-prefix)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-exec-prefix ()
  "Return the exec-prefix for installed platform-dependent files."
  (let ((ptr (clpy.ffi.fns:py-get-exec-prefix)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-path ()
  "Return the default module search path."
  (let ((ptr (clpy.ffi.fns:py-get-path)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-program-full-path ()
  "Return the full program name of the Python executable."
  (let ((ptr (clpy.ffi.fns:py-get-program-full-path)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-version ()
  "Return the version of this Python interpreter."
  (clpy.ffi.fns:py-get-version))

(defun get-platform ()
  "Return the platform identifier for the current platform."
  (clpy.ffi.fns:py-get-platform))

(defun get-copyright ()
  "Return the official copyright string for the current Python
version."
  (clpy.ffi.fns:py-get-copyright))

(defun get-compiler ()
  "Return an indication of the compiler used to build the current
Python version."
  (clpy.ffi.fns:py-get-compiler))

(defun get-build-info ()
  "Return information about the sequence number and build date and time
of the current Python interpreter instance."
  (clpy.ffi.fns:py-get-build-info))

(defun get-recursion-limit ()
  "Return the recursion limit."
  (clpy.ffi.fns:py-get-recursion-limit))

(defun set-recursion-limit (n)
  "Set the recursion limit."
  (clpy.ffi.fns:py-set-recursion-limit n))

;; Recursive Control

(defun enter-recursive-call (where)
  "Marks a point where a recursive C-level call is about to be performed."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-enter-recursive-call where)
    (clpy.exception:raise-generic-or-python-error)))

(defun leave-recursive-call ()
  (clpy.ffi.fns:py-leave-recursive-call))


(defun repr-enter (obj)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-repr-enter obj)
    (clpy.exception:raise-generic-or-python-error)))

(defun repr-leave (obj)
  (clpy.ffi.fns:py-repr-leave))

;; Interpreter State

(defun state-new ()
  (clpy.ffi.fns:py-interpreter-state-new))

(defun state-get ()
  (clpy.ffi.fns:py-interpreter-state-get))

(defun state-get-id (interp)
  (clpy.ffi.fns:py-interpreter-state-get-id interp))

(defun state-get-dict (interp)
  (clpy.ffi.fns:py-interpreter-state-get-dict interp))

(defun state-clear (interp)
  (clpy.ffi.fns:py-interpreter-state-clear interp))

(defun state-delete (interp)
  (clpy.ffi.fns:py-interpreter-state-delete interp))

;; Process Control

(defun main (argv)
  (error "Not implemented yet."))

(defun fatal-error (message)
  (clpy.ffi.fns:py-fatal-error message))

(defun exit (status)
  (clpy.ffi.fns:py-exit status))

(defun at-exit (func)
  "Call a function at exit.

The function should be a void function without argument."
  (clpy.ffi.fns:py-at-exit func))

;; Sub-interpreter

(defun new-interpreter ()
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-new-interpreter)
    (error 'clpy.exception:generic-error
           :message "Failed to create a sub-interpreter.")))

(defun end-interpreter (tstate)
  "Destroy the (sub-)interpreter reprensted by the given thread state."
  (clpy.ffi.fns:py-end-interpreter tstate))

;; Schedule function call

(defun add-pending-call (func arg)
  "Schedule a function to be called later.

FUNC should have signature ``int (void *)`` and ARG is a ``void*``."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-add-pending-call func arg)
    (clpy.exception:raise-generic-or-python-error)))

(defun make-pending-calls ()
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-make-pending-calls)
    (clpy.exception:raise-generic-or-python-error)))
