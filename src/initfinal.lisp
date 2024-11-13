(cl:in-package :clpy.core)

;; Initializing and finalizing the interpreter

(defun initialize (&optional (initsigs t))
  "Initialize the Python interpreter. If INITSIGS is set to NIL, it skips initialization registration of signal handlers, which migh be useful when Python is embedded."
  (cl:let ((-initsigs (if initsigs 1 0)))  
    (clpy.ffi.fns:py-initialize-ex -initsigs)))

(defun is-initialized ()
  "Return T when the Python interpreter has been initialized, NIL if not."
  (plusp (clpy.ffi.fns:py-is-initialized)))

(defun finalize ()
  "Undo all initializations and subsequent use of Python/C API functions, and destroy all sub-interpreters that were created and not yet destroyed since the last call to INITILIZE. Return t normally, otherwise NIL."
  (zerop (clpy.ffi.fns:py-finalize-ex)))
