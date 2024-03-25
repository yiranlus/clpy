(in-package :clpy)

;; Initializing and finalizing the interpreter

(defun initialize (&optional (initsigs 1))
  "Initialize the Python interpreter. If INITSIGS is set to 0, it skips
initialization registration of signal handlers, which migh be useful when
Python is embedded."
  (check-type initsigs integer)
  (clpy.ffi.fns::py-initialize-ex initsigs))

(defun is-initialized ()
  "Return T when the Python interpreter has been initialized, NIL if not."
  (plusp (clpy.ffi.fns::py-is-initialized)))

(defun finalize ()
  "Undo all initializations and subsequent use of Python/C API functions, and
destroy all sub-interpreters that were created and not yet destroyed since the
last call to INITILIZE. Return t normally, otherwise NIL."
  (zerop (clpy.ffi.fns:py-finalize-ex)))

;; encode and decode based on filesystem encoding.

(defun encode-locale (text)
  "Encode a wide character string to the filesystem encoding."
  (c-with ((error-pos clpy.ffi:size-t))
    (multiple-value-bind (res ptr)
        (clpy.ffi.fns:py-encode-locale text (error-pos &))
      (unless (cffi:null-pointer-p ptr)
        (clpy.ffi.fns:py-mem-free ptr)
        res))))

(defun decode-locale (arg)
  (c-with ((error-pos clpy.ffi:size-t))
    (py:ensure-null-as-nil
      (clpy.ffi.fns:py-decode-locale arg (error-pos &)))))

;; Process-wide parameters

(defun set-program-name (name)
  "Set the program name."
  (clpy.ffi.fns:py-set-program-name
   (if (stringp name) (decode-locale name) name)))

(defun get-program-name ()
  "Return the program name set with SET-PROGRAM-NAME."
  (multiple-value-bind (res ptr)
      (encode-locale (clpy.ffi.fns:py-get-program-name))
    (clpy.ffi.fns:py-mem-free ptr)
    res))

(defun get-prefix ()
  "Return the prefix for installed platform-independent files."
  (multiple-value-bind (res ptr)
      (encode-locale (clpy.ffi.fns:py-get-prefix))
    (clpy.ffi.fns:py-mem-free ptr)
    res))

(defun get-exec-prefix ()
  "Return the exec-prefix for installed platform-dependent files."
  (multiple-value-bind (res ptr)
      (encode-locale (clpy.ffi.fns:py-get-exec-prefix))
    (clpy.ffi.fns:py-mem-free ptr)
    res))

(defun set-path (name)
  "Set the default module search path."
  (clpy.ffi.fns:py-set-path
   (if (stringp name) (decode-locale name) name)))

(defun get-path ()
  "Return the default module search path."
  (multiple-value-bind (res ptr)
      (encode-locale (clpy.ffi.fns:py-get-path))
    (clpy.ffi.fns:py-mem-free ptr)
    res))

(defun get-program-full-path ()
  "Return the full program name of the Python executable."
  (multiple-value-bind (res ptr)
      (encode-locale (clpy.ffi.fns:py-get-program-full-path))
    (clpy.ffi.fns:py-mem-free ptr)
    res))

(defun get-version ()
  "Return the version of this Python interpreter."
  (clpy.ffi.fns:py-get-version))

(defun get-platform ()
  "Return the platform identifier for the current platform."
  (clpy.ffi.fns:py-get-platform))

(defun get-copyright ()
  "Return the official copyright string for the current Python version."
  (clpy.ffi.fns:py-get-copyright))

(defun get-compiler ()
  "Return an indication of the compiler used to build the current Python
version."
  (clpy.ffi.fns:py-get-compiler))

(defun get-build-info ()
  "Return information about the sequence number and build date and time of the
current Python interpreter instance."
  (clpy.ffi.fns:py-get-build-info))

(defun set-python-home (name)
  "Set the default location of the standard python libraries. SEE ``PYTHONHOME''
for the meaning of the argument string."
  (clpy.ffi.fns:py-set-python-home
   (if (stringp name) (decode-locale name) name)))

(defun get-python-home ()
  "Return the program name set with SET-PYTHON-HOME."
  (multiple-value-bind (res ptr)
      (encode-locale (clpy.ffi.fns:py-get-python-home))
    (clpy.ffi.fns:py-mem-free ptr)
    res))
