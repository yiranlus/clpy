(cl:in-package :clpy.core)

(defun encode-locale (text)
  "Encode a wide character string to the filesystem encoding."
  (c-with ((error-pos clpy.ffi:size-t))
    (multiple-value-bind (res ptr)
        (clpy.ffi.fns:py-encode-locale text (error-pos &))
      (unless (cffi:null-pointer-p ptr)
        (clpy.ffi.fns:py-mem-free ptr))
        res)))

(defun decode-locale (arg)
  (c-with ((error-pos clpy.ffi:size-t))
    (clpy.ffi.fns:py-decode-locale arg (error-pos &))))

;; PROCESS-WIDE PARAMETERS

(defun set-program-name (name)
  "set the program name."
  (clpy.ffi.fns:py-set-program-name
   (if (stringp name) (decode-locale name) name)))

(defun get-program-name ()
  "Return the program name set with SET-PROGRAM-NAME."
  (cl:let ((ptr (clpy.ffi.fns:py-get-program-name)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-prefix ()
  "Return the prefix for installed platform-independent files."
  (cl:let ((ptr  (clpy.ffi.fns:py-get-prefix)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-exec-prefix ()
  "Return the exec-prefix for installed platform-dependent files."
  (cl:let ((ptr (clpy.ffi.fns:py-get-exec-prefix)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-path ()
  "Return the default module search path."
  (cl:let ((ptr (clpy.ffi.fns:py-get-path)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

(defun get-program-full-path ()
  "Return the full program name of the Python executable."
  (cl:let ((ptr (clpy.ffi.fns:py-get-program-full-path)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))

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

;; Deprecated since version 3.11
(defun set-python-home (name)
  "Set the default location of the standard python libraries. SEE ``PYTHONHOME''
for the meaning of the argument string."
  (clpy.ffi.fns:py-set-python-home
   (if (stringp name) (decode-locale name) name)))

;; Deprecated since version 3.13
;; Use PYTHONHOME instead
(defun get-python-home ()
  "Return the program name set with SET-PYTHON-HOME."
  (cl:let ((ptr (clpy.ffi.fns:py-get-python-home)))
    (unless (cffi:null-pointer-p ptr)
      (encode-locale ptr))))
