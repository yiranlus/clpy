(in-package :clpy)

(defun clear-error ()
  "Clear the error indicator. If the error indicator is not set, there is no
effect."
  (clpy.ffi.fns:py-err-clear))

(defun print-error (&optional (set-sys-last-vars 1))
  "Print a standard tracebak to ``sys.stderr'' and clear the error indicator."
  (clpy.ffi.fns:py-err-print-ex set-sys-last-vars))

(defun write-unraisable (obj)
  "Call ``sys.unraisablehook()'' using the current exception and OBJ argument."
  (clpy.ffi.fns:py-err-write-unraisable obj))

;; Only available since Python 3.12
;;(defun display-exception (exc)
;;  (clpy.ffi.fns:py-err-display-exception exc))

;; Raising exceptions

(defun set-content (type &key (message nil message-p) (object nil object-p))
  "Set the error indicator either by a string supplied by MESSAGE or an object.
One and only one of the key parameter should appear."
  (when (xor message-p object-p)
    (error 'simple-error "Only one of :MESSAGE and :OBJECT need to be set."))
  (cond
    (message-p (clpy.ffi.fns:py-err-set-string type message))
    (object-p (clpy.ffi.fns:py-err-set-object type (or object
                                                       (cffi:null-pointer))))))

(defun set-none (type)
  "Shorthand for (SET-CONTENT TYPE NIL)"
  (clpy.ffi.fns:py-err-set-none type))

(defun bad-argument ()
  (clpy.ffi.fns:py-err-bad-argument))

(defun no-memory ()
  "Shorthand for ``PyErr_SetNone(PyExc_MemoryError)''. Always return nil."
  (and (clpy.ffi.fns:py-err-no-memory) nil))

(defun set-from-errno (type &key
                              (filename-obj nil filename-obj-p)
                              (filename-obj2 nil filename-obj2-p)
                              (filename nil filename-p))
  "Convenience function to raise an exception when a C library function has
returned an error and set the C variable errno."
  (cond
    (filename-p (clpy.ffi.fns:py-err-set-from-errno-with-filename type filename))
    (filename-obj2-p (clpy.ffi.fns:py-err-set-from-errno-with-filename-objects
                      type
                      (or filename-obj (cffi:null-pointer))
                      (or filename-obj2 (cffi:null-pointer))))
    (filename-obj-p (clpy.ffi.fns:py-err-set-from-errno-with-filename-object
                     type
                     (or filename-obj (cffi:null-pointer))))
    (t (clpy.ffi.fns:py-err-set-from-errno type))))

(defun set-import-error (msg &key name path subclass)
  "Convenience function to raise ``ImportError''"
  (if subclass
      (clpy.ffi.fns:py-err-set-import-error-subclass
       subclass msg (or name (cffi:null-pointer)) (or path (cffi:null-pointer)))
      (clpy.ffi.fns:py-err-set-import-error msg
                                            (or name (cffi:null-pointer))
                                            (or path (cffi:null-pointer)))))

(defun syntax-location (filename lineno col-offset)
  "Set file, line, and offset information for the current exception."
  (if col-offset
      (clpy.ffi.fns:py-err-syntax-location-ex filename lineno col-offset)
      (clpy.ffi.fns:py-err-syntax-location filename lineno)))

(defun bad-internal-call ()
  (clpy.ffi.fns:py-err-bad-internal-call))

;; Issuing warnings

(defun py-warn (category message stack-level))

;; Query the error indicator

efun error-occurred ()
"Test whether the error indicator is set. If set, return the exception TYPE. You
 not own a reference to the return value, so you do not need to PY:DEC-REF it."
(py:ensure-null-as-nil (clpy.ffi.fns:py-err-occurred)))

(defun exception-matches (exc)
  "Equivalent to ``PyErr_GivenExceptionMatches(PyError_Occurred(), exc)''."
  (clpy.ffi.fns:py-err-exception-matches))

(defun given-exception-matches (given exc)
  "Return T if the GIVEN exception matches the exception type in EXC."
  (clpy.ffi.fns:py-err-given-exception-matches given exc))

;; Only available since Python 3.12
;;(defun get-raised-exception ()
;;  (let ((res (clpy.ffi.fns:py-err-get-raised-exception)))
;;    (unless (cffi:null-pointer-p (autowrap:ptr res))
;;      res)))

;;(defun set-raised-exception (exc)
;;  (clpy.ffi.fns:py-err-set-raised-exception exc)))

;; Signal Handling
