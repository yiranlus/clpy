(in-package :clpy.error)

(defun fetch ()
  "Retrieve the error indicator.

The retrieved objects will be automatically normalized."
  (c-with ((ptype (:pointer clpy.ffi:py-object))
	   (pvalue (:pointer clpy.ffi:py-object))
	   (ptrackback (:pointer clpy.ffi:py-object)))
    (clpy.ffi.fns:py-err-fetch (ptype &) (pvalue &) (ptrackback &))
    (clpy.ffi.fns:py-err-normalize-exception (ptype &) (pvalue &) (ptrackback &))
    (values (clpy.util:ensure-null-as-nil (ptype *))
	    (clpy.util:ensure-null-as-nil (pvalue *))
	    (clpy.util:ensure-null-as-nil (ptrackback *)))))

(defun restore (type value trackback)
  (clpy.ffi.fns:py-err-restore type value trackback))

(defun fetch-as-string ()
  "Return the string of type, value and trackback of current exception.

This function combine the :cl:function:`fetch` and :cl:function:`restore`
to get the string representation of the current exception."
  (multiple-value-bind (type value traceback) (fetch)
    (when type
      (let ((type-str (clpy.util:let* ((obj-str (clpy.object:str type))
				       (encoded (clpy.str:encode obj-str)))
			(clpy.bytes:as-string encoded)))
	    
	    (value-str (when value
			 (clpy.util:let* ((obj-str (clpy.object:str value))
					  (encoded (clpy.str:encode obj-str)))
			   (clpy.bytes:as-string encoded))))
	    (traceback-str (when traceback
			     (clpy.util:let* ((obj-str (clpy.object:str traceback))
					      (encoded (clpy.str:encode obj-str)))
			       (clpy.bytes:as-string encoded)))))
	(restore type value traceback)
	(values type-str value-str traceback-str)))))

;;(cl:in-package :clpy.error)

;;(defun write-unraisable (obj)
;;  "Call ``sys.unraisablehook()'' using the current exception and OBJ argument."
;;  (clpy.ffi.fns:py-err-write-unraisable obj))
;;
;;(defun display-exception (exc)
;;  (clpy.ffi.fns:py-err-display-exception exc))
;;
;;;; Raising exceptions
;;
;;(defun set (type (content nil))
;;  "Set the error indicator either by a string supplied by CONTENT or an object."
;;  (cond
;;    ((null content) (clpy.ffi.fns:py-err-set-none type))
;;    ((stringp content) (clpy.ffi.fns:py-err-set-string type message))
;;    (otherwise (clpy.ffi.fns:py-err-set-object type content))))
;;
;;(defun bad-argument ()
;;  (clpy.ffi.fns:py-err-bad-argument))
;;
;;(defun no-memory ()
;;  "Shorthand for ``PyErr_SetNone(PyExc_MemoryError)''"
;;  (clpy.ffi.fns:py-err-no-memory))
;;
;;(defun set-from-errno (type &key
;;                              (filename-obj nil filename-obj-p)
;;                              (filename-obj2 nil filename-obj2-p)
;;                              (filename nil filename-p))
;;  "Convenience function to raise an exception when a C library function has
;;returned an error and set the C variable errno."
;;  (cond
;;    (filename-p (clpy.ffi.fns:py-err-set-from-errno-with-filename type filename))
;;    (filename-obj2-p (clpy.ffi.fns:py-err-set-from-errno-with-filename-objects
;;                      type
;;                      (or filename-obj (cffi:null-pointer))
;;                      (or filename-obj2 (cffi:null-pointer))))
;;    (filename-obj-p (clpy.ffi.fns:py-err-set-from-errno-with-filename-object
;;                     type
;;                     (or filename-obj (cffi:null-pointer))))
;;    (t (clpy.ffi.fns:py-err-set-from-errno type))))
;;
;;(defun set-windows-err ())
;;
;;(defun set-import-error (msg &key name path subclass)
;;  "Convenience function to raise ``ImportError''"
;;  (if subclass
;;      (clpy.ffi.fns:py-err-set-import-error-subclass
;;       subclass msg (or name (cffi:null-pointer)) (or path (cffi:null-pointer)))
;;      (clpy.ffi.fns:py-err-set-import-error msg
;;                                            (or name (cffi:null-pointer))
;;                                            (or path (cffi:null-pointer)))))
;;
;;(defun syntax-location (filename lineno col-offset)
;;  "Set file, line, and offset information for the current exception."
;;  (if col-offset
;;      (clpy.ffi.fns:py-err-syntax-location-ex filename lineno col-offset)
;;      (clpy.ffi.fns:py-err-syntax-location filename lineno)))
;;
;;(defun bad-internal-call ()
;;  (clpy.ffi.fns:py-err-bad-internal-call))
;;
;;;; Issuing warnings
;;
;;(defun py-warn (category message stack-level))
;;
;;;; Query the error indicator
;;
;;
;;(defun exception-matches (exc)
;;  "Equivalent to ``PyErr_GivenExceptionMatches(PyError_Occurred(), exc)''."
;;  (clpy.ffi.fns:py-err-exception-matches))
;;
;;(defun given-exception-matches (given exc)
;;  "Return T if the GIVEN exception matches the exception type in EXC."
;;  (clpy.ffi.fns:py-err-given-exception-matches given exc))
;;
;;;; Only available since Python 3.12
;;;;(defun get-raised-exception ()
;;;;  (let ((res (clpy.ffi.fns:py-err-get-raised-exception)))
;;;;    (unless (cffi:null-pointer-p (autowrap:ptr res))
;;;;      res)))
;;
;;;;(defun set-raised-exception (exc)
;;;;  (clpy.ffi.fns:py-err-set-raised-exception exc)))
;;
;;;; Signal Handling
