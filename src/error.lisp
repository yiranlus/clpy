(in-package :clpy.error)

(defun occurred ()
  "Test whether the error indicator is set.

If set, return the exception TYPE. You not own a reference to the
return value, so you do not need to :cl:function:`py:dec-ref` it."
  (clpy.exception:from (clpy.util:ensure-null-as-nil
                        (clpy.ffi.fns:py-err-occurred))))


(defun print (&optional (set-sys-last-vars 1) &rest rest &key (capture nil))
  "Print a standard tracebak to ``sys.stderr`` and clear error indicator.

Call this function only when the error indicator is set. Otherwise it
will case a fatal error. If CAPTURE is ``T``, the printed error will
be return as a string."
  (if capture
      (with-stderr-captured
          (clpy.ffi.fns:py-err-print-ex set-sys-last-vars))
      (clpy.ffi.fns:py-err-print-ex set-sys-last-vars)))


(defun clear ()
  "Clear the error indicator.

If the error indicator is not set, there is no effect."
  (clpy.ffi.fns:py-err-clear))


(defun write-unraisable (obj)
  "Call ``sys.unraisablehook()`` using the current exception and OBJ argument."
  (clpy.ffi.fns:py-err-write-unraisable obj))

;;(defun display-exception (exc)
;;  (clpy.ffi.fns:py-err-display-exception exc))

;;Raising exceptions

(defun set (type &optional (message nil))
  "Set the error indicator either by a string supplied by CONTENT or an object."
  (let ((-type (if (keywordp type)
                   (plus-c:c-ref (clpy.exception:get :type)
                                 clpy.ffi:py-object)
                   type)))
    (cond
      ((null message) (clpy.ffi.fns:py-err-set-none -type))
      ((stringp message) (clpy.ffi.fns:py-err-set-string -type message))
      (t (clpy.ffi.fns:py-err-set-object -type message)))))

(defun set-from-errno (type &optional
                              (filename nil filename-p)
                              (filename2 nil filename2-p))
  "Convenience function to raise an exception when a C library function has
returned an error and set the C variable errno."
  (let ((-type (if (keywordp type)
                   (plus-c:c-ref (clpy.exception:get :type)
                                 clpy.ffi:py-object)
                   type)))
    (cond
      (filename2-p (clpy.util:let ((-filename (if (clpy.object:p filename)
                                                  (clpy.object:new-ref filename)
                                                  (clpy.str:new filename)))
                                   (-filename2 (if (clpy.object:p filename2)
                                                   (clpy.object:new-ref filename2)
                                                   (clpy.str:new filename2))))
                     (clpy.ffi.fns:py-err-set-from-errno-with-filename-objects
                      -type -filename -filename2)))
      (filename-p (if (stringp filename)
                      (clpy.ffi.fns:py-err-set-from-errno-with-filename -type filename)
                      (clpy.ffi.fns:py-err-set-from-errno-with-filename-object -type filename)))
      (t (clpy.ffi.fns:py-err-set-from-errno -type)))))


(defun set-import-error (msg &key name path subclass)
  "Convenience function to raise ``ImportError''"
  (if subclass
      (clpy.ffi.fns:py-err-set-import-error-subclass subclass msg name path)
      (clpy.ffi.fns:py-err-set-import-error msg name path)))

(defun syntax-location (filename lineno col-offset)
  "Set file, line, and offset information for the current exception."
  (if col-offset
      (clpy.ffi.fns:py-err-syntax-location-ex filename lineno col-offset)
      (clpy.ffi.fns:py-err-syntax-location filename lineno)))

;;Issuing warnings

(defun warn (message &key
                       category
                       (stack-level 1)
                       (filename nil filename-p) lineno module registry)
  (clpy.util:ensure-zero
   (if (filename-p)
       (if lineno
           (clpy.ffi.fns:py-err-warn-explicit category message filename lineno module registry)
           (error 'clpy.exception:generic-error "Need LINENO for CLPY.ERR:WARN"))
       (clpy.ffi.fns:py-err-warn-ex category message stack-level))
   (clpy.exception:raise-generic-or-python-error
    :message "Error encountered when issuing warning.")))

;;Query the error indicator

(defun exception-matches (exc &optional given)
  "Check if the exception mathces certain type.

This function should be called when an exception is actually set;
otherwise, a memory access violation will occur. If GIVEN is not NIL,
this function will check the given exception instead of the raised
one."
  (plusp
   (if given
       (clpy.ffi.fns:py-err-given-exception-matches given exc)
       (clpy.ffi.fns:py-err-exception-matches exc))))


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

(defun get-handled-exception ()
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-err-get-handled-exception)))

(defun set-handled-exception (exc)
  "Set the active exception.

To clear the exception, pass NIL."
  (clpy.ffi.fns:py-err-set-handled-exception exc))

;; Signal Handling

(defun check-signals ()
  (clpy.util:ensure-zero
   (clpy.ffi.fns:py-err-check-signals)
   (clpy.exception:raise-generic-or-python-error
    :message "Error encountered when calling CHECK-SIGNALS.")))

(defun set-interrupt (&optional signum)
  "Simulate the effect of a signal arriving.

Without SIGNUM specified, the signal SIGINT will be sent. To get the
value of other signals, you can use SB-POSIX:SIG<NAME>."
  (clpy.util:ensure-zero
   (if signum
       (clpy.ffi.fns:py-err-set-interrupt)
       (clpy.ffi.fns:py-err-set-interrupt-ex signum))
   (clpy.exception:raise-generic-or-python-error
    :message "Error encountered when calling SET-INTERRUPT.")))
