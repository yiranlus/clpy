(defpackage :clpy.unicode
  (:nicknames :py.unicode)
  (:use :cl)
  (:export #:is-identifier
           #:from-string
           #:decode-fs-default
           #:encode-fs-default))

(in-package :clpy.unicode)

;; Unicode type

(defun is-identifier (unicode)
  "Return T if the string is valid identifier according to the language definition, otherwise nil"
  (zerop (clpy.ffi.fns:py-unicode-is-identifier unicode)))

;; Unicode properties

(defun from-string (str)
  (py:ensure-null-as-nil (clpy.ffi.fns:py-unicode-from-string str)))

(defun decode-fs-default (str)
  (log:debug "Enter PY.UNICODE:DECODE-FS-DEFAULT.")
  (log:debug "Creating unicode object from" str)
  (py:ensure-null-as-nil (clpy.ffi.fns:py-unicode-decode-fs-default str)
    (log:debug "Failed to create unicode object from" str)))

(defun encode-fs-default (unicode)
  "Encode a Unicode object to the filesystem encoding and error handler, and return PY-BYTES."
  (py:ensure-null-as-nil (clpy.ffi.fns:py-unicode-encode-fs-default unicode)))
