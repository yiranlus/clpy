(defpackage :clpy.codec
  (:nicknames :py.codec)
  (:use :cl)
  (:shadow #:ignore-errors)
  (:export #:register
           #:unregister
           #:known-encoding-p
           #:encode
           #:decode
           ;;codec lookup API
           #:encoder
           #:decoder
           #:incremental-encoder
           #:incremental-decoder
           #:stream-reader
           #:stream-writer
           ;; registry API for unicode encoding error handlers
           #:register-error
           #:lookup-error
           #:strict-errors
           #:ignore-errors
           #:replace-errors
           #:XML-char-ref-replace-errors
           #:backslash-replace-errors
           #:name-replace-errors))

(in-package :clpy.codec)

(defun register (search-function)
  (clpy.util:ensure-zero
   (clpy.ffi.fns:py-codec-register search-function)
   (clpy.exception:raise-generic-or-python-error
    :message "Unable to register the codec.")))

(defun unregister (search-function)
  (clpy.util:ensure-zero
   (clpy.ffi.fns:py-codec-unregister search-function)
   (clpy.exception:raise-generic-or-python-error)))

(defun known-encoding-p (encoding)
  (zerop (clpy.ffi.fns:py-codec-known-encoding encoding)))

(defun encode (object encoding &optional errors)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-encode object encoding errors)
   (clpy.exception:raise-generic-or-python-error)))

(defun decode (object encoding &optional errors)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-decode object encoding errors)
   (clpy.exception:raise-generic-or-python-error)))

;; codec lookup API

(defun encoder (encoding)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-encoder encoding)
   (clpy.exception:raise-generic-or-python-error)))

(defun decoder (encoding)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-decoder encoding)))

(defun incremental-encoder (encoding errors)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-incremental-encoder encoding errors)
   (clpy.exception:raise-generic-or-python-error)))

(defun incremental-decoder (encoding errors)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-incremental-decoder encoding errors)
   (clpy.exception:raise-generic-or-python-error)))

(defun stream-reader (encoding stream errors)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-stream-reader encoding stream errors)
   (clpy.exception:raise-generic-or-python-error)))

(defun stream-writer (encoding stream errors)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-stream-writer encoding stream errors)
   (clpy.exception:raise-generic-or-python-error)))

;; registry for unicode encoding error handlers

(defun register-error (name error)
  (zerop (clpy.ffi.fns:py-codec-register-error name error)))

(defun lookup-error (name)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-lookup-error name)
   (error 'py.exc:generic-error)))

(defun strict-errors (exc)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-strict-errors exc)))

(defun ignore-errors (exc)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-ignore-errors exc)
   (clpy.exception:raise-generic-or-python-error)))

(defun replace-errors (exc)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-replace-errors exc)
   (clpy.exception:raise-generic-or-python-error)))

(defun xml-char-errors (exc)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-xml-char-ref-replace-errors exc)
   (clpy.exception:raise-generic-or-python-error)))

(defun backslash-replace-errors (exc)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-backslash-replace-errors exc)
   (clpy.exception:raise-generic-or-python-error)))

(defun name-replace-errors (exc)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-codec-name-replace-errors exc)
   (clpy.exception:raise-generic-or-python-error)))
