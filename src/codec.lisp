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
           #:decoder))

(in-package :clpy.codec)

(defun register (search-function)
  (clpy.ffi.fns:py-codec-register search-function))

(defun unregister (search-function)
  (zerop (clpy.ffi.fns:py-codec-unregister search-function)))

(defun known-encoding-p (encoding)
  (zerop (clpy.ffi.fns:py-codec-known-encoding encoding)))

(defun encode (object encoding errors)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-encode object encoding errors)
    (error 'py.exc:generic-error)))

(defun decode (object encoding errors)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-decode object encoding errors)
    (error 'py.exc:generic-error)))

;; codec lookup API

(defun encoder (encoding)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-encoder encoding)
    (error 'py.exc:generic-error)))

(defun decoder (encoding)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-decoder encoding)
    (error 'py.exc:generic-error)))

(defun incremental-encoder (encoding errors)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-incremental-encoder encoding errors)
    (error 'py.exc:generic-error)))

(defun incremental-decoder (encoding errors)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-incremental-decoder encoding errors)
    (error 'py.exc:generic-error)))

(defun stream-reader (encoding stream errors)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-stream-reader encoding stream errors)
    (error 'py.exc:generic-error)))

(defun stream-writer (encoding stream errors)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-stream-writer encoding stream errors)
    (error 'py.exc:generic-error)))

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
    (error 'py.exc:generic-error)))

(defun replace-errors (exc)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-replace-errors exc)
    (error 'py.exc:generic-error)))

(defun xml-char-errors (exc)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-xml-char-ref-replace-errors exc)
    (error 'py.exc:generic-error)))

(defun backslash-replace-errors (exc)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-backslash-replace-errors exc)
    (error 'py.exc:generic-error)))

(defun name-replace-errors (exc)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-codec-name-replace-errors exc)
    (error 'py.exc:generic-error)))
