(defpackage :clpy.long
  (:nicknames :py.long)
  (:use :cl)
  (:export #:new
           #:as-integer))

(in-package :clpy.long)

(defvar +py-long-from-binding-list+
  `((:long . ,#'clpy.ffi.fns:py-long-from-long)
    (:long-long . ,#'clpy.ffi.fns:py-long-from-long-long)
    (:unsigned-long . ,#'clpy.ffi.fns:py-long-from-unsigned-long)
    (:unsigned-long . ,#'clpy.ffi.fns:py-long-from-unsigned-long-long)
    (:size-t . ,#'clpy.ffi.fns:py-long-from-size-t)
    (:ssize-t . ,#'clpy.ffi.fns:py-long-from-ssize-t)
    (:double . ,#'clpy.ffi.fns:py-long-from-double)))

(defun new (value &key method (base 0))
  (py:ensure-null-as-nil
    (cond
      ((assoc method +py-long-from-binding-list+)
       (funcall (assoc method +py-long-from-binding-list+) value))
      ((stringp value)
       (clpy.ffi.fns:py-long-from-string value nil base))
      (t
       (cond
         ((floatp value) (clpy.ffi.fns:py-long-from-double value))
         ((integerp value)
          (let ((nbits (when (integerp value) (1+ (integer-length value)))))
            (cond
              ((<= nbits 32) (clpy.ffi.fns:py-long-from-long value))
              ((<= nbits 64) (clpy.ffi.fns:py-long-from-long-long value))))))))))

(defvar +py-long-as-binding-list+
  `((:long . ,#'clpy.ffi.fns:py-long-as-long)
    (:long-long . ,#'clpy.ffi.fns:py-long-as-long-long)
    (:unsigned-long . ,#'clpy.ffi.fns:py-long-as-unsigned-long)
    (:unsigned-long . ,#'clpy.ffi.fns:py-long-as-unsigned-long-long)
    (:size-t . ,#'clpy.ffi.fns:py-long-as-size-t)
    (:ssize-t . ,#'clpy.ffi.fns:py-long-as-ssize-t)
    (:double . ,#'clpy.ffi.fns:py-long-as-double)))

(defun as-integer (obj &key (method :ssize-t))
  "Return a C long representation of OBJ."
  (let ((res (funcall (cdr (assoc method +py-long-as-binding-list+)) obj)))
    res))
