(defpackage :clpy.buffer
  (:nicknames :py.buf)
  (:use :cl :plus-c)
  (:import-from :clpy.ffi #:py-buffer)
  (:export #:p
           #:buffer-buf
           #:buffer-obj
           #:buffer-len
           #:buffer-readonly
           #:buffer-itemsize
           #:buffer-format
           #:buffer-ndim
           #:buffer-shape
           #:buffer-strides
           #:buffer-suboffsets
           #:buffer-internal
           #:make-flags
           #:get-buffer
           #:release
           #:size-from-format
           #:contiguous-p
           #:get-pointer
           #:copy))

(in-package :clpy.buffer)

;; Py_buffer


(defun buffer-buf (buf)
  (clpy.ffi.acc:py-buffer.buf buf))



(defun buffer-obj (buf)
  (clpy.ffi.acc:py-buffer.obj buf))



(defun buffer-len (buf)
  (clpy.ffi.acc:py-buffer.len buf))



(defun buffer-readonly (buf)
  (plusp
   (clpy.ffi.acc:py-buffer.readonly buf)))



(defun buffer-itemsize (buf)
  (clpy.ffi.acc:py-buffer.itemsize buf))



(defun buffer-format (buf)
  (clpy.ffi.acc:py-buffer.format buf))



(defun buffer-ndim (buf)
  (clpy.ffi.acc:py-buffer.ndim buf))


(defun buffer-shape (buf)
  (c-let ((shape clpy.ffi:py-buffer
                 :from (clpy.ffi.acc:py-buffer.shape buf)))
    '(loop for i from 0 to (buffer-ndim buf)
      collect (shape i))))


(defun buffer-strides (buf)
  (c-let ((shape clpy.ffi:py-buffer
                 :from (clpy.ffi.acc:py-buffer.strides buf)))
    (loop for i from 0 to (buffer-ndim buf)
          collect (shape i))))


(defun buffer-suboffsets (buf)
  (c-let ((shape clpy.ffi:py-buffer
                 :from (clpy.ffi.acc:py-buffer.suboffsets buf)))
    (loop for i from 0 to (buffer-ndim buf)
          collect (shape i))))


(defun buffer-internal (buf)
  (clpy.ffi.acc:py-buffer.internal buf))

;; flags

(defun make-flags (&rest flags)
  (labels ((interp (flag)
             (if (integerp flag)
                 flag
                 (case flag
                   (:simple         #x0001)
                   (:writable       #x0001)
                   (:format         #x0004)
                   (:nd             #x0008)
                   (:strides        (logior #x0010 (interp :nd)))
                   (:c-contiguous   (logior #x0020 (interp :strides)))
                   (:f-contiguous   (logior #x0040 (interp :strides)))
                   (:any-contiguous (logior #x0080 (interp :strides)))
                   (:indirect       (logior #x0100 (interp :strides)))
                   (:contig         (logior (interp :nd) (interp :writable)))
                   (:contig-ro      (interp :nd))
                   (:strided        (logior (interp :strides) (interp :writable)))
                   (:strided-ro     (interp :strides))
                   (:records        (logior (interp :strides) (interp :writable) (interp :format)))
                   (:records-ro     (logior (interp :strides) (interp :format)))
                   (:full           (logior (interp :indirect) (interp :writable) (interp :format)))
                   (:full-ro        (logior (interp :indirect) (interp :format)))
                   (:read           #x0100)
                   (:write          #x0200)
                   (otherwise (error "Unsuppoerted Py_Buffer flags."))))))
    (reduce #'(lambda (a b)
                (logior (interp a) (interp b)))
            flags :initial-value #x0000)))

;; Buffer protocol


(defun p (o)
  "Check if the object ``O`` support the Buffer protocol."
  (plusp (clpy.ffi.fns:py-object-check-buffer o)))


(defun get-buffer (o &rest flags)
  "Get the buffer of the buffer of the PyObject.

An optional FLAGS can be provide. Unlike PyObject_GetBuffer, you will get the
Py_buffer pointer as a return function.

You should use :cl:functoin:`release` to release the reference to the current
view and use ``autowrap:free`` to free the allocated Py_Buffer object."
  (c-let ((view clpy.ffi:py-buffer))
    (clpy.util:ensure-zero
        (clpy.ffi.fns:py-object-get-buffer o view (apply #'make-flags flags))
      (clpy.exception:raise-generic-or-python-error))
    view))


(defun release (view)
  "Release the strong reference from VIEW.

This should be used paired with :cl:function:`get-buffer`."
  (clpy.ffi.fns:py-buffer-release view)
  (autowrap:free view))


(defun size-from-format (format)
  "Return the implied itemsize from FORMAT.

FORMAT should be a string."
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-buffer-size-from-format format)
    (clpy.exception:raise-generic-or-python-error)))


(defun contiguous-p (view order)
  "Check the order of the VIEW.

ORDER should be one of ``:C``, ``:F`` or ``:ANY``, which means C-style order,
Fortran-style order or either one."
  (plusp
   (let ((-order (case order
                   (:c (char-code #\C))
                   (:f (char-code #\F))
                   (:any (char-code #\A)))))
     (clpy.ffi.fns:py-buffer-is-contiguous view -order))))


(defun get-pointer (view &rest indices)
  "Get the memory area pointed to by the indices inside the given VIEW.

INDECES are specified as a list of number. It cals ``PyBuffer_GetPointer``, but
the indices array is dynamically allocated and destroyed after use."
  (let ((len (length indices))
        (ndim (buffer-ndim view))
        res)
    (unless (= len ndim)
      (error (format nil "The length of INDICES ~A is not the same as (BUFFER-NDIM view) which is ~A." len ndim)))
    (c-with ((-indices clpy.ffi:py-ssize-t :count len))
      (dotimes (i len)
        (setf (-indices i) (nth i indices)))
      (setf res (clpy.ffi.fns:py-buffer-get-pointer view -indices)))
    (clpy.util:ensure-null-as-nil
        res
      (clpy.exception:raise-generic-or-python-error))))


(defun copy (from to &optional (as-order :any) len)
  "Copy the buffer from one to another.

FROM and TO can be either PyObject, Py_Buffer or a pointer. AS-ORDER should be
one of ``:C``, ``:F`` or ``:ANY``. LEN should be specified when you copy from
a pointer"
  (let ((order (case as-order
                 (:c (char-code #\C))
                 (:f (char-code #\F))
                 (:any (char-code #\A))
                 (otherwise (error (format nil "Non-supported ORDER ~A" as-order))))))
    (clpy.util:ensure-non-negative
        (cond
          ((and (typep from 'clpy.ffi:py-buffer)
                (cffi:pointerp to))
           (let ((len (or len (buffer-len from))))
             (clpy.ffi.fns:py-buffer-to-contiguous to from len order)))
          ((and (cffi:pointerp from)
                (typep to 'clpy.ffi:py-buffer))
           (unless len
             (error 'clpy.exception:generic-error
                    :message "LENGTH must be specified when copying from pointer."))
           (when (eq as-order :any)
             (error ":ANY is not supported when copying from pointer"))
           (clpy.ffi.fns:py-buffer-from-contiguous to from len order))
          ((and (typep from 'clpy.ffi:py-object)
                (typep to 'clpy.ffi:py-object))
           (clpy.ffi.fns:py-object-copy-data to from))
          (t (error 'clpy.exception:generic-error
                    :message (format nil "Copying data from TYPE ~A to ~A is not supported"
                                     (type-of from) (type-of to)))))
      (clpy.exception:raise-generic-or-python-error))))
