(defpackage :clpy.iterator
  (:nicknames :py.iter)
  (:use :cl :plus-c)
  (:export #:p
           #:async-p
           #:next
           #:send

           #:seq-p
           #:seq-new

           #:call-p
           #:call-new))

(in-package :clpy.iterator)

(defun p (o)
  (not (zerop
        (clpy.ffi.fns:py-iter-check o))))

(defun async-p (o)
  (not (zerop
        (clpy.ffi.fns:py-a-iter-check o))))

(defun next (iter)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-iter-next iter)
   (clpy.exception:return-or-raise-python-error nil)))

(defun send (iter arg)
  (c-with ((presult (:pointer clpy.ffi:py-object)))
          (let ((res (clpy.ffi.fns:py-iter-send iter arg (presult &))))
            (match res
              (clpy.ffi:+pygen-return+ (values res presult))
              (clpy.ffi:+pygen-next+ (values res presult))
              (clpy.ffi:+pygen-error+ nil)))))


(clpy.type:define-type "PySeqIter_Type" seq-iter)

(defun seq-p (iter)
  (clpy.type:of iter :seq-iter))

(defun seq-new (seq)
  "Return an iterator that works with a general sequence object."
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-seq-iter-new seq)
   (clpy.exception:raise-generic-or-python-error)))


(clpy.type:define-type "PyCallIter_Type" call-iter)

(defun call-p (iter)
  (clpy.type:of iter :call-iter))

(defun call-new (callable sentinel)
  "Return a new iterator using values returned from callable.

Then CALLABLE returns a value equal to SENTINEL, the iteration will be
terminated."
  (clpy.ffi.fns:py-call-iter-new callable sentinel))
