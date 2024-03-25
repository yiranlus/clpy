(defpackage :clpy.iter
  (:nicknames :py.iter)
  (:use :cl))

(in-package :clpy.iter)

(defun check (o)
  (plusp (clpy.ffi.fns:py-iter-check o)))

(defun next (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-iter-next o)
    (when (py:error-occurred)
      (error 'py.exc:python-error))))

;; TODO
;;(defun send (iter arg)
;;  (let ((presult ()))
;;    (let ((res (clpy.ffi.fns:py-iter-send iter arg (presult &))))
;;      (match res
;;        (clpy.ffi:+pygen-return+ (values res presult))
;;        (clpy.ffi:+pygen-next+ (values res presults))
;;        (clpy.ffi:+pygen-error+)))))))


(defpackage :clpy.aiter
  (:nicknames :py.aiter)
  (:use :cl))

(in-package :clpy.aiter)

(defun check (o)
  (plusp (clpy.ffi.fns:py-aiter-check o)))
