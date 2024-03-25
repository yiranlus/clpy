(defpackage :clpy
  (:nicknames :py)
  (:use :cl :plus-c
        :trivia)
  (:export #:ensure-null-as-nil
           #:ensure-zero
           #:ensure-non-negative
           #:initialize
           #:is-initialized
           #:finalize
           ;; locale
           #:encode-locale
           #:decode-locale
           #:set-program-name
           #:get-program-name
           #:get-prefix
           #:get-exec-prefix
           #:set-path
           #:get-path
           #:get-program-full-path
           #:get-version
           #:get-platform
           #:get-copyright
           #:get-compiler
           #:get-build-info
           #:set-python-home
           #:get-python-home
           #:import-module ;; import.lisp
           #:clear-error ;; error.lisp
           #:print-error
           #:error-occurred
           #:new-ref ;; refcnt.lisp
           #:inc-ref
           #:dec-ref
           #:callable-check ;; call.lisp
           ))

(in-package :clpy)

(defmacro ensure-null-as-nil (value &body body)
  (let ((res (gensym)))
    `(let ((,res ,value))
       (if (not (cffi:null-pointer-p (autowrap:ptr ,res)))
           ,res
           (progn ,@body)))))

(defmacro ensure-zero (value &body body)
  (let ((res (gensym)))
    `(let ((,res (zerop ,value)))
       (or ,res
           (progn ,@body)))))

(defmacro ensure-non-negative (value &body body)
  (let ((res (gensym)))
    `(let ((,res ,value))
       (if (not (minusp ,res))
           ,res
           (progn ,@body)))))
