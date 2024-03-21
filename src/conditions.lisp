(in-package :clpy)

(define-condition python-error ())

(define-condition type-error (python-error))
