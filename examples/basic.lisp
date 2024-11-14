#!/usr/bin/env -S sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "R:/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :clpy)

(py:initialize)

(py:let* ((math (py:import "math"))
          (py-gcd (py.obj:get-attr math "gcd")))
  (let ((a 100)
        (b 40))
    (py:let ((c (py.obj:call py-gcd :args (list a b))))
      (format t "GCD of ~A and ~A is ~A.~%" a b (py.num:as-integer c)))))

(py:finalize)
