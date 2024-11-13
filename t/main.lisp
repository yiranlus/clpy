#!/usr/bin/env -S sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "R:/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :clpy/test)
(require :fiveam)

(py:initialize)

(in-package :clpy.test)
(fiveam:run! 'clpy)

(py:finalize)
