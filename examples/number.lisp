#!/usr/bin/env -S sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "R:/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :clpy)

(py:initialize)

(defparameter a -3)
(defparameter a-py (py.number:new a))
(defparameter b-py (py.number:abs a-py))

(format t "The result of abs(~A) is ~A.~%" a (py.number:as-double b-py))

(py:dec-ref b-py)
(py:dec-ref a-py)

(py:finalize)
