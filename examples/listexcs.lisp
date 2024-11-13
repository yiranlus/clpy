#!/usr/bin/env -S sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "R:/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :clpy)

(py:initialize)

(loop for (kw . value) in py.exc:*assoc-excs*
      do (format t "~A~30T~A~%" kw value))

(py:finalize)
