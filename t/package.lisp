(defpackage :clpy.test
  (:use :cl :fiveam
        :plus-c)
  (:shadow #:test)
  (:export #:clpy))

(in-package :clpy.test)

(def-suite clpy
  :description "Top-level test suite.")

(defmacro test (name &body body)
  `(fiveam:test ,name
     (py:initialize)
     (unless (py:is-initialized)
       (format *error-output* "Unable to intialize Python.")
       (exit 1))
     ,@body
     (py:finalize)))
