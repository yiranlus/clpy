(defpackage :clpy.test
  (:use :cl :fiveam
        :plus-c)
  (:export #:clpy))

(in-package :clpy.test)

(def-suite clpy
  :description "Top-level test suite.")
