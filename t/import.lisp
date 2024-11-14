(in-package :clpy.test)

(def-suite import-test
  :in clpy
  :description "Test CLPY system")

(in-suite import-test)

(test import-by-name
  (py:let ((m (py:import "math")))))
