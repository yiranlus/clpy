(in-package :clpy.test)

(def-suite smart-test
  :in clpy
  :description "Test CLPY system")

(in-suite smart-test)

;;(test new-string-singlton
;;  (py:let ((x (py:new "lsjdflksjf")))
;;    (is (py.str:p
