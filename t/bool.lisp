(in-package :clpy.test)

(def-suite bool-test
  :in clpy
  :description "Test CLPY system")

(in-suite bool-test)

(test test-bool-type
  (py:let ((v (py.bool:new nil)))
    (is-true (py.bool:p v))))

(test create-bool
  (py:let ((v (py.bool:new nil)))
    (is-true (py:false-p v)))
  (py:let ((v (py.bool:new t)))
    (is-true (py:true-p v))))

(test smart-new
  (py:let ((v (py:new :false)))
    (is-true (py:false-p v))
    (is-false (py:none-p v)))
  (py:let ((v (py:new :true)))
    (is-true (py:true-p v))
    (is-true (py:true-p v :exact t))))
