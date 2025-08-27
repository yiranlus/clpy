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
    (is-true (py.obj:false-p v)))
  (py:let ((v (py.bool:new t)))
    (is-true (py.obj:true-p v))))
