(in-package :clpy.test)

(def-suite bool-test
  :in clpy
  :description "Test CLPY system")

(in-suite bool-test)

(test create-bool
  (py:let ((v (py.bool:new nil)))
    (is-true (py.obj:is-false v)))
  (py:let ((v (py.bool:new t)))
    (is-true (py.obj:is-true v))))
