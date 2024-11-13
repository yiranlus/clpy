(in-package :clpy.test)

(def-suite object-test
  :in clpy
  :description "Test CLPY system")

(in-suite object-test)

(test is-instance-list-type
  (py:let ((x (py.list:new 1 2 3 4 5)))
    (is-true (py.object:is-instance x :list))
    (is-false (py.object:is-instance x :long))))

(test is-instance-int-type
  (py:let ((x (py.number:new 2)))
    (is-true (py.object:is-instance x :long))
    (is-false (py.object:is-instance x :float))))

(test is-instance-float-type
  (py:let ((x (py.number:new 3.0)))
    (is-true (py.object:is-instance x :float))
    (is-false (py.object:is-instance x :list))))

(test is-instance-complex
  (py:let ((x (py.number:new #C(3 4))))
    (is-true (py.object:is-instance x :complex))
    (is-false (py.object:is-instance x :float))))

;;(test list-repr
;;  (py:let ((x (py.list:new 1 2 3 4 5))))
