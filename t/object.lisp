(in-package :clpy.test)

(def-suite object-test
  :in clpy
  :description "Test CLPY system")

(in-suite object-test)

(test is-instance-list-type
  (py:let ((x (py.list:new 1 2 3 4 5)))
    (is-true (py.obj:is-instance x :list))
    (is-false (py.obj:is-instance x :long))))

(test is-instance-int-type
  (py:let ((x (py.num:new 2)))
    (is-true (py.obj:is-instance x :long))
    (is-false (py.obj:is-instance x :float))))

(test is-instance-float-type
  (py:let ((x (py.num:new 3.0)))
    (is-true (py.obj:is-instance x :float))
    (is-false (py.obj:is-instance x :list))))

(test is-instance-complex
  (py:let ((x (py.num:new #C(3 4))))
    (is-true (py.obj:is-instance x :complex))
    (is-false (py.obj:is-instance x :float))))

;;(test list-repr
;;  (py:let ((x (py.list:new 1 2 3 4 5))))
