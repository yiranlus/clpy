(in-package :clpy.test)

(def-suite sequence-test
  :in clpy
  :description "Test CLPY system")

(in-suite sequence-test)

(test append-items
  (py:let ((l (py.list:new 1 2 3 4 5)))
    (dotimes (i 5)
      (py:let ((x (py.num:new i)))
	(py.list:append l x)))
    (is (= 10 (py.list:size l)))))

(test insert
  (py:let ((l (py.list:new 1 2 3 4 5)))
    (dotimes (i 5)
      (py:let ((x (py.num:new i)))
	(py.list:insert l 0 x)))
    (is (= 10 (py.list:size l)))
    (is (= 4 (py.num:as-integer (py.list:get-item l 0))))))
