(in-package :clpy.test)

(def-suite list-test
  :in clpy
  :description "Test CLPY system")

(in-suite list-test)

(test new-empty-list-1
  (py:let ((l (py.list:new)))
    (is (= 0 (py.list:size l))))
  (py:let ((l (py.list:new-of 0)))
    (is (= 0 (py.list:size l)))))

(test new-empty-list-2
  (py:let ((l (py.list:new-of 5)))
    (is (= 5 (py.list:size l)))))

(test new-list-with-number
  (py:let ((l (py.list:new 1 2 3 4 5 6)))
    (is (= 6 (py.list:size l)))))

(test append-items
  (py:let ((l (py.list:new 1 2 3 4 5)))
    (dotimes (i 5)
      (py:let ((x (py.number:new i)))
	(py.list:append l x)))
    (is (= 10 (py.list:size l)))))

(test insert
  (py:let ((l (py.list:new 1 2 3 4 5)))
    (dotimes (i 5)
      (py:let ((x (py.number:new i)))
	(py.list:insert l 0 x)))
    (is (= 10 (py.list:size l)))
    (is (= 4 (py.number:as-integer (py.list:get-item l 0))))))
