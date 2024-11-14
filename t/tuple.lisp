(in-package :clpy.test)

(def-suite tuple-test
  :in clpy
  :description "Test CLPY system")

(in-suite tuple-test)

(test new-empty-tuple-1
  (py:let ((l (py.tuple:new)))
    (is (= 0 (py.tuple:size l))))
  (py:let ((l (py.tuple:new-of 0)))
    (is (= 0 (py.tuple:size l)))))

(test new-empty-tuple-2
  (py:let ((l (py.tuple:new-of 5)))
    (is (= 5 (py.tuple:size l)))))

(test new-tuple-with-nil
  (py:let ((l (py.tuple:new nil)))
    (is (= 1 (py.tuple:size l)))))

(test new-tuple-with-number
  (py:let ((l (py.tuple:new 1 2 3 4 5 6)))
    (is (= 6 (py.tuple:size l)))))
