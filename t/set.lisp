(in-package :clpy.test)

(def-suite set-test
  :in clpy
  :description "Test CLPY system")

(in-suite set-test)

(test new-empty-set
  (py:let ((x (py.set:new)))
    (is (= 0 (py.set:size x)))))

(test new-set-number
  (py:let ((x (py.set:new 1 2 3 4 5)))
    (is (= 5 (py.set:size x)))))

(test new-complex-set-number-string
  (py:let ((x (py.set:new 1 2 3 4 "hello")))
    (is (= 5 (py.set:size x)))))

(test set-clear
  (py:let ((x (py.set:new 1 2 3 4 5)))
    (is (= 5 (py.set:size x)))
    (py.set:clear x)
    (is (= 0 (py.set:size x)))))

(test set-contains
  (py:let ((x (py.set:new 1 2 3 4 5)))
    (py:let ((k (py.num:new 1)))
      (is-true (py.set:contains x k)))
    (py:let ((k (py.num:new 6)))
      (is-false (py.set:contains x k)))))

(test set-add
  (py:let ((x (py.set:new 1 2 3 4 5)))
    (py:let ((k (py.num:new 6)))
      (py.set:add x k)
      (is (= 6 (py.set:size x)))
      (is-true (py.set:contains x k)))))

(test discard-existed-key
  (py:let ((x (py.set:new 1 2 3 4 5)))
    (py:let ((k (py.num:new 4)))
      (py.set:discard x k)
      (is (= 4 (py.set:size x)))
      (is-false (py.set:contains x k)))))

(test discard-non-existed-key
  (py:let ((x (py.set:new 1 2 3 4 5)))
    (py:let ((k (py.num:new 6)))
      (py.set:discard x k)
      (is (= 5 (py.set:size x)))
      (is-false (py.set:contains x k)))))
