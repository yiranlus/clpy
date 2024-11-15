(in-package :clpy.test)

(def-suite smart-test
  :in clpy
  :description "Test CLPY system")

(in-suite smart-test)

;; smart new

(test new-nil-object
  (py:let ((x (py:new nil)))
    (is-true (null x))))

(test new-empty-string
  (py:let ((x (py:new "")))
    (is (py.str:p x))))

(test new-string-singlton
  (py:let ((x (py:new "lsjdflksjf")))
    (is-true (py.str:p x))
    (py:let ((v (py.str:encode x)))
      (is (string-equal "lsjdflksjf" (py.bytes:as-string v))))))

(test new-number-singlton
  (py:let ((x (py:new 20)))
    (is-true (py.num:p x))
    (is (py.type:of x :long)))
  (py:let ((x (py:new #C(1 2))))
    (is-true (py.num:p x))
    (is (py.type:of x :complex)))
  (py:let ((x (py:new 20.03)))
    (is-true (py.num:p x))
    (is (py.type:of x :float))))

(test new-pyobjct-singlton
  (py:let ((x (py:new 2)))
    (py:let ((y (py:new x)))
      (is-true (cffi:pointer-eq (autowrap:ptr x) (autowrap:ptr y)))
      (is (= (py:ob-refcnt x) (py:ob-refcnt y))))))

(test new-list-singlton
  (py:let ((x (py:new `(:l 1 2 3 4 5))))
    (is-true (py.list:p x))
    (is (= 5 (py.list:size x)))))

(test new-dict-singlton
  (py:let ((x (py:new `(:d ("key1" . 1) ("key2" . 2)))))
    (is-true (py.dict:p x))
    (is (= 2 (py.dict:size x)))))

(test new-complex-list
  (py:let ((x (py:new '(:l 1 2 3.44 "Hello" #C(1 2)))))
    (is (= 5 (py.list:size x)))))

;; (test new-complex-set
;;   (py:let ((x (py:new '(:set 1 2 3.44 "Hello" #C(1 2)))))
;;     (is (= 5 (py.set:size x)))))

(test new-complex-dict
  (py:let ((x (py:new '(:d ("key1" . 1) (3 . "lksdjf")))))
    (is (= 2 (py.dict:size x)))))

(test new-complex-1
  (py:let ((x (py:new '(:l 1 2 3
			(:l 2 3 4)
			(:d ("key1" . 3) ("key2" . 4))))))
    (is-true (py.list:p x))
    (py:let ((y (py.list:get-item x 4)))
      (is-true (py.dict:p y)))
    (is (= 5 (py.list:size x)))))

;; smart print

(test smart-print-integer
  (py:let ((x (py.num:new 1)))
    (let ((value (with-output-to-string (output)
                   (py:print x output))))
      (is (string-equal "1" value)))))

(test smart-print-float
  (py:let ((x (py.num:new 1.0)))
    (let ((value (with-output-to-string (output)
                   (py:print x output))))
      (is (string-equal "1.0" value)))))

(test smart-print-complex
  (py:let ((x (py.num:new #C(1 2))))
    (let ((value (with-output-to-string (output)
                   (py:print x output))))
      (is-true (py.type:of x :complex))
      (is (string-equal "(1+2j)" value)))))

(test smart-print-list
  (py:let ((x (py.list:new 1 2 3 4 5)))
    (let ((value (with-output-to-string (output)
                   (py:print x output))))
      (is (string-equal "[1, 2, 3, 4, 5]" value)))))

(test smart-print-dict
  (py:let ((x (py.dict:new (cons 1 2)
                           (cons 3 4))))
    (let ((value (with-output-to-string (output)
                   (py:print x output))))
      (is (string-equal "{1: 2, 3: 4}" value)))))

(test smart-print-dict
  (py:let ((x (py.set:new 1 2 3 4 5)))
    (let ((value (with-output-to-string (output)
                   (py:print x output))))
      (is (string-equal "{1, 2, 3, 4, 5}" value)))))

(test smart-print-complex-list
  (py:let ((x (py:new `(:l 1 2 3
                           (:d (1 . 2)
                               (3 . 4))
                           (:s 1 2 3)))))
    (let ((value (with-output-to-string (output)
                   (py:print x output))))
      (is (string-equal "[1, 2, 3, {1: 2, 3: 4}, {1, 2, 3}]" value)))))
