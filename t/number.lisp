(in-package :clpy.test)

(def-suite number-test
  :in clpy
  :description "Test CLPY system")

(in-suite number-test)

(test test-number-type
  (py:let ((x (py.num:new 20)))
    (is-true (py.num:p x)))
  (py:let ((x (py.num:new #C(1.0 2.0))))
    (is-true (py.num:p x)))
  (py:let ((x (py.num:new 20.30)))
    (is-true (py.num:p x))))

(test add
  (let ((a (funcall (gen-integer :max 256 :min -256)))
	(b (funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.num:new a))
	      (b-py (py.num:new b)))
      (let ((c-py (py.num:+ a-py b-py)))
	(is (= (+ a b) (py.num:as-integer c-py)))
	(py:dec-xref c-py)))))

(test subtract
  (let ((a (funcall (gen-integer :max 256 :min -256)))
	(b (funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.num:new a))
	      (b-py (py.num:new b)))
      (let ((c-py (py.num:- a-py b-py)))
	(is (= (- a b) (py.num:as-integer c-py)))
	(py:dec-xref c-py)))))

(test multiply
  (let ((a (funcall (gen-integer :max 256 :min -256)))
	(b (funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.num:new a))
	      (b-py (py.num:new b)))
      (let ((c-py (py.num:* a-py b-py)))
	(is (= (* a b) (py.num:as-integer c-py)))
	(py:dec-xref c-py)))))

(test complex-add
  (let ((a 1) ;(funcall (gen-integer :max 256 :min -256)))
	(b 2) ;(funcall (gen-integer :max 256 :min -256)))
	(c 3) ;(funcall (gen-integer :max 256 :min -256)))
	(d 4)) ;(funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.num:new (complex a b)))
	      (b-py (py.num:new (complex c d))))
      (let ((c-py (py.num:+ a-py b-py)))
	(is (= (+ a c) (py.num:real c-py)))
	(is (= (+ b d) (py.num:imag c-py)))
	(py:dec-xref c-py)))))
