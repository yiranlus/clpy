(in-package :clpy.test)

(def-suite number-test
  :in clpy
  :description "Test CLPY system")

(in-suite number-test)

(test add
  (let ((a (funcall (gen-integer :max 256 :min -256)))
	(b (funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.number:new a))
	      (b-py (py.number:new b)))
      (let ((c-py (py.number:+ a-py b-py)))
	(is (= (+ a b) (py.number:as-integer c-py)))
	(py:dec-xref c-py)))))

(test subtract
  (let ((a (funcall (gen-integer :max 256 :min -256)))
	(b (funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.number:new a))
	      (b-py (py.number:new b)))
      (let ((c-py (py.number:- a-py b-py)))
	(is (= (- a b) (py.number:as-integer c-py)))
	(py:dec-xref c-py)))))

(test multiply
  (let ((a (funcall (gen-integer :max 256 :min -256)))
	(b (funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.number:new a))
	      (b-py (py.number:new b)))
      (let ((c-py (py.number:* a-py b-py)))
	(is (= (* a b) (py.number:as-integer c-py)))
	(py:dec-xref c-py)))))

(test complex-add
  (let ((a 1) ;(funcall (gen-integer :max 256 :min -256)))
	(b 2) ;(funcall (gen-integer :max 256 :min -256)))
	(c 3) ;(funcall (gen-integer :max 256 :min -256)))
	(d 4)) ;(funcall (gen-integer :max 256 :min -256))))
    (py:let ((a-py (py.number:new (complex a b)))
	      (b-py (py.number:new (complex c d))))
      (let ((c-py (py.number:+ a-py b-py)))
	(is (= (+ a c) (py.number:real c-py)))
	(is (= (+ b d) (py.number:imag c-py)))
	(py:dec-xref c-py)))))
