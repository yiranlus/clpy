(in-package :clpy.test)

(def-suite object-call-test
  :in clpy
  :description "Test CLPY system")

(in-suite object-call-test)

(test math.gcd-call
  (py:let* ((math (py:import "math"))
            (py-gcd (py.obj:get-attr math "gcd")))
    (let ((a 100)
          (b 40))
      (py:let ((c (py.obj:call py-gcd :args (list a b))))
        (is (= 20 (py.num:as-integer c)))))))
