(require :clpy)

(py:initialize)

;; (let ((py-math (py.import:get "math" '("gcd"))))
;;   (destructuring-bind (py-gcd) py-math
;;     (let ((a 100)
;; 	  (b 40))
;;       (py:let ((c (py.obj:call py-gcd :args (list a b))))
;; 	(format t "GCD of ~A and ~A is ~A.~%" a b c)))))


(py:import-as (("math" ((py-gcd "gcd")
			(py-sin "sin"))))
  (let ((a 100)
	(b 40))
    (py:let ((c (py:call py-gcd :args (list a b))))
      (format t "GCD of ~A and ~A is ~A.~%" a b c))))

(py:finalize)
