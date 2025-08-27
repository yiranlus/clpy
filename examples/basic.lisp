(require :clpy)

(py:initialize)

;; (py:let* ((math (py:import "math"))
;;           (py-gcd (py.obj:get-attr math "gcd")))
;;   (let ((a 100)
;;         (b 40))
;;     (py:let ((c (py.obj:call py-gcd :args (list a b))))
;;       (format t "GCD of ~A and ~A is ~A.~%" a b c))))

(let ((py-math (py.import:get "math" '("gcd"))))
  (destructuring-bind (py-gcd) py-math
    (let ((a 100)
	  (b 40))
      (py:let ((c (py.obj:call py-gcd :args (list a b))))
	(format t "GCD of ~A and ~A is ~A.~%" a b c)))))

(py:finalize)
