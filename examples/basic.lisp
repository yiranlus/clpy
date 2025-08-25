(require :clpy)

(defmethod print-object ((object clpy.ffi:py-object) stream)
  (py:let* ((obj-repr (py.obj:repr object))
	    (encoded (py.str:encode obj-repr)))
    (print (py.bytes:as-string encoded))))

(py:initialize)

(py:let* ((math (py:import "math"))
          (py-gcd (py.obj:get-attr math "gcd")))
  (let ((a 100)
        (b 40))
    (py:let ((c (py.obj:call py-gcd :args (list a b))))
      (format t "GCD of ~A and ~A is ~A.~%" a b c))))

(py:finalize)
