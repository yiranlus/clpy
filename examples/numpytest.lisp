(require 'clpy)

;; (defpackage :clpy.examples
;;   (:use :cl))

;; (in-package :clpy.examples)

(py:initialize)

(format t "Python Version: ~X~%" py:+version+)


(sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow)
  (defparameter *numpy* (py:import "numpy")))


(py:let ((array (py.obj:get-attr *numpy* "array"))
         (sum (py.obj:get-attr *numpy* "sum"))
         (arange (py.obj:get-attr *numpy* "arange")))
  (py:let* ((l (py:new '(:list 1 2 3 4 5 6 7 8 9 10)))
            (numpy-l (py:call array :args (list l)))
            (sum-l (py:call sum :args (list numpy-l))))
    (format t "NumPy Array: ~A~%" numpy-l)
    (format t "Numpy Sum: ~A~%" sum-l))
  (format t "~%")
  (py:let* ((x (py:call arange :args '(20)))
            (four (py:new 4))
            (y (py.num:% x four)))
    (format t "NumPy Array: ~A~%" x)
    (format t "Power: ~A~%" y)))

(py.obj:dec-xref *numpy*)

(py:finalize)
