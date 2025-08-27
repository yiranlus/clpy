(require :clpy)

(py:initialize)

(defparameter a -3)
(defparameter a-py (py.num:new a))
(defparameter b-py (py.num:abs a-py))

(format t "The result of abs(~A) is ~A.~%" a (py.num:as-integer b-py))

(py:dec-ref b-py)
(py:dec-ref a-py)

(py:finalize)
