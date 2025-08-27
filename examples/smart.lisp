(require :clpy)

(py:initialize)

(py:let ((x (py:new 3)))
  (format t "An integer: ~A.~%" x))

(py:let ((x (py:new '(:list 1 2 3 4 5))))
  (format t "A list: ~A.~%" x))

(py:let ((x (py:new '(:dict
		      ("new" . 3)
		      ("old" . 4)))))
  (format t "A dict: ~A.~%" x))

(py:let ((x (py:new '(:set 1 2 3 4 5))))
  (format t "A set: ~A.~%" x))

(py:finalize)
