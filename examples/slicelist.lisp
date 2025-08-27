(require 'clpy)


(py:initialize)


(py:let ((l (py:new '(:list 1 2 3 4 5)))
	 (s (py.slice:new nil 3)))
  (format t "A list: ~A~%" l)
  (format t "A slice: ~A~%" s)
  (py:let ((sl (py.obj:get-item l s)))
    (format t "Sliced Object: ~A~%" sl)))


(py:finalize)
