(ql:quickload '(:clpy :cl-heredoc))

(defmethod print-object ((object clpy.ffi:py-object) stream)
  (py:let* ((obj-repr (py.obj:repr object))
	    (encoded (py.str:encode obj-repr)))
    (print (py.bytes:as-string encoded))))

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(py:initialize)

(defparameter code "sum(i**2 i for i in range(1, 11))")

(py:let ((eval-ob (py:eval code)))
  (format t "~A~%" eval-ob))

(py:finalize)
