(ql:quickload '(:clpy :cl-heredoc))

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(py:initialize)

(defparameter code "sum(i**2 for i in range(1, 11))")

(py:let ((eval-ob (py:eval code)))
  (format t "~A~%" eval-ob))

(py:finalize)
