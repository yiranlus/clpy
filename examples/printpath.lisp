(require 'clpy)

(py:initialize)

(let ((sys (py.import:get "sys" (list "path"))))
  (destructuring-bind (sys.path) sys
    (format t "sys.path: ~A~%" sys.path)
    (clpy.util:xdec-refs sys)))

(py:finalize)
