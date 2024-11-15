(in-package :clpy.smart)

(defparameter *print-mapping* (make-hash-table))

(defun print-hook (type print-func)
  (setf (gethash type *print-mapping*) print-func))

(defun print (x &optional stream)
  (if (clpy.object:p x)
      (let ((type (clpy.object:ob-type x)))
        (write-string (if (and (keywordp type)
                               (gethash type *print-mapping*))
                          (funcall (gethash type *print-mapping*) x)
                          (clpy.pylet:let* ((repr (py.obj:repr x))
                                            (code (py.str:encode repr)))
                            (py.bytes:as-string code)))
                      stream))
      (princ x stream)))
