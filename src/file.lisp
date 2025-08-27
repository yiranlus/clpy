(defpackage :clpy.file
  (:nicknames :py.file)
  (:use :cl)
  (:shadow #:write-string)
  (:export #:from-fd
	   #:get-line
	   #:write-object
	   #:write-string))

(in-package :clpy.file)

(defun from-fd (fd mode &key (buffering -1) encoding errors newline (closefd t))
  (declare (ignore name))
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-file-from-fd fd nil mode buffering encoding errors newline (if closefd 1 0))
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to create a file object from the file descriptor.")))

(defun get-line (o &optional (n 0))
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-file-get-line o n)
    (clpy.exception:return-or-raise-python-error nil)))

(defun write-object (obj p &rest flags)
  "Write OBJ to file object ``P``."
  (labels ((interp (flag)
             (if (integerp flag)
                 flag
                 (case flag
                   (:raw #x0001)
                   (otherwise (error "Unsuppoerted Py_PRINT flags."))))))
    (let ((-flags (reduce #'(lambda (a b)
			      (logior (interp a) (interp b)))
			  flags :initial-value #x0000)))
      (clpy.util:ensure-non-negative
	  (clpy.ffi.fns:py-file-write-object obj p -flags)
	(clpy.exception:raise-generic-or-python-error)))))

(defun write-string (s p)
  "Write string ``S`` to file object ``P``."
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-file-write-string s p)
    (clpy.exception:raise-generic-or-python-error)))

