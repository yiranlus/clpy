(defpackage clpy.sys
  (:nicknames :py.sys)
  (:use :cl :plus-c))

(in-package :clpy.sys)

;; TODO: NOT IMPLEMENTED.
(defun set-argv (args &optional (updatepath 1))
  "Set ``sys.argv'' based on args. These parameters are similar to those passed
to the program main() function with the difference that the first entry should
refer to the script file to be executed rather than the executable hosting the
python interpreter.")
