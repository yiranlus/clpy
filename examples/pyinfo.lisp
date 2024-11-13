#!/usr/bin/env -S sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "R:/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :clpy)

(py:initialize)

(format t "Program Name: ~A~%" (py:get-program-name))
(format t "Version: ~A~%" (py:get-version))
(format t "Platform: ~A~%" (py:get-platform))
(format t "Compiler: ~A~%" (py:get-compiler))
(format t "BuildInfo: ~A~%" (py:get-build-info))
(format t "Program Prefix: ~A~%" (py:get-prefix))
(format t "Exec Prefix: ~A~%" (py:get-exec-prefix))
(format t "Full Path: ~A~%" (py:get-program-full-path))
(format t "PythonHome: ~A~%" (py:get-python-home))
(format t "Path: ~A~%" (py:get-path))
(format t "Copyright: ~A~%" (py:get-copyright))

(py:finalize)

(quit)
