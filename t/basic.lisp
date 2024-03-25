;;(in-package :clpy.test)
;;
;;(def-suite* clpy.test.basic :in clpy.test)
;;
;;(test get-program-name
(ql:quickload :clpy)

(let ((name "sbcl-python3"))
  (py:set-program-name name))
(py:initialize)

(format t "Program Name: ~A~%" (py:get-program-name))
(format t "Prefix: ~A~%" (py:get-prefix))
(format t "Exec Prefix: ~A~%" (py:get-exec-prefix))
(format t "Program Full Path: ~A~%" (py:get-program-full-path))
(format t "Module Path: ~A~%" (py:get-path))
(format t "Version: ~A~%" (py:get-version))
(format t "Platform: ~A~%" (py:get-platform))
(format t "Copyright: ~A~%" (py:get-copyright))
(format t "Compiler: ~A~%" (py:get-compiler))
(format t "Build Info: ~A~%" (py:get-build-info))

(py:finalize)
