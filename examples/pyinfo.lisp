(require :clpy)

(py:initialize)

(format t "Setting Program Name to CLPY~%")
(py:set-program-name "CLPY")
(write-line "")

(format t "Program Name: ~A~%" (py:get-program-name))
(format t "Version: ~A~%" (py:get-version))
(format t "Platform: ~A~%" (py:get-platform))
(format t "Compiler: ~A~%" (py:get-compiler))
(format t "BuildInfo: ~A~%" (py:get-build-info))
(format t "Program Prefix: ~A~%" (py:get-prefix))
(format t "Exec Prefix: ~A~%" (py:get-exec-prefix))
(format t "Full Path: ~A~%" (py:get-program-full-path))
(format t "Path: ~A~%" (py:get-path))
(format t "Recursion Limit: ~A~%" (py:get-recursion-limit))
(format t "Copyright: ~A~%" (py:get-copyright))

(py:finalize)

(quit)
