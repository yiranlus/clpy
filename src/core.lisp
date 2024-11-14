(defpackage :clpy.core
  (:use :cl :plus-c)
  (:export #:initialize
	   #:is-initialized
	   #:finalize
	   ;; runtime
	   #:encode-locale
           #:decode-locale
           #:set-program-name
           #:get-program-name
           #:get-prefix
           #:get-exec-prefix
           #:get-path
           #:get-program-full-path
           #:get-version
           #:get-platform
           #:get-copyright
           #:get-compiler
           #:get-build-info
           #:set-python-home
           #:get-python-home
	   ;; error
	   #:clear-error ;; error.lisp
           #:print-error
           #:error-occurred))
