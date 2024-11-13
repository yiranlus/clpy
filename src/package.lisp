(defpackage :clpy.pylet
  (:use :cl)
  (:shadow #:let #:let*)
  (:export #:let #:let*))

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

(defpackage :clpy.smart
  (:use :cl)
  (:export #:new))

(defpackage :clpy
  (:nicknames :py)
  (:use :clpy.core :clpy.object :clpy.pylet :clpy.util :clpy.smart)
  (:export #:new

	   ;; PyObject
	   #:object-p
	   #:ob-refcnt
	   #:ob-type
	   #:new-ref
	   #:new-xref
           #:inc-ref
	   #:inc-xref
           #:dec-ref
	   #:dec-xref

	   ;; initialization and finalization
           #:initialize
           #:is-initialized
           #:finalize
	   
           ;; runtime.lisp
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
	   
           ;;#:import-module ;; import.lisp
           #:clear-error ;; error.lisp
           #:print-error
           #:error-occurred

	   ;; clpy.util
	   #:ensure-null-as-nil
	   #:ensure-zero
	   #:ensure-non-negative

	   ;; clpy.pylet
	   #:let #:let*
           ))
