(defpackage :clpy
  (:nicknames :py)
  (:use :clpy.interpreter :clpy.object :clpy.import
        :clpy.util :clpy.smart 
	:clpy.exception :clpy.eval)
  (:export #:new
           ;; #:print

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

	   ;; interpreter
           #:initialize
           #:is-initialized
           #:finalize
	   
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
	   #:get-recursion-limit

	   ;; clpy.eval
	   #:eval

	   ;; clpy.import
           #:import
	   
	   ;; clpy.exception
           #:clear-error
           #:print-error
           #:error-occurred

	   ;; clpy.util
	   #:let #:let*
           ))
