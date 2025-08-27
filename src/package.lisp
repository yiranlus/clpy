(defpackage :clpy
  (:nicknames :py)
  (:use :clpy.object :clpy.import
        :clpy.util :clpy.smart 
	:clpy.exception :clpy.eval
	:clpy.interpreter)
  (:shadowing-import-from :clpy.object #:type)
  (:shadow #:finalize #:get)
  (:export #:+VERSION+
	   #:+UTF8-MODE+
	   #:+ELLIPSIS+

	   #:+NONE+
	   #:none
	   #:+NOT-IMPLEMENTED+
	   #:not-implemented
	   
	   #:new

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

	   ;; PyObject Attrs
	   #:has-attr
           #:get-attr
           #:set-attr
           #:get-dict
           #:set-dict
	   #:get-item
	   #:set-item
	   #:del-item
           #:dir

	   ;; query
           #:rich-compare
           #:format
           #:repr
           #:ascii
           #:str
           #:bytes
           #:is-subclass
           #:is-instance
           #:hash
           #:hash-not-implemented
           #:true-p
	   #:false-p
	   #:none-p
           #:not
           #:type

	   ;; item
           #:len
           #:get-item
           #:set-item
           #:del-item
	   #:self-iter
	   #:get-iter
	   #:get-a-iter

	   ;; function calling
           #:callable-p
           #:call

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

	   ;; clpy.str
	   #:+FILE-SYSTEM-DEFAULT-ENCODE-ERRORS+
	   #:+FILE-SYSTEM-DEFAULT-ENCODING+
	   #:+HAS-FILE-SYSTEM-DEFAULT-ENCODING+

	   ;; clpy.interpreter
	   #:fatal-error
	   #:exit
	   #:at-exit
	   
	   #:enter-recursive-call
	   #:leave-recursive-call
	   #:repr-enter
	   #:repr-leave
	   
	   #:new-interpreter
	   #:end-interpreter

	   #:add-pending-call
	   #:make-panding-calls

	   ;; clpy.eval
	   #:eval
	   #:allow-threads

	   ;; clpy.import
           #:import
	   #:import-as

	   ;; clpy.util
	   #:let #:let*
           ))

(defmethod print-object ((object clpy.ffi:py-object) stream)
  (unless (clpy.smart:print object stream)
    (clpy.util:let* ((repr (clpy.object:repr object))
		     (encoded (clpy.str:encode repr)))
      (write-string (clpy.bytes:as-string encoded) stream))))

(in-package :clpy)

(cffi:defcvar ("Py_Version" +VERSION+ :read-only t) :unsigned-long)
(cffi:defcvar ("Py_UTF8Mode" +UTF8-MODE+ :read-only t) :int)

;; Helper function to finalize and free the registry

(cl:defun finalize ()
  (clpy.interpreter:finalize)
  ;; Free the registry
  (clpy.named-tuple:free-registry)
  (clpy.c-function:free-registry)
  (clpy.member:free-registry)
  )




