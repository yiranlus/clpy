(defpackage :clpy.object
  (:nicknames :py.obj)
  (:use :cl)
  (:shadow #:format #:type #:not)
  (:export #:+GENERIC-GET-ATTR+
	   #:+GENERIC-SET-ATTR+
	   #:+GENERIC-GET-DICT+
	   #:+GENERIC-SET-DICT+
	   #:p
	   #:ob-refcnt
	   #:ob-type
	   #:ob-base
	   #:ob-size
	   #:new-ref
	   #:new-xref
           #:inc-ref
	   #:inc-xref
           #:dec-ref
	   #:dec-xref

	   #:init
	   #:init-var

	   ;; constant
	   #:+NONE+
	   #:none
	   #:+NOT-IMPLEMENTED+
	   #:not-implemented

	   ;; attr
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
           ;;#:get-iter
           ;;#:get-a-iter

	   ;; function calling
           #:callable-p
           #:call

	   #:self-iter
	   #:get-iter
	   #:get-a-iter

	   #:as-fd

	   #:clear-weakrefs
	   #:malloc
	   #:calloc
	   #:realloc
	   #:free))
