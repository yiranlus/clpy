(defpackage :clpy.object
  (:nicknames :py.obj)
  (:use :cl)
  (:shadow #:format #:type)
  (:export #:object-p
	   #:ob-refcnt
	   #:ob-type
	   #:new-ref
	   #:new-xref
           #:inc-ref
	   #:inc-xref
           #:dec-ref
	   #:dec-xref

	   ;; attr
	   #:has-attr
           #:get-attr
           #:set-attr
           ;;#:generic-get-attr
           ;;#:generic-set-attr
           ;;#:generic-get-dict
           ;;#:generic-set-dict
           #:dir

           ;; query
           ;;#:rich-compare
           ;;#:format
           #:repr
           #:ascii
           #:str
           #:bytes
           ;;#:is-subclass
           #:is-instance
           ;;#:hash
           ;;#:hash-not-implemented
           #:is-true
           #:is-false
           ;;#:type

           ;; item
           #:size
           #:get-item
           #:set-item
           #:del-item
           ;;#:get-iter
           ;;#:get-a-iter
           ;; callable
           #:call
           ))
