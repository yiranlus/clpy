(defpackage :clpy.object
  (:nicknames :py.object)
  (:use :cl)
  (:shadow #:format #:type)
  (:export #:has-attr
           #:get-attr
           #:set-attr
           ;;#:generic-get-attr
           ;;#:generic-set-attr
           ;;#:generic-get-dict
           ;;#:generic-set-dict
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
           ;;#:size
           ;;#:get-item
           ;;#:set-item
           ;;#:del-item
           ;;#:dir
           ;;#:get-iter
           ;;#:get-a-iter
           ;;#:call ;; call.lisp
           ))
