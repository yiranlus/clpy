(asdf:defsystem "clpy"
  :depends-on ("cl-autowrap"
               ;;"cl-autowrap/libffi"
               "cl-plus-c"
               "alexandria"
               "sb-posix"
               ;;"trivia"
               ;;"log4cl"
               "cffi")
  :components ((:module clpy-inc
                :pathname "include"
                :components ((:static-file "stable-api.h")))
               (:module clpy-spec 
                :pathname "spec")
               (:module clpy-lib
                :pathname "lib")
               (:module "src"
                :components ((:file "library")
                             (:file "ffi" :depends-on ("library"))

                             (:file "util" :depends-on ("object"))
                             (:file "smart" :depends-on ("object"))

                             (:file "exception" :depends-on ("ffi" "util" "object"))
                             (:file "error" :depends-on ("ffi" "util" "str" "object" "exception"))

                             (:file "object")
                             (:file "object-base" :depends-on ("object" "ffi" "util" "type"))
                             (:file "object-proto" :depends-on ("object" "ffi" "str" "smart" "util"))
                             (:file "object-call" :depends-on ("object" "list" "dict" "util" "exception"))

                             ;; Data Types
                             (:file "type" :depends-on ("ffi" "object" "exception"))
                             (:file "number" :depends-on ("ffi" "smart" "type" "object" "util" "exception"))
                             (:file "bool" :depends-on ("ffi" "object" "type" "smart" "util"))
                             (:file "bytes" :depends-on ("type" "smart" "util" "exception"))
                             (:file "byte-array" :depends-on ("type" "smart" "util" "exception"))
                             (:file "str" :depends-on ("type" "smart" "util" "exception"))
                             
                             (:file "list" :depends-on ("type" "util" "smart"))
                             (:file "tuple" :depends-on ("type" "util" "smart"))
                             (:file "named-tuple" :depends-on ("type" "str" "util" "smart" "exception"))
                             (:file "set" :depends-on ("type" "util" "list" "smart"))
                             (:file "dict" :depends-on ("type" "smart" "util" "sequence" "exception"))

                             (:file "slice" :depends-on ("ffi" "util" "smart" "number" "type" "exception"))


                             (:file "file" :depends-on ("ffi" "util" "exception"))

                             ;; Protocols
                             (:file "sequence" :depends-on ("type" "util" "exception"))
                             (:file "mapping" :depends-on ("type" "util" "object" "exception"))
                             (:file "buffer" :depends-on ("ffi" "util" "exception"))
                             (:file "iterator" :depends-on ("ffi" "util" "exception"))
                             (:file "codec" :depends-on ("ffi" "util" "exception"))
                             (:file "weakref" :depends-on ("ffi" "util" "exception"))

                             ;; Module-Related
                             (:file "module" :depends-on ("ffi" "type" "util" "str" "exception"))
                             (:file "state" :depends-on ("ffi" "util" "exception"))
                             (:file "c-function" :depends-on ("ffi" "type" "util" "exception"))
                             (:file "import" :depends-on ("ffi" "util" "str" "dict" "list" "exception"))
                             (:file "capsule" :depends-on ("ffi" "type" "util" "exception"))

                             (:file "descriptor" :depends-on ("ffi" "type" "util" "exception"))
                             (:file "wrapper" :depends-on ("ffi" "type" "util" "exception"))
                             (:file "member" :depends-on ("ffi" "type" "exception"))

                             ;; Interpreter-Related
                             (:file "gc" :depends-on ("ffi"))
                             (:file "interpreter" :depends-on ("ffi" "util" "c-function" "named-tuple"))
                             (:file "gil" :depends-on ("ffi"))
                             (:file "thread" :depends-on ("ffi" "exception"))
                             (:file "system" :depends-on ("ffi" "exception"))
                             (:file "os" :depends-on ("ffi" "exception"))
                             (:file "eval" :depends-on ("ffi" "frame" "util" "exception"))
                             (:file "traceback" :depends-on ("ffi" "type" "util" "exception"))
                             (:file "frame" :depends-on ("ffi"))
                             (:file "memory" :depends-on ("ffi" "exception"))
                             (:file "memory-view" :depends-on ("ffi" "type" "object" "exception"))

                             (:file "package" :depends-on ("util" "smart" "bytes" "str" "object" "interpreter" "exception" "import" "eval" "named-tuple" "c-function" "member")))))
  :in-order-to ((test-op (test-op "clpy/test"))))

(asdf:defsystem "clpy/test"
  :depends-on ("clpy"
               "cl-plus-c"
               "fiveam")
  :pathname "t/"
  :components ((:file "package")
               ;;(:file "number" :depends-on ("package"))
               (:file "bool" :depends-on ("package"))
               (:file "bytes" :depends-on ("package"))
               (:file "byte-array" :depends-on ("package"))
               (:file "dict" :depends-on ("package"))
               (:file "list" :depends-on ("package"))
               (:file "tuple" :depends-on ("package"))
               (:file "str" :depends-on ("package"))
               (:file "object" :depends-on ("package"))
               (:file "set" :depends-on ("package"))
               (:file "buffer" :depends-on ("package"))
               (:file "import" :depends-on ("package"))
               (:file "object-call" :depends-on ("package"))
               (:file "smart" :depends-on ("package"))
               )
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clpy :clpy.test))))
