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
			     (:file "smart")
			     
                             (:file "package" :depends-on ("util" "smart" "object" "interpreter" "exception" "import" "eval"))

                             
                             (:file "exception" :depends-on ("ffi" "util"))
                             (:file "error" :depends-on ("ffi" "util" "str" "bytes" "object" "exception"))

			     (:file "object")
			     (:file "object-base" :depends-on ("object" "ffi" "util" "type"))
			     (:file "object-proto" :depends-on ("object" "ffi" "util"))
                             ;; (:file "object-attr" :depends-on ("object" "utils" "error"))
                             ;; (:file "object-repr" :depends-on ("object" "utils" "error"))
                             ;; (:file "object-item" :depends-on ("object" "utils" "error"))
                             ;; (:file "object-query" :depends-on ("object" "utils" "error"))
                             (:file "object-call" :depends-on ("object" "list" "dict" "util" "exception"))

			     ;; Data Types
                             (:file "type" :depends-on ("ffi" "object"))
			     (:file "none" :depends-on ("type" "smart"))
                             (:file "number" :depends-on ("ffi" "smart" "type" "none" "util" "exception"))
                             (:file "bool" :depends-on ("type" "smart" "util"))
                             (:file "bytes" :depends-on ("type" "smart" "util" "exception"))
                             (:file "byte-array" :depends-on ("type" "smart" "util" "exception"))
                             (:file "str" :depends-on ("type" "smart" "util" "exception"))
                             
                             (:file "list" :depends-on ("type" "util" "smart"))
                             (:file "tuple" :depends-on ("type" "util" "smart"))
                             (:file "set" :depends-on ("type" "util" "list" "smart"))
                             (:file "dict" :depends-on ("type" "smart" "util" "sequence" "exception"))
			     
                             (:file "slice" :depends-on ("ffi" "util" "type" "exception"))


			     (:file "file" :depends-on ("ffi" "util" "exception"))
			     
			     ;; Protocols
                             (:file "sequence" :depends-on ("type" "util" "exception"))
                             (:file "mapping" :depends-on ("type" "util" "object" "exception"))
                             (:file "buffer" :depends-on ("ffi" "util" "exception"))
                             (:file "iterator" :depends-on ("ffi" "util" "exception"))
                             ;; (:file "codec" :depends-on ("ffi" "utils" "error"))

			     ;; Module-Related
                             ;; (:file "module" :depends-on ("ffi" "type" "utils" "error"))
                             (:file "import" :depends-on ("ffi" "smart" "util" "str" "dict" "list"))
                             ;; (:file "capsule" :depends-on ("ffi" "utils" "error"))

			     ;; Interpreter-Related
			     (:file "gc" :depends-on ("ffi"))
			     (:file "interpreter" :depends-on ("ffi" "util"))
			     (:file "eval" :depends-on ("ffi" "util" "exception"))
			     (:file "frame" :depends-on ("ffi" "type"))
                             ;; (:file "memory" :depends-on ("ffi" "object"))
                             
                             )))
  :in-order-to ((test-op (test-op "clpy/test"))))

(asdf:defsystem "clpy/test"
  :depends-on ("clpy"
               "cl-plus-c"
               "fiveam")
  :pathname "t/"
  :components ((:file "package")
               (:file "number" :depends-on ("package"))
               (:file "bool" :depends-on ("package"))
               (:file "bytes" :depends-on ("package"))
               (:file "byte-array" :depends-on ("package"))
               (:file "list" :depends-on ("package"))
               (:file "tuple" :depends-on ("package"))
               ;; (:file "unicode" :depends-on ("package"))
               ;; (:file "object" :depends-on ("package"))
               (:file "set" :depends-on ("package"))
               (:file "dict" :depends-on ("package"))
               ;; (:file "buffer" :depends-on ("package"))
               ;; (:file "import" :depends-on ("package"))
               ;; (:file "object-call" :depends-on ("package"))
               ;; (:file "smart" :depends-on ("package"))
	       )
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clpy :clpy.test))))
