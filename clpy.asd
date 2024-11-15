(asdf:defsystem "clpy"
  :depends-on ("cl-autowrap"
               "cl-autowrap/libffi"
               "cl-plus-c"
               "alexandria"
               "trivia"
               "log4cl"
               "cffi")
  :components ((:module clpy-inc
                :pathname "include"
                :components ((:static-file "Python.h")))
               (:module clpy-spec
                :pathname "spec")
               (:module clpy-lib
                :pathname "lib")
               (:module "src"
                :components ((:file "library")
                             (:file "ffi" :depends-on ("library"))

                             (:file "utils")
                             (:file "package" :depends-on ("ffi" "utils" "smart" "object" "import"))

                             (:file "object" :depends-on ("ffi"))

                             (:file "core" :depends-on ("ffi"))
                             (:file "runtime" :depends-on ("core" "utils"))
                             (:file "initfinal" :depends-on ("core" "utils"))

                             (:file "pylet" :depends-on ("object"))
                             
                             (:file "types" :depends-on ("ffi"))
                             (:file "exception" :depends-on ("ffi"))
                             (:file "error" :depends-on ("exception" "utils" "core"))
                             
                             (:file "object-basic" :depends-on ("object" "types"))
                             (:file "object-attr" :depends-on ("object" "utils" "error"))
                             (:file "object-repr" :depends-on ("object" "utils" "error"))
                             (:file "object-item" :depends-on ("object" "utils" "error"))
                             (:file "object-query" :depends-on ("object" "utils" "error"))
                             (:file "object-call" :depends-on ("object" "smart" "utils" "error"))
                             
                             (:file "number" :depends-on ("ffi" "smart-new" "types" "utils" "error"))
                             (:file "bool" :depends-on ("types" "smart-new" "utils" "error"))
                             (:file "bytes" :depends-on ("types" "smart-new" "utils" "error"))
                             (:file "byte-array" :depends-on ("types" "smart-new" "utils" "error"))
                             (:file "unicode" :depends-on ("types" "smart-new" "utils" "error"))
                             
                             (:file "list" :depends-on ("types" "smart-new" "utils" "error"))
                             (:file "tuple" :depends-on ("types" "smart-new" "utils" "error"))
                             (:file "set" :depends-on ("types" "smart-new" "utils" "pylet" "error"))
                             (:file "dict" :depends-on ("types" "smart-new" "utils" "pylet" "sequence" "error"))
                             
                             (:file "sequence" :depends-on ("types" "utils" "pylet"))
                             (:file "mapping" :depends-on ("types" "utils" "pylet" "object"))
                             (:file "buffer" :depends-on ("ffi"))
                             (:file "slice" :depends-on ("ffi" "utils" "types" "error"))
                             (:file "iter" :depends-on ("ffi" "utils" "error"))
                             (:file "codec" :depends-on ("ffi" "utils" "error"))

                             (:file "memory" :depends-on ("ffi" "object"))

                             (:file "module" :depends-on ("ffi" "types" "utils" "error"))
                             (:file "import" :depends-on ("ffi" "smart" "utils" "unicode" "dict" "list"))
                             (:file "capsule" :depends-on ("ffi" "utils" "error"))

                             (:file "smart")
                             (:file "smart-new" :depends-on ("error"))
                             (:file "smart-print" :depends-on ("object" "pylet" "unicode" "bytes"))
                             
                             ;;(:file "object" :depends-on ("clpy"))
                             ;;(:file "complex" :depends-on ("object"))
                             ;;(:file "bool" :depends-on ("package"))
                             ;;(:file "exception" :depends-on ("ffi"))
                             ;;(:file "object" :depends-on ("package"))
                             ;;(:file "refcnt" :depends-on ("package"))
                             ;;(:file "bytes" :depends-on ("package"))
                             ;;(:file "byte-array" :depends-on ("package"))
                             ;;(:file "tuple" :depends-on ("package"))
                             ;;(:file "long" :depends-on ("package"))
                             ;;(:file "error" :depends-on ("exception"))
                             ;;(:file "init" :depends-on ("package"))
                             ;;(:file "sys" :depends-on ("package"))
                             ;;(:file "unicode" :depends-on ("package"))
                             ;;(:file "call" :depends-on ("object"))
                             ;;(:file "module" :depends-on ("object"))
                             ;;(:file "import" :depends-on ("unicode"))
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
               (:file "unicode" :depends-on ("package"))
               (:file "object" :depends-on ("package"))
               (:file "set" :depends-on ("package"))
               (:file "dict" :depends-on ("package"))
               (:file "buffer" :depends-on ("package"))
               (:file "import" :depends-on ("package"))
               (:file "object-call" :depends-on ("package"))
               (:file "smart" :depends-on ("package")))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clpy :clpy.test))))
