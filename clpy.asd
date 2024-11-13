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
                :components ((:file "utils")
			     
			     (:file "library")
                             (:file "ffi" :depends-on ("library"))
			     (:file "object" :depends-on ("ffi"))

                             (:file "package" :depends-on ("ffi" "utils" "object"))
			     (:file "runtime" :depends-on ("package"))
			     (:file "initfinal" :depends-on ("package"))

			     (:file "pylet" :depends-on ("package" "object"))
			     
                             (:file "types" :depends-on ("ffi"))
                             (:file "exception" :depends-on ("ffi"))
                             (:file "error" :depends-on ("exception" "utils"))
                             (:file "number" :depends-on ("ffi" "types" "utils"))
			     
			     (:file "object-basic" :depends-on ("package" "object" "types"))
			     (:file "object-attr" :depends-on ("object" "utils" "error"))
			     (:file "object-repr" :depends-on ("object" "utils" "error"))
			     (:file "object-ops" :depends-on ("object" "utils" "error"))
			     (:file "object-compare" :depends-on ("object" "utils" "error"))
			     
			     (:file "bool" :depends-on ("types" "utils" "error"))
			     (:file "list" :depends-on ("types" "utils" "error"))
			     (:file "bytes" :depends-on ("types" "utils" "error"))
			     (:file "unicode" :depends-on ("types" "utils" "error"))
			     (:file "set" :depends-on ("types" "utils" "pylet" "error"))
			     (:file "dict" :depends-on ("types" "utils" "pylet" "sequence" "error"))
			     
			     (:file "sequence" :depends-on ("types" "utils" "pylet"))
			     (:file "mapping" :depends-on ("types" "utils" "pylet" "object"))
			     (:file "smart" :depends-on ("package" "number" "list" "unicode"))
                             
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
	       (:file "list" :depends-on ("package"))
	       (:file "unicode" :depends-on ("package"))
	       (:file "object" :depends-on ("package"))
	       (:file "set" :depends-on ("package"))
	       (:file "dict" :depends-on ("package"))
	       (:file "smart" :depends-on ("package")))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clpy :clpy.test))))
