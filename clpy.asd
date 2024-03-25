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
                             (:file "package" :depends-on ("ffi"))
                             (:file "bool" :depends-on ("package"))
                             (:file "exception" :depends-on ("ffi"))
                             (:file "object" :depends-on ("package"))
                             (:file "refcnt" :depends-on ("package"))
                             (:file "bytes" :depends-on ("package"))
                             (:file "byte-array" :depends-on ("package"))
                             (:file "tuple" :depends-on ("package"))
                             (:file "long" :depends-on ("package"))
                             (:file "error" :depends-on ("exception"))
                             (:file "init" :depends-on ("package"))
                             (:file "sys" :depends-on ("package"))
                             (:file "unicode" :depends-on ("package"))
                             (:file "call" :depends-on ("object"))
                             (:file "module" :depends-on ("object"))
                             (:file "import" :depends-on ("unicode"))
                             )))
  :in-order-to ((test-op (test-op :clpy.test/test))))

(asdf:defsystem "clpy/test"
  :depends-on ("cl-autowrap"
               "cl-plus-c"
               "fiveam"
               "cffi")
  :pathname "t/"
  :components ((:file "package")
               (:file "basic" :depends-on ("package")))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clpy.test :clpy/test))))
