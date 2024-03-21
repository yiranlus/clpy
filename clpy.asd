(asdf:defsystem "clpy"
  :depends-on ("cl-autowrap"
               "cl-autowrap/libffi"
               "cl-plus-c"
               "sdl2"
               "cffi")
  :components ((:module clpy-inc
                :pathname "include"
                :components ((:static-file "Python.h")))
               (module clpy-spec
                :pathname "spec")
               (:module clpy-lib
                :pathname "lib")
               (:module "src"
                :components ((:file "ffi")
                             (:file "library")))))
