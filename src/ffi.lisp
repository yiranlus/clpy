(defpackage :clpy.ffi
  (:import-from :autowrap))

(defpackage :clpy.ffi.acc)
(defpackage :clpy.ffi.fns)

(in-package :clpy.ffi)

(autowrap:c-include '(clpy clpy-inc "Python.h")
           :spec-path '(clpy clpy-spec)
           :accessor-package :clpy.ffi.acc
           :function-package :clpy.ffi.fns
           :exclude-definitions ("^Py.*_Va"
                                 "^Py.+V$"
                                 "vsnprintf$")
           :symbol-exceptions (("PyObject_ASCII" . "OBJECT-ASCII")
                               ("PyUnicode_AsASCIIString" . "UNICODE-AS-ASCII-STRING")
                               ("PyUnicode_CompareWithASCIIString" . "UNICODE-COMPARE-WITH-ASCII-STRING")
                               ("PyUnicode_DecodeASCII" . "UNICODE-DECODE-ASCII"))
           :symbol-regex (("^Py(.*)"
                           ()
                           (cl:lambda (string matches regex)
                             (autowrap:default-c-to-lisp (cl:aref matches 0)))))
           :sysincludes '("/usr/include/python3.11/")
           :exclude-sources ("/usr/include/"
                             "/usr/lib/")
           :include-sources ("/usr/include/python3.11/"
                             "stdint.h"
                             "unistd.h"
                             "stddef.h"
                             "/usr/include/.*/bits/types.h"
                             "/usr/include/.*/bits/stdint-uintn.h"
                             "/usr/include/.*/sys/types.h")
           :no-accessors cl:t)
