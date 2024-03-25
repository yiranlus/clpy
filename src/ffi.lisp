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
                                 "_Py.*_Va"
                                 "^Py.+V$"
                                 "vsnprintf$")
           :symbol-exceptions (("PyOS_FSPath" . "PY-OS-FS-PATH")
                               ("PyObject_ASCII" . "PY-OBJECT-ASCII")
                               ("PyUnicode_AsASCIIString" . "PY-UNICODE-AS-ASCII-STRING")
                               ("PyUnicode_CompareWithASCIIString" . "PY-UNICODE-COMPARE-WITH-ASCII-STRING")
                               ("PyUnicode_DecodeFSDefault" . "PY-UNICODE-DECODE-FS-DEFAULT")
                               ("PyUnicode_EncodeFSDefault" . "PY-UNICODE-ENCODE-FS-DEFAULT")
                               ("PyUnicode_DecodeFSDefaultAndSize" . "PY-UNICODE-DECODE-FS-DEFAULT-AND-SIZE")
                               ("PyUnicode_DecodeASCII" . "PY-UNICODE-DECODE-ASCII")
                               ("PyUnicode_FSConverter" . "PY-UNICODE-FS-CONVERTER")
                               ("PyUnicode_FSDecoder" . "PY-UNICODE-FS-DECODER"))
           :sysincludes '("/usr/include/python3.11/")
           :exclude-sources ("/usr/include/"
                             "/usr/lib/")
           :include-sources ("/usr/include/python3.11/"
                             "stdint.h"
                             "unistd.h"
                             "stddef.h"
                             "/usr/include/.*/bits/types.h"
                             "/usr/include/.*/bits/stdint-uintn.h"
                             "/usr/include/.*/bits/stdint-intn.h"
                             "/usr/include/.*/sys/types.h")
           :no-accessors cl:nil)
