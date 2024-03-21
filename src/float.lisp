(defpackage clpy.float
  (:use :cl)
  (:import-from :clpy.ffi.acc
                #:float-type)
  (:import-from :clpy.ffi.fns
                #:float-check
                #:float-from-string
                #:float-from-double
                #:float-get-info
                #:float-get-max
                #:float-get-min))

(in-package :clpy.float)
