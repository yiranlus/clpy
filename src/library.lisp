(defpackage :clpy.library
  (:use :cl))

(in-package :clpy.library)

(cffi:define-foreign-library libpython
  (:unix (:or "libpython3.so.0" "libpython3.so.1" "libpython3.so"))
  (:windows "python3.dll")
  (t (:default "libpython3")))

(cffi:use-foreign-library libpython)
