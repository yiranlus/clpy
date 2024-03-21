(defpackage :clpy.library
  (:use :cl))

(in-package :clpy.library)

(cffi:define-foreign-library libpython
  (:unix (:or "libpython3.11.so.0" "libpython3.11.so.1" "libpython3.11.so"))
  (:windows "python3.11.dll")
  (t (:default "libpython3.11")))

(cffi:use-foreign-library libpython)
