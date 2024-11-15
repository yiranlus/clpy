(defpackage :clpy.smart
  (:nicknames :py.smart)
  (:use :cl)
  (:shadow #:print)
  (:export #:new
           #:new-hook
           #:print
           #:print-hook))
