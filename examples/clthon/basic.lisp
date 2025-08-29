(require 'clthon)

;; Feature planned
;; * heredoc, using cl-heredoc

(defpackage :clthon.examples
  (:use :cl :clthon))

(in-package :clthon.examples)

(sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow)
  (clthon:code (:finalize nil)
    (from (|numpy| :as np))))

(code (:finalize t)
  (from (|matplotlib.pyplot| :as plt))

  (format t "figure.dpi: ~A~%" (@ 'plt |rcParams| ("figure.dpi")))
  ;;(setf (@ 'plt |rcParams| ("figure.dpi")) 150)
  ;;(format t "changed figure.dpi: ~A~%" (@ 'plt |rcParams| ("figure.dpi")))

  (py:let* ((x (call (@ 'np |linspace|) (0 10 100)))
            (y (py.num:* x x)))
    ;;(format t "X: ~A~%" x)
    ;;(format t "Y: ~A~%" y)

    (ncall (@ 'plt |plot|) (x y) ((|label| . "y = 2x")
                                  (|linestyle| . "--")
                                  (|linewidth| . 2)))

    (ncall (@ 'plt |xlabel|) ("x-axis"))
    (ncall (@ 'plt |ylabel|) ("y-axis"))
    (ncall (@ 'plt |title|) ("simple line plot"))
    (ncall (@ 'plt |legend|))
    (ncall (@ 'plt |show|)))
  )
