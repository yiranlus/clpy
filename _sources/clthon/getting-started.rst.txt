Getting Started
===============

To use CLthon, you can create a package and import the CLthon symbols:

.. code:: common-lisp

   (defpackage :clthon.example
     (:use :cl :clthon)))

   (in-package :clthon.example)

Then you need to write you code in a ``CODE`` macro. Following is an example
code

.. code:: common-lisp

   (code (:finalize t)
     (sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow)
       (from (|numpy| :as np)))

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
       (ncall (@ 'plt |show|))))

This code will make a plot using `Matplotlib`_ from some `NumPy`_ arrays.

**Note:** Import `NumPy`_ using C API has some problem due to its initialization
procedure involving operations like multiplying large flots to reach infinity.
In order to avoid the errors to be raised, we need to disable the overflow
masking temporarily, that's why we need to use SBCL's
``with-float-traps-masked``.


.. _`Matplotlib`: https://matplotlib.org/
.. _`NumPy`: https://numpy.org/
