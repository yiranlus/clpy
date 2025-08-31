
Attribute Access (``@``)
========================

To facilite the access to the attributes or the item easily in an object, CLthon
have an :cl:macro:`@`. It receives zero or multiple attributes:

.. code:: common-lisp

   (@ obj [attr ...])

If there is no attributes specified, this is will return the object it self.
The first argument should be the object you want to access. There is a special
case where the first object given is an atom, then the macro will expand to
the object saved in the registry. For example, if you have imported ``numpy``
using :cl:macro:`from`, you can access the module using

.. code:: common-lisp

   (@ 'numpy)

Pay attention to the single quote before ``numpy``. If you don't use it, the
macro will simply search the variable named ``numpy`` in the current scope.

To access the attribute, you should give the name enclosed in ``|``; to access
the item, you should give it in a list. For example,

.. code:: common-lisp

   (@ 'plt |xlable|)
   ;; give `plt.xlable`
   ;; where `plt` is searched in the global registry
   (@ a |b| |c|)
   ;; give `a.b.c`
   (@ a (i) x)
   ;; give `a[i].X`,
   ;; the uppercase is due to case-insensitivity of Common Lisp


You can also use setf to change the value of the attributes or items. For
example,

.. code:: common-lisp

   (setf (@ a (1)) 100)
   ;; equivalent to `a[1] = 100`
   (setf (@ 'plt |rcParams| ("figure.dpi")) 150)
   ;; equivalent to `plt.rcParams["figure.dpi"] = 1001`
