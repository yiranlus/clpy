
Import Modules
==============

Import a module composed of a single :cl:macro:`from`, you should write in form
of

.. code:: common-lisp

   (from <module> [:import <attrs>])

``<module>`` should be a Lisp symbol. It is optional to specify the attributes
to be imported; in this case, only the module will be imported. Since Common
Lisp ignore the case of letters by default, it is important to enclose the
module name by ``|``. For example

.. code:: common-lisp

   (from |numpy|)
   ;; equivalent to `import numpy`

   (from |matplotlib.pyplot| :import (|xlabel| |ylabel|))
   ;; equivalent to `from matplotlib.pyplot import xlabel, ylabel

If you don't use ``|``, the module name will be assumed all uppercase. For
example,

.. code:: common-lisp

   (from numpy)
   ;; equivalent to `import NUMPY`
   ;; which will lead to an import error in Python

By default, if only the module is imported, CLthon will save this module with
its corresponding symbol in the global registry, you can access it using
:cl:macro:`@` easily. Otherwise, you can supply an alias using ``:as`` keyword:

.. code:: common-lisp

   (from (|numpy| :as np))
   (from (|matplotlib.pyplot| :as plt))

This is the same for the imported objects,

.. code:: common-lisp

   (from |matplotlib.pyplot| :import ((|xlabel| :as xlb)
                                      (|ylabel| :as ylb)))
