
Caveats
=======

High-level Object Abstraction
-----------------------------

In CLthon, the macros are built mainly on the PyObject protocol. That means that
the created PyObject won't be distinguished based on their type. For example,
for a list or a dictionary object, the specific function provided by C API for
more performant access won't be applied to them.


Module Import
-------------

The :cl:macro:`from` macro will not be able to find the submodule if the
submodule is not automatically imported in the module. An example is that
``pyplot`` is not automatically imported when ``matplotlib`` is imported. If you
use:

.. code:: common-lisp

   (from |matplotlib| import ((|pyplot| :as plt)))

The interpreter will signal that ``matplotlib`` has no attribute named
``pyplot`` because the above reason.
