
Function Calling
================

CLthon provide two macro's to call the functions: :cl:macro:`call` and
:cl:macro:`ncall`. The former will call the function and return the result
as PyObject. The latter is similar to the former except that the returned value
if discarded after called. It is suitable to call function where the returned
value is ``None``.

The syntax is as follows:

.. code:: common-lisp

   (call  obj [arg-list kwarg-dict])
   (ncall obj [arg-list kwarg-dict])

Similar to :cl:macro:`@`, the first argument can be an atom which indicates an
global registered object; or a variable. ``arg-list`` is for positional
arguments and ``kwarg-dict`` is for keyword argument. For example,

.. code:: common-lisp

   (ncall f)
   ;; equivalent to `f()`
   (ncall f (1 2))
   ;; equivalent to `f(1, 2)`
   (ncall f (1) (|c| . 20))
   ;; equivalent to `f(1, c=20)`

You can combine with :cl:macro:`@` to call the method object easily.

.. code:: common-lisp

   (ncall (@ 'plt |plot|) (x y) ((|linestyle| . "--")
                                 (|linecolor| . "red")))
   (ncall (@ 'plt |xlabel|) ("X axis"))

if ``obj`` is not a variable, the ``obj`` will be automatically dereferenced
after the call. So the object created inside :cl:macro:`call` and
:cl:macro:`ncall` (not variable, i.e., single atom) doesn't need to be
dereferenced automatically.
