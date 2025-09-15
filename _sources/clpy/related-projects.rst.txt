
Related Projects
================

There are several related projects before which are intended to bridge the
Python and Common Lisp:

* `py4cl`_: enables the communication between Common Lisp and Python through
  streaming. Python runs in a separate process.
* `py4cl2`_: an improvement to the original `py4cl` using the same mechanism.
* `burgled-batteries`_: using Python C API to run Python code. It uses FFI to
  run Python code, similar to what CLPy does. However, it hasn't been updated
  for a long time and not that complete.
* `CLPython`_: a Python implementation in Common Lisp. However, it is not fully
  compatible with CPython, so you can't run all the Python code.

CLPy is in early development, and I tried to cover the complete Python's Limited
API. However, I can't promise that this package is compatible with all versions
of Python. Currently, the version of Python C API I use is 3.11. After the core
functions are complete, I will gradually continue to support other version.

.. _`py4cl`: https://github.com/bendudson/py4cl/
.. _`py4cl2`: https://github.com/digikar99/py4cl2
.. _`burgled-batteries`: https://github.com/pinterface/burgled-batteries
.. _`CLPython`: https://clpython.common-lisp.dev/
