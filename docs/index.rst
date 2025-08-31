==================
CLPy Documentation
==================

CLPy is a Common Lisp wrapper of `Python Limited API`_. It use `cl-autowrap`_ to
generate the FFI bindings and then rewrap these functions to a more Lisper way.
CLPy also comes with a :doc:`CLthon <clthon/index>`, which is a set of macros that
facilitate writing Python-like scripts in Common Lisp.


.. _`cl-autowrap`: https://github.com/rpav/cl-autowrap/
.. _`Python Limited API`: https://docs.python.org/3.11/c-api/stable.html#limited-c-api

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   clpy/index
   clthon/index
   references/clpy/index
   references/clthon/index
