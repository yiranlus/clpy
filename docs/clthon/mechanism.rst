
Mechanism
=========

CLthon uses a global registry to trace the created PyObjects. This registry
stores a global name and associated PyObjects, either module, submodule or a
method. The code in :cl:macro:`code` is surrounded by automatically
initialization and finalization of the Python interpreter.
