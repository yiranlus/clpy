(in-package :clpy.object)

(defconstant +GENERIC-GET-ATTR+
  (cffi:foreign-symbol-pointer "PyObject_GenericGetAttr"))
(defconstant +GENERIC-SET-ATTR+
  (cffi:foreign-symbol-pointer "PyObject_GenericSetAttr"))

(defconstant +GENERIC-GET-DICT+
  (cffi:foreign-symbol-pointer "PyObject_GenericGetDict"))
(defconstant +GENERIC-SET-DICT+
  (cffi:foreign-symbol-pointer "PyObject_GenericSetDict"))

;; Initx

(defun init (o type)
  (clpy.ffi.fns:py-object-init o type))

(defun init-var (vo type size)
  (clpy.ffi.fns:py-object-init-var vo type size))

;; Attributes

(defun has-attr (o attr-name)
  "Retrieve an attribute named ATTR-NAME. ATTR-NAME can be a py-object or a string."
  (plusp
   (if (stringp attr-name)
       (clpy.ffi.fns:py-object-has-attr-string o attr-name)
       (clpy.ffi.fns:py-object-has-attr o attr-name))))


(defun get-attr (o attr-name)
  "Retrieve an attribute named ATTR-NAME. ATTR-NAME can be a py-object or a string"
  (clpy.util:ensure-null-as-nil
      (if (stringp attr-name)
          (clpy.ffi.fns:py-object-get-attr-string o attr-name)
          (clpy.ffi.fns:py-object-get-attr o attr-name))
    (clpy.exception:raise-generic-or-python-error)))


(defun set-attr (o attr-name v)
  "Set the value of the attribute named ATTR-NAME, for object o,to the value v."
  (clpy.util:ensure-zero
      (if (stringp attr-name)
          (clpy.ffi.fns:py-object-set-attr-string o attr-name v)
          (clpy.ffi.fns:py-object-set-attr o attr-name v))
    (clpy.exception:raise-generic-or-python-error)))

(defun get-dict (o &optional context)
  "A generic implementation for the getter of a ``__dict__`` descriptor.

It creates the dictionary if necessary. This function may need to
allocate memory for the dictionary, it may be more efficient to call
:cl:function:`get-attr` when accessing an attribute on the object."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-generic-get-dict o context)
    (clpy.exception:raise-generic-or-python-error)))

(defun set-dict (o value &optional context)
  "A generic implementation for the settof of a ``__dict__`` descriptor."
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-object-generic-set-dict o value context)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-item (o key)
  "Return element of o corresponding to the object KEY."
  (clpy.util:ensure-null-as-nil
      (clpy.util:let ((-key (clpy.smart:new key)))
        (clpy.ffi.fns:py-object-get-item o -key))
    (clpy.exception:raise-generic-or-python-error)))

(defun set-item (o key v)
  "Map the object key to the value."
  (clpy.util:ensure-non-negative
      (clpy.util:let ((-key (clpy.smart:new key))
                      (-v (clpy.smart:new v)))
        (clpy.ffi.fns:py-object-set-item o -key v))
    (clpy.exception:raise-generic-or-python-error)))

(defun del-item (o key)
  "Return element of o corresponding to the object KEY."
  (clpy.util:ensure-null-as-nil
      (if (stringp key)
          (clpy.ffi.fns:py-object-del-item-string o key)
          (clpy.ffi.fns:py-object-del-item o key))
    (clpy.exception:raise-generic-or-python-error)))


(defun dir (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-dir o)
    (clpy.exception:raise-generic-or-python-error)))

;; Comparison

(defun is-subclass (derived cls)
  (plusp
   (clpy.util:ensure-non-negative
       (clpy.ffi.fns:py-object-is-subclass derived cls)
     (clpy.exception:raise-generic-or-python-error))))

(defun is-instance (o cls)
  "Return ``T`` if the class derived is identical to or derived from
the class CLS, otherwise return 0. CLS can be keyword registered in
CLPY.TYPE:*ASSOC-TYPES*."
  (let ((-cls (if (keywordp cls) (py.type:get cls) cls)))
    (case (clpy.ffi.fns:py-object-is-instance o -cls)
      (1 t)
      (0 nil)
      (-1 (clpy.exception:raise-generic-or-python-error)))))

(defun is (o1 o2)
  "Test if the object O1 is the object O2.

This is equivalent to ``o1 is o2`` in Python."
  (plusp (clpy.ffi.fns:py-is o1 o2)))


(defun true-p (o &key (exact nil))
  "Test if ``O`` is True.

Return ``T`` if the object ``O`` is considered to be true. If EXACT-P
is ``T``, then this is equivalent to ``o is True``."
  (plusp
   (clpy.util:ensure-non-negative
       (if exact
           (clpy.ffi.fns:py-is-true o)
           (clpy.ffi.fns:py-object-is-true o))
     (clpy.exception:raise-generic-or-python-error))))


(defun none-p (o)
  "Test if the object ``O`` is None.

This is equivalent to ``o is None`` in Python."
  (plusp (clpy.ffi.fns:py-is-none o)))


(defun false-p (o)
  "Test if the object ``O`` is False.

This is equivalent to ``o is False`` in Python."
  (plusp (clpy.ffi.fns:py-is-false o)))

(defun is (o1 o2)
  "Utility function to minic Python's ``is`` operator.

O1 should be an PyObject. O2 can be another PyObject, :NONE, :TRUE or :FALSE."
  (case o2
    (:none (none-p o1))
    (:true (true-p o1 :exact t))
    (:false (false-p o2))
    (otherwise (cffi:pointer-eq (autowrap:ptr o1)
                                (autowrap:ptr o2)))))


(defun not (o)
  "Return NIL if the object ``O`` is considered to be true, otherwise ``T``"
  (plusp
   (clpy.util:ensure-non-negative
       (clpy.ffi.fns:py-object-not o)
     (clpy.exception:raise-generic-or-python-error))))

(defun rich-compare (left right op &optional as-object)
  "Rich compare two objects.

The return value can be ``T``, NIL, or Py_NotImplemented. Possible
values for OP are :GT, :GE, :EQ, :NE, :LT, :LE."
  (let ((py-op (case op
                 (:GT clpy.type:+PY-GT+)
                 (:GE clpy.type:+PY-GE+)
                 (:EQ clpy.type:+PY-EQ+)
                 (:NE clpy.type:+PY-NE+)
                 (:LT clpy.type:+PY-LT+)
                 (:LE clpy.type:+PY-LE+))))

    (if as-object
        (clpy.util:ensure-null-as-nil
            (clpy.ffi.fns:py-object-rich-compare left right py-op)
          (clpy.exception:raise-generic-or-python-error))
        (clpy.util:ensure-non-negative
            (clpy.ffi.fns:py-object-rich-compare-bool left right py-op)
          (clpy.exception:raise-generic-or-python-error)))))

(defun len (o)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-object-length o)))

(defun type (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-type o)
    (clpy.exception:raise-generic-or-python-error)))

;; Representations

(defun format (o format-spec)
  "Compute a string representation of the object. This generates a string similar to that returned by ``repr()''"
  (clpy.util:ensure-null-as-nil
      (clpy.util:let ((-format-spec (clpy.str:new format-spec)))
        (clpy.ffi.fns:py-object-format o -format-spec)
        (clpy.exception:raise-generic-or-python-error))))

(defun repr (o &key only-ascii)
  "Compute a string representation of the object. This generates a string similar to that returned by ``repr()''"
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-repr o)
    (clpy.exception:raise-generic-or-python-error)))

(defun ascii (o)
  "Compute a string representation of the object. This generates a string similar to that returned by ``ascii()''"
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-ascii o)
    (clpy.exception:raise-generic-or-python-error)))

(defun str (o)
  "Compute a string representation of the object. This is the equivalent of the Python expresssion ``str(o)''. Return nil when fails."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-str o)
    (clpy.exception:raise-generic-or-python-error)))

(defun bytes (o)
  "Compute a bytesing representation of the object. This is the equivalent of the Python expresssion ``bytes(o)''. Return nil when fails."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-bytes o)
    (clpy.exception:raise-generic-or-python-error)))

(defun hash (o)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-object-hash o)
    (clpy.exception:raise-generic-or-python-error)))

(defun hash-not-implemented (o)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-object-hash-not-implemented o)
    (clpy.exception:raise-generic-or-python-error)))

;; Iterators

(defun self-iter (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-self-iter o)
    (clpy.exception:raise-generic-or-python-error)))


(defun get-iter (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-get-iter o)
    (clpy.exception:raise-generic-or-python-error)))

(defun get-a-iter (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-get-a-iter o)
    (clpy.exception:raise-generic-or-python-error)))

(defun as-fd (o)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-object-as-file-descriptor o)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to get the file descriptor from the object.")))

;; Memory Management

(defun clear-weakrefs (o)
  (clpy.ffi.fns:py-object-clear-weak-refs o))

(defun malloc (n)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-malloc n)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to allocate object")))

(defun calloc (nelem elsize)
  "Allocates NELEM elements each whose size in bytes is ELSIZE."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-calloc nelem elsize)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to allocate object")))

(defun realloc (p n)
  "Resizes the memory block pointed to by ``P`` to ``N`` bytes."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-realloc p n)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to allocate object")))

(defun free (p)
  (clpy.ffi.fns:py-object-free p))
