(in-package :clpy.object)

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

;; Comparison

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


(defun true-p (o &optional (exact-p nil))
  "Test if ``O`` is True.

Return ``T`` if the object ``O`` is considered to be true. If EXACT-P
is ``T``, then this is equivalent to ``o is True``."
  (plusp
   (clpy.util:ensure-non-negative
    (if exact-p
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


(defun not (o)
  "Return NIL if the object ``O`` is considered to be true, otherwise ``T``"
  (plusp
   (clpy.util:ensure-non-negative
       (clpy.ffi.fns:py-object-not o)
     (clpy.exception:raise-generic-or-python-error))))

;; Representations

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
