(in-package :clpy.object)

(defun p (o)
  "Check if `O` is a PyObject."
  (typep o 'clpy.ffi:py-object))

;; PyObject basic operations

(defun ob-refcnt (obj)
  "Return ob_refcnt of the PyObject."
  (clpy.ffi.acc:py-object.ob-refcnt obj))

(defun ob-type (obj)
  "Return ob_type of the PyObject."
  (let ((pto (clpy.ffi.acc:py-object.ob-type obj)))
    (or (clpy.type:from pto) pto)))

(defun new-ref (o)
  "Create a strong reference to `O` and return it."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-new-ref o)))

(clpy.smart:new-hook #'p #'new-ref)

(defun inc-ref (o)
  "Indicate taking a new strong reference to `O`."
  (clpy.ffi.fns:py-inc-ref o))

(defun dec-ref (o)
  "Release a strong reference to object O."
  (clpy.ffi.fns:py-dec-ref o))

;; X versions

(defun new-xref (o)
  "Similar to :cl:function:`new-ref`, but the object `O` can be NULL.

This is equivalent to Python's `Py_XNewRef`."
  (unless (or (null o)
	      (cffi:null-pointer-p (autowrap:ptr o)))
    (new-ref o)))

(defun inc-xref (o)
  "Similar to :cl:function:`inc-ref`, but the object `O` can be NULL.

This equivalent to Python's `Py_XINCREF."
  (unless (or (null o)
	      (cffi:null-pointer-p (autowrap:ptr o)))
    (inc-ref o)))

(defun dec-xref (o)
    "Similar to :cl:function:`dec-ref`, but the object `O` can be NULL.

This equivalent to Python's `Py_XDECCREF."
  (unless (or (null o)
	      (cffi:null-pointer-p (autowrap:ptr o)))
    (dec-ref o)))

;; Some hacks: Define None and NotImplemented

(defun not-implemented ()
  (new-ref (plus-c:c-ref
	    (cffi:foreign-symbol-pointer "_Py_NotImplementedStruct")
	    clpy.ffi:py-object)))
