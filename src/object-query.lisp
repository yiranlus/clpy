(cl:in-package :clpy.object)

(defun is-instance (o cls)
  "Return T if the class derived is identical to or derived from the class CLS, otherwise return 0. CLS can be keyword registered in PY.TYPE:*ASSOC-TYPES*."
  (let ((-cls (if (keywordp cls) (py.type:get cls) cls)))
    (case (clpy.ffi.fns:py-object-is-instance o -cls)
      (1 t)
      (0 nil)
      (-1 (error 'py.exc:python-error)))))

(defun is-true (o)
  "Return T if the object o is considered to be true, otherwise NIL"
  (not (zerop
        (clpy.util:ensure-non-negative
            (clpy.ffi.fns:py-object-is-true o)
          (error 'py.exc:python-error)))))

(defun is-false (o)
  "Return NIL if the object o is considered to be true, otherwise t"
  (not (zerop
        (clpy.util:ensure-non-negative
            (clpy.ffi.fns:py-object-not o)
          (error 'py.exc:python-error)))))

