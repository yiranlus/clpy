(in-package :clpy)

(defun new-ref (o)
  (py:ensure-null-as-nil
    (clpy.ffi.fns:py-new-ref o)))

(defun inc-ref (o)
  (clpy.ffi.fns:py-inc-ref o))

(defun dec-ref (o)
  "Release a strong reference to object O."
  (clpy.ffi.fns:py-dec-ref o))
