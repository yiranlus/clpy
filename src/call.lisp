(in-package :clpy)

(defun callable-check (o)
  (plusp (clpy.ffi.fns:py-callable-check o)))

(in-package :clpy.object)

(defun call (callable args)
  (py:ensure-null-as-nil
    (clpy.ffi.fns:py-object-call-object callable args)))
