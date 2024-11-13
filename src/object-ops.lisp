(in-package :clpy.object)

(defun size (o)
 (py:ensure-non-negative
     (clpy.ffi.fns:py-object-size o)
   (error 'py.exc:python-error)))

(defun get-item (o key)
 "Return element of O corresponding to the object KEY or nil on failure."
 (py:ensure-null-as-nil
     (clpy.ffi.fns:py-object-get-item o key)
   (error 'py.exc:python-error)))

(defun set-item (o key v)
 "Map the object KEY to the value V."
 (py:ensure-zero
     (clpy.ffi.fns:py-object-set-item o key v)
   (error 'py.exc:python-error)))

(defun del-item (o key)
 "Remove the mapping from the object KEY from the object O."
 (py:ensure-non-negative
     (clpy.ffi.fns:py-object-del-item o key)
   (error 'py.exc:python-error)))
