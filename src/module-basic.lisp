(in-package :clpy.module)

(clpy.type:define-type "PyModule_Type" module)

(defun p (o)
  (clpy.type:of o :module))

(defun new (name)
  (py:ensure-null-as-nil
      (if (stringp name)
          (clpy.ffi.fns:py-module-new name)
          (clpy.ffi:fns:py-module-new-object name))
    (error 'py.exc:generic-error)))

(defun get-dict (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-module-get-dict o)
    (error 'py.exc:generic-error)))

(defun get-name (o &optional as-object)
  (py:ensure-null-as-nil
      (if as-object
          (clpy.ffi.fns:py-module-get-name-object o)
          (clpy.ffi.fns:py-module-get-name o))
    (error 'py.exc:generic-error)))

(defun get-state (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-module-get-state o)
    (error 'py.exc:generic-error)))

(defun get-def (o)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-module-get-def o)
    (error 'py.exc:generic-error)))

(defun get-filename (o &optional as-object)
  (py:ensure-null-as-nil
      (if as-object
          (clpy.ffi.fns:py-module-get-filename-object o)
          (clpy.ffi.fns:py-module-get-filename o))
    (error 'py.exc:generic-error)))

