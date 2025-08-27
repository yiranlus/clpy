(defpackage :clpy.state
  (:nicknames :py.state)
  (:use :cl)
  (:export #:find-module
	   #:add-module
	   #:remove-module))

(defun find-module (def)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-state-find-module def)))

(defun add-module (module def)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-state-add-module module def)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to add the module.")))

(defun remove-module (def)
  (clpy.util:ensure-zero
      (clpy.ffi.fns:py-state-remove-module def)
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to remove the module.")))
