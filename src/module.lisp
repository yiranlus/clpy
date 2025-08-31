(defpackage :clpy.module
  (:nicknames :py.mod)
  (:use :cl)
  (:export #:p
           #:exact-p
           #:new

           #:set-doc-string
           #:get-dict
           #:get-name
           #:get-state
           #:get-def
           #:get-filename

           #:add-object
           #:add-constant
           #:add-type))

(in-package :clpy.module)

(clpy.type:define-type "PyModule_Type" module)

(defun p (o)
  (or (clpy.type:of o :module)
      (clpy.type:subtype-p (clpy.object:ob-type o)
                           (clpy.type:get :module))))

(defun exact-p (o)
  (clpy.type:of o :module))


(defun new (name)
  (clpy.util:ensure-null-as-nil
   (clpy.util:let ((-name (clpy.str:new name)))
     (clpy.ffi.fns:py-module-new-object -name))
   (clpy.exception:raise-generic-or-python-error
    :message "Unable to create module.")))

(defun set-doc-string (module docstring)
  (clpy.util:ensure-zero
   (clpy.ffi.fns:py-module-set-doc-string module docstring)
   (clpy.exception:raise-generic-or-python-error)))


(defun get-dict (module)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-module-get-dict module)
   (clpy.exception:raise-generic-or-python-error)))

(defun get-name (module &optional as-object)
  (clpy.util:ensure-null-as-nil
   (if as-object
       (clpy.ffi.fns:py-module-get-name-object module)
       (clpy.ffi.fns:py-module-get-name module))))

(defun get-state (module)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-module-get-state module)))

(defun get-def (module)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-module-get-def module)))

(defun get-filename (module &optional as-object)
  (clpy.util:ensure-null-as-nil
   (if as-object
       (clpy.ffi.fns:py-module-get-filename-object module)
       (clpy.ffi.fns:py-module-get-filename module))))

;; Support functions

(defun add-object (module name value &key (steal nil))
  (clpy.util:ensure-non-negative
   (if steal
       (clpy.ffi.fns:py-module-add-object module name value)
       (clpy.ffi.fns:py-module-add-object-ref module name value))
   (clpy.exception:raise-generic-or-python-error
    :message "Failed to add object to the module.")))

(defun add-constant (module name value)
  (clpy.util:ensure-non-negative
   (cond
     ((integerp value) (clpy.ffi.fns:py-module-add-int-constant module name value))
     ((stringp value) (clpy.ffi.fns:py-module-add-string-constant module name value)))
   (clpy.exception:raise-generic-or-python-error
    :message "Failed to add constant to the module.")))

(defun add-type (module type)
  (clpy.util:ensure-non-negative
   (clpy.ffi.fns:py-module-add-type module type)
   (clpy.exception:raise-generic-or-python-error
    :message "Failed to add type to the module.")))
