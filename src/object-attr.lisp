(cl:in-package :clpy.object)

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
    (error 'py.exc:python-error :type :attribute-error)))

(defun set-attr (o attr-name v)
  "Set the value of the attribute named ATTR-NAME, for object o,to the value v."
  (clpy.util:ensure-zero
      (if (stringp attr-name)
          (clpy.ffi.fns:py-object-set-attr-string o attr-name v)
          (clpy.ffi.fns:py-object-set-attr o attr-name v))
    (error 'py.exc:python-error :type :attribute-error)))

;; Generic Atributes

;;(defun generic-get-attr (o name)
;;  "Generic attribute getter function that is meant to be put into a type object's ``tp_getattro'' slot"
;;  (py:ensure-null-as-nil
;;    (clpy.ffi.fns:py-object-generic-get-attr o name)))

;;(defun generic-set-attr (o name v)
;;  "Generic attribute setter function that is meant to be put into a type object's ``tp_setattro'' slot"
;;  (py:ensure-zero
;;      (clpy.ffi.fns:py-object-generic-set-attr o name v)
;;    error 'py.exc:attribute-error))
;;
;;(defun generic-get-dict (o &optional context)
;;  "A generic implementation for the getter of a ``__dict''descriptor. It creates the dictionary if necessary."
;;  (py:ensure-null-as-nil
;;      (clpy.ffi.fns:py-object-generic-get-dict o context)
;;    (error 'py.exc:attribute-error)))
;;
;;(defun generic-set-dict (o value &optional context)
;;  "A generic implementation for the setter of a ``__dict''descriptor. This implementation does not allow the dictionary to be deletec"
;;  (py:ensure-zero
;;      (clpy.ffi.fns:py-object-generic-set-dict o value context)
;;    (error 'py.exc:attribute-error)))

(defun dir (&optional (o nil))
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-object-dir o)
    (when (clpy.core:error-occurred)
      (error 'py.exc:generic-error))))
