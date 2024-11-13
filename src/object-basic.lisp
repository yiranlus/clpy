(in-package :clpy.object)

(defun object-p (obj)
  (typep obj 'clpy.ffi:py-object))

;; PyObject basic operations

(defun ob-refcnt (obj)
  "Return ob_refcnt of the PyObject."
  (c-ref obj clpy.ffi:py-object :ob-refcnt))

(defun ob-type (obj)
  "Return ob_type of the PyObject."
  (let ((pto (c-ref obj clpy.ffi:py-object :ob-type)))
    (or (clpy.type:from pto) pto)))

(defun new-ref (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-new-ref o)))

(defun inc-ref (o)
  (clpy.ffi.fns:py-inc-ref o))

(defun dec-ref (o)
  "Release a strong reference to object O."
  (clpy.ffi.fns:py-dec-ref o))

;; X versions

(defun new-xref (o)
  (unless (or (null o)
	      (cffi:null-pointer-p (autowrap:ptr o)))
    (new-ref o)))

(defun inc-xref (o)
  (unless (or (null o)
	      (cffi:null-pointer-p (autowrap:ptr o)))
    (inc-ref o)))

(defun dec-xref (o)
  (unless (or (null o)
	      (cffi:null-pointer-p (autowrap:ptr o)))
    (dec-ref o)))
