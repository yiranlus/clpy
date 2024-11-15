(in-package :clpy.object)

(defun p (obj)
  (typep obj 'clpy.ffi:py-object))

(clpy.smart:new-hook #'p
                       (lambda (x)
                         (inc-xref x)
                         x))

;; PyObject basic operations

(defun ob-refcnt (obj)
  "Return ob_refcnt of the PyObject."
  (clpy.ffi.acc:py-object.ob-refcnt obj))

(defun ob-type (obj)
  "Return ob_type of the PyObject."
  (let ((pto (clpy.ffi.acc:py-object.ob-type obj)))
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
