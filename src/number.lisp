(defpackage :clpy.number
  (:nicknames :py.num)
  (:use :cl)
  (:shadow #:abs #:real #:+ #:- #:* #:/ #:// #:** #:/=
	   #:int #:float #:and #:or #:xor)
  (:export #:new
	   #:p
	   #:int-p
	   #:int-exact-p
	   #:float-p
	   #:float-exact-p
	   #:complex-p
	   #:complex-exact-p
	   #:as-double
	   #:as-integer
	   #:int
	   #:real
	   #:imag
	   #:abs
	   #:+ #:- #:* #:@ #:/ #:// #:** #:**% #:% #:~ #:divmod
	   #:and #:or #:xor #:<< #:>>
	   #:+= #:-= #:*= #:@= #:/= #://= #:**=  #:**%= #:%= #:~=
	   #:and= #:or= #:xor= #:<<= #:>>=))

(in-package :clpy.number)

(clpy.type:define-type "PyLong_Type" long)
(clpy.type:define-type "PyFloat_Type" float)
(clpy.type:define-type "PyComplex_Type" complex)

(defun int-p (o)
  (or (clpy.type:of o :long)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :long))))

(defun int-exact-p (o)
  (clpy.type:of o :long))

(defun float-p (o)
  (or (clpy.type:of o :float)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :float))))

(defun float-exact-p (o)
  (clpy.type:of o :float))

(defun complex-p (o)
  (or (clpy.type:of o :complex)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :complex))))

(defun complex-exact-p (o)
  (clpy.type:of o :complex))


(defun p (o)
  (plusp (clpy.ffi.fns:py-number-check o)))

(defun float-info ()
  (clpy.ffi.fns:py-float-get-info))

(defun float-min ()
  (clpy.ffi.fns:py-float-get-min))

(defun float-max ()
  (clpy.ffi.fns:py-float-get-max))
  

(defvar *py-long-from-binding-list*
  `((:long . ,#'clpy.ffi.fns:py-long-from-long)
    (:long-long . ,#'clpy.ffi.fns:py-long-from-long-long)
    (:unsigned-long . ,#'clpy.ffi.fns:py-long-from-unsigned-long)
    (:unsigned-long . ,#'clpy.ffi.fns:py-long-from-unsigned-long-long)
    (:size-t . ,#'clpy.ffi.fns:py-long-from-size-t)
    (:ssize-t . ,#'clpy.ffi.fns:py-long-from-ssize-t)
    (:double . ,#'clpy.ffi.fns:py-long-from-double)))

(defun new-long (value &key (method nil method-p) (base 0))
  (when (cl:and method-p (not (assoc method *py-long-from-binding-list*)))
    (error (format nil "Method ~A is not supported." method)))
  (clpy.util:ensure-null-as-nil
      (cond
	((assoc method *py-long-from-binding-list*)
	 (funcall (assoc method *py-long-from-binding-list*) value))
	((stringp value)
	 (clpy.ffi.fns:py-long-from-string value nil base))
	((floatp value) (clpy.ffi.fns:py-long-from-double value))
	((integerp value)
	 (let ((nbits (when (integerp value) (1+ (integer-length value)))))
           (cond
             ((<= nbits 32) (clpy.ffi.fns:py-long-from-long value))
             ((<= nbits 64) (clpy.ffi.fns:py-long-from-long-long value))))))))

(defun new (n &optional (type nil type-p))
  (clpy.util:ensure-null-as-nil
      (cond
	((stringp n)
	 (if (not type-p)
	     (error "TYPE must be specified when N is a string.")
	     (case type
	       (:float (clpy.ffi.fns:py-float-from-string n))
	       (:long (clpy.ffi.fns:py-long-from-string n))
	       (:complex (error "String of complex number is not supported."))
	       (otherwise (error (format nil "TYPE ~A is not found." type))))))
	((not type-p)
	 (cond
	   ((complexp n) (clpy.ffi.fns:py-complex-from-doubles (coerce (realpart n) 'double-float)
							       (coerce (imagpart n) 'double-float)))
	   ((floatp n) (clpy.ffi.fns:py-float-from-double (coerce n 'double-float)))
	   ((integerp n) (new-long n))
	   (t (error (format nil "non-supported type for ~A." n)))))
	(type-p
	 (case type
	   (:float (clpy.ffi.fns:py-float-from-double n))
	   (:complex (clpy.ffi.fns:py-complex-from-doubles (realpart n) (imagpart n)))
	   (otherwise (new-long n :method type)))))
    (error 'py.exc:generic-error :message (format nil "Unable to create PyObject for ~A." n))))

(defun as-double (o)
  (case (py.obj:ob-type o)
    (:float (clpy.ffi.fns:py-float-as-double o))
    (:long (clpy.ffi.fns:py-long-as-double o))
    (otherwise (error "Unsupported type to convert to double."))))

(defun real (o)
  (clpy.ffi.fns:py-complex-real-as-double o))

(defun imag (o)
  (clpy.ffi.fns:py-complex-imag-as-double o))

(defun as-integer (o &optional (type :long) (mask nil))
  (unless (clpy.type:of o :long)
    (error "O is not a PyLong."))
  (case type
    ;;(:int (clpy.ffi.fns:py-long-as-int 0))
    (:long (clpy.ffi.fns:py-long-as-long o))
    (:long-long (clpy.ffi.fns:py-long-as-long-long o))
    (:ssize-t (clpy.ffi.fns:py-long-as-ssize-t o))
    (:usigned-long
     (if mask
	 (clpy.ffi.fns:py-long-as-unsigned-long-mask o)
	 (clpy.ffi.fns:py-long-as-unsigned-long o)))
    (:unsigned-long-long
     (if mask
	 (clpy.ffi.fns:py-long-as-unsigned-long-long-mask o)
	 (clpy.ffi.fns:py-long-as-unsigned-long-long o)))
    (:size-t (clpy.ffi.fns:py-long-as-size-t o))))

;; smart

(clpy.smart:new-hook #'numberp #'new)
;; (clpy.smart:print-hook :long (lambda (x) (princ-to-string (as-integer x)))
;; (clpy.smart:print-hook :float (lambda (x) (princ-to-string x)))
;; (clpy.smart:print-hook :complex
;;                        (lambda (x)
;;                          (py:let ((r (real x))
;;                                   (i (imag x)))
;;                            (format nil "#(~A ~A)" r i))))

;; conversion

(defun int (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-long o)
    (error 'py.exc:generic-error)))

(defun float (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-float o)
    (error 'py.exc:generic-error)))

(defun index (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-index o)
    (clpy.exception:raise-generic-or-python-error)))

(defun to-base (o base)
  "Return the integer ``o`` converted to base base as a string."
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-to-base o base)
    (clpy.exception:raise-generic-or-python-error)))

;; operations

(defun + (o1 &optional (o2 nil))
  (clpy.util:ensure-null-as-nil
      (if o2
	  (clpy.ffi.fns:py-number-add o1 o2)
	  (clpy.ffi.fns:py-number-positive o1))
    (error 'py.exc:generic-error)))

(defun - (o1 &optional (o2 nil))
  (clpy.util:ensure-null-as-nil
      (if o2
	  (clpy.ffi.fns:py-number-subtract o1 o2)
	  (clpy.ffi.fns:py-number-negative o1))
    (error 'py.exc:generic-error)))

(defun * (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-multiply o1 o2)
    (error 'py.exc:generic-error)))

(defun @ (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-matrix-multiply o1 o2)
    (error 'py.exc:generic-error)))

(defun // (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-floor-divide o1 o2)
    (error 'py.exc:generic-error)))

(defun / (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-true-divide o1 o2)
    (error 'py.exc:generic-error)))

(defun % (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-remainder o1 o2)
    (error 'py.exc:generic-error)))

(defun divmod (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-divmod o1 o2)
    (error 'py.exc:generic-error)))

(defun ** (o1 o2)
  (clpy.util:ensure-null-as-nil
      (let* ((none (clpy.none:new))
	     (res (clpy.ffi.fns:py-number-power o1 o2 none)))
	(clpy.object:dec-ref none)
	res))
  (error 'py.exc:generic-error))

(defun **% (o1 o2 o3)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-power o1 o2 o3)
    (clpy.exception:raise-generic-or-python-error)))

(defun ~ (o1)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-invert o1)
    (error 'py.exc:generic-error)))

(defun << (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-lshift o1 o2)
    (error 'py.exc:generic-error)))

(defun >> (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-rshift o1 o2)
    (error 'py.exc:generic-error)))

(defun and (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-and o1 o2)
    (error 'py.exc:generic-error)))

(defun xor (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-xor o1 o2)
    (error 'py.exc:generic-error)))

(defun or (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-or o1 o2)
    (error 'py.exc:generic-error)))

;; in-place operation

(defun += (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-add o1 o2)
    (error 'py.exc:generic-error)))

(defun -= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-subtract o1 o2)
    (error 'py.exc:generic-error)))

(defun *= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-multiply o1 o2)
    (error 'py.exc:generic-error)))

(defun @= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-matrix-multiply o1 o2)
    (error 'py.exc:generic-error)))

(defun //= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-floor-divide o1 o2)
    (error 'py.exc:generic-error)))

(defun /= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-true-divide o1 o2)
    (error 'py.exc:generic-error)))

(defun %= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-remainder o1 o2)
    (error 'py.exc:generic-error)))

(defun **= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (let* ((none (clpy.none:new))
	     (res (clpy.ffi.fns:py-number-in-place-power o1 o2 none)))
	(clpy.object:dec-ref none)
	res)
    (error 'py.exc:generic-error)))

(defun **%= (o1 o2 o3)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-power o1 o2 o3)
    (clpy.exception:raise-generic-or-python-error)))

(defun <<= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-lshift o1 o2)
    (error 'py.exc:generic-error)))

(defun >>= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-rshift o1 o2)
    (error 'py.exc:generic-error)))

(defun and= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-and o1 o2)
    (error 'py.exc:generic-error)))

(defun xor= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-xor o1 o2)
    (error 'py.exc:generic-error)))

(defun or= (o1 o2)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-in-place-or o1 o2)
    (error 'py.exc:generic-error)))

;; util functions
(defun abs (o)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-number-absolute o)
    (error 'py.exc:generic-error)))

