(defpackage :clpy.number
  (:nicknames :py.num)
  (:use :cl)
  (:shadow #:abs #:real #:+ #:- #:* #:/ #:// #:** #:/=
           #:int #:float #:and #:or #:xor)
  (:export #:new
           #:p
           #:index-p
           #:int-p
           #:int-exact-p
           #:float-p
           #:float-exact-p
           #:complex-p
           #:complex-exact-p
           #:as-double
           #:as-integer
           #:int
           #:float
           #:index
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
  (cl:or (clpy.type:of o :long)
         (clpy.type:subtype-p (clpy.object:ob-type o)
                              (clpy.type:get :long))))

(defun int-exact-p (o)
  (clpy.type:of o :long))

(defun float-p (o)
  (cl:or (clpy.type:of o :float)
         (clpy.type:subtype-p (clpy.object:ob-type o)
                              (clpy.type:get :float))))

(defun float-exact-p (o)
  (clpy.type:of o :float))

(defun complex-p (o)
  (cl:or (clpy.type:of o :complex)
         (clpy.type:subtype-p (clpy.object:ob-type o)
                              (clpy.type:get :complex))))

(defun complex-exact-p (o)
  (clpy.type:of o :complex))


(defun p (o)
  (plusp (clpy.ffi.fns:py-number-check o)))

(defun index-p (o)
  (plusp (clpy.ffi.fns:py-index-check o)))

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
   (clpy.exception:raise-generic-or-python-error :message (format nil "Unable to create PyObject for ~A." n))))

(defun as-double (number)
  (case (py.obj:ob-type number)
    (:float (clpy.ffi.fns:py-float-as-double number))
    (:long (clpy.ffi.fns:py-long-as-double number))
    (otherwise (error "Unsupported type to convert to double."))))

(defun real (number)
  (clpy.ffi.fns:py-complex-real-as-double number))

(defun imag (number)
  (clpy.ffi.fns:py-complex-imag-as-double number))

(defun as-complex (number)
  (clpy.util:let ((r (real number))
                  (i (imag number)))
    (complex r i)))

(defun as-integer (number &optional (type :long) (mask nil))
  (unless (clpy.type:of number :long)
    (error "NUMBER is not a PyLong."))
  (case type
    ;;(:int (clpy.ffi.fns:py-long-as-int 0))
    (:long (clpy.ffi.fns:py-long-as-long number))
    (:long-long (clpy.ffi.fns:py-long-as-long-long number))
    (:ssize-t (clpy.ffi.fns:py-long-as-ssize-t number))
    (:usigned-long
     (if mask
         (clpy.ffi.fns:py-long-as-unsigned-long-mask number)
         (clpy.ffi.fns:py-long-as-unsigned-long number)))
    (:unsigned-long-long
     (if mask
         (clpy.ffi.fns:py-long-as-unsigned-long-long-mask number)
         (clpy.ffi.fns:py-long-as-unsigned-long-long number)))
    (:size-t (clpy.ffi.fns:py-long-as-size-t number))))

;; smart

(clpy.smart:new-hook #'numberp #'new)

(clpy.smart:print-hook #'int-p #'as-integer)
(clpy.smart:print-hook #'float-p #'as-double)
(clpy.smart:print-hook #'complex-p #'as-complex)

;; conversion

(defun int (number)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-long number)
   (clpy.exception:raise-generic-or-python-error)))

(defun float (number)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-float number)
   (clpy.exception:raise-generic-or-python-error)))

(defun index (number)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-index number)
   (clpy.exception:raise-generic-or-python-error)))

(defun to-base (number base)
  "Return the integer ``number`` converted to base base as a string."
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-to-base number base)
   (clpy.exception:raise-generic-or-python-error)))

;; operations

(defun + (o1 &optional (o2 nil))
  (clpy.util:ensure-null-as-nil
   (if o2
       (clpy.ffi.fns:py-number-add o1 o2)
       (clpy.ffi.fns:py-number-positive o1))
   (clpy.exception:raise-generic-or-python-error)))

(defun - (o1 &optional (o2 nil))
  (clpy.util:ensure-null-as-nil
   (if o2
       (clpy.ffi.fns:py-number-subtract o1 o2)
       (clpy.ffi.fns:py-number-negative o1))
   (clpy.exception:raise-generic-or-python-error)))

(defun * (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-multiply o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun @ (o1 o2)
    (clpy.util:ensure-null-as-nil
     (clpy.ffi.fns:py-number-matrix-multiply o1 o2)
     (clpy.exception:raise-generic-or-python-error)))

(defun // (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-floor-divide o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun / (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-true-divide o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun % (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-remainder o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun divmod (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-divmod o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun ** (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.util:let ((none (clpy.object:none)))
     (clpy.ffi.fns:py-number-power o1 o2 none))
   (clpy.exception:raise-generic-or-python-error)))

(defun **% (o1 o2 o3)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-power o1 o2 o3)
   (clpy.exception:raise-generic-or-python-error)))

(defun ~ (o1)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-invert o1)
   (clpy.exception:raise-generic-or-python-error)))

(defun << (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-lshift o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun >> (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-rshift o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun and (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-and o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun xor (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-xor o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun or (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-or o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

;; in-place operation

(defun += (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-add o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun -= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-subtract o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun *= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-multiply o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun @= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-matrix-multiply o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun //= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-floor-divide o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun /= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-true-divide o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun %= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-remainder o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun **= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.util:let ((none (clpy.object:none)))
     (clpy.ffi.fns:py-number-in-place-power o1 o2 none))
   (clpy.exception:raise-generic-or-python-error)))

(defun **%= (o1 o2 o3)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-power o1 o2 o3)
   (clpy.exception:raise-generic-or-python-error)))

(defun <<= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-lshift o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun >>= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-rshift o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun and= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-and o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun xor= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-xor o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

(defun or= (o1 o2)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-in-place-or o1 o2)
   (clpy.exception:raise-generic-or-python-error)))

;; util functions
(defun abs (number)
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-number-absolute number)
   (clpy.exception:raise-generic-or-python-error)))
