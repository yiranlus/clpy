(defpackage :clpy.object
  (:nicknames :py.object)
  (:use :cl :trivia)
  (:shadow #:format #:type)
  (:export #:ob-refcnt
           #:ob-type
           #:has-attr
           #:get-attr
           #:set-attr
           #:generic-get-attr
           #:generic-set-attr
           #:generic-get-dict
           #:generic-set-dict
           #:rich-compare
           #:format
           #:repr
           #:ascii
           #:str
           #:bytes
           #:is-subclass
           #:is-instance
           #:hash
           #:hash-not-implemented
           #:is-true
           #:is-false
           #:type
           #:size
           #:get-item
           #:set-item
           #:del-item
           #:dir
           #:get-iter
           #:get-a-iter
           #:call ;; call.lisp
           ))

(in-package :clpy.object)

;; PyObject

(defun ob-refcnt (obj)
  "Return ob_refcnt of the PyObject."
  (c-ref obj clpy.ffi:py-object :ob-refcnt))

(defun ob-type (obj)
  "Return ob_type of the PyObject."
  (c-ref obj clpy.ffi:py-object :ob-type))

;; TODO changed to python style
;;(defmethod print-object ((obj py-object) stream)
;;  ;;(write-string (as-string (ascii obj)) stream)
;;  (print-object obj stream))

;; Python API

;; Attributes

(defun has-attr (o attr-name)
  "Retrieve an attribute named ATTR-NAME. ATTR-NAME can be a py-object or a string."
  (plusp
   (if (stringp attr-name)
       (clpy.ffi.fns:py-object-has-attr-string o attr-name)
       (clpy.ffi.fns:py-object-has-attr o attr-name))))

(defun get-attr (o attr-name)
  "Retrieve an attribute named ATTR-NAME. ATTR-NAME can be a py-object or a string"
  (py:ensure-null-as-nil
      (if (stringp attr-name)
          (clpy.ffi.fns:py-object-get-attr-string o attr-name)
          (clpy.ffi.fns:py-object-get-attr o attr-name))
    (error 'py.exc:attribute-error)))

(defun set-attr (o attr-name v)
  "Set the value of the attribute named ATTR-NAME, for object o,to the value v."
  (py:ensure-zero
      (if (stringp attr-name)
          (clpy.ffi.fns:py-object-set-attr-string o attr-name v)
          (clpy.ffi.fns:py-object-set-attr o attr-name v))
    (error 'py.exc:attribute-error)))

(defun generic-get-attr (o name)
  "Generic attribute getter function that is meant to be put into a type object's ``tp_getattro'' slot"
  (py:ensure-null-as-nil
    (clpy.ffi.fns:py-object-generic-get-attr o name)))

(defun generic-set-attr (o name v)
  "Generic attribute setter function that is meant to be put into a type object's ``tp_setattro'' slot"
  (py:ensure-zero
      (clpy.ffi.fns:py-object-generic-set-attr o name v)
    error 'py.exc:attribute-error))

(defun generic-get-dict (o &optional context)
  "A generic implementation for the getter of a ``__dict''descriptor. It creates the dictionary if necessary."
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-generic-get-dict o context)
    (error 'py.exc:attribute-error)))

(defun generic-set-dict (o value &optional context)
  "A generic implementation for the setter of a ``__dict''descriptor. This implementation does not allow the dictionary to be deletec"
  (py:ensure-zero
      (clpy.ffi.fns:py-object-generic-set-dict o value context)
    (error 'py.exc:attribute-error)))

(defun rich-compare (o1 o2 op &optional (as :bool))
  "Compare the values of O1 and O2 using the operation specified by OPID, which must be of of :LT, :EQ, :NE, :GT, or :GE."
  (let ((py-op (match op
                (:lt clpy.ffi:+py-lt+)
                (:eq clpy.ffi:+py-eq+)
                (:ne clpy.ffi:+py-ne+)
                (:gt clpy.ffi:+py-gt+)
                (:ge clpy.ffi:+py-ge+))))
    (match as
      (:object
       (py:ensure-null-as-nil
           (clpy.ffi.fns:py-object-rich-compare o1 o2 op)
         (error 'py.exc:python-error)))
      (:bool (match
                 (clpy.ffi.fns:py-object-rich-compare-bool o1 o2 op)
               (1 t)
               (0 nil)
               (otherwise (error 'py.exc:python-error)))))))

;; Representation

(defun format (obj &optional format-spec)
  "Format OBJ using FORMAT-SPEC. This is equivalent to the Python expression ``format(object, format_spec)''."
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-format obj format-spec)
    (error 'py.exc:python-error)))

(defun repr (o)
  "Compute a string representation of the object. This generates a string similar to that returned by ``repr()''"
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-repr o)
    (error 'py.exc:python-error)))

(defun ascii (o)
  "Compute a string representation of the object. This generates a string similar to that returned by ``ascii()''"
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-ascii o)
    (error 'py.exc:python-error)))

(defun str (o)
  "Compute a string representation of the object. This is the equivalent of the Python expresssion ``str(o)''. Return nil when fails."
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-str o)
    (error 'py.exc:python-error)))

(defun bytes (o)
  "Compute a bytesing representation of the object. This is the equivalent of the Python expresssion ``bytes(o)''. Return nil when fails."
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-bytes o)
    (error 'py.exc:python-error)))

;; Class relations

(defun is-subclass (o cls)
  "Return T if the class derived is identical to or derived from the class CLS, otherwise return 0."
  (match (clpy.ffi.fns:py-object-is-subclass o cls)
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:python-error))))

(defun is-instance (o cls)
  "Return T if the class derived is identical to or derived from the class CLS, otherwise return 0."
  (match (clpy.ffi.fns:py-object-is-instance o cls)
    (1 t)
    (0 nil)
    (-1 (error 'py.exc:python-error))))

(defun hash (o)
  "Compute and return the hash value of an object O. On failure,return NIL"
  (let ((res (clpy.ffi.fns:py-object-hash o)))
    (when (= res -1)
      (error 'py.exc:python-error))
    res))

(defun hash-not-implemented (o)
  "Set a TypeError indicating that ``type(o)'' is not hashable."
  (clpy.ffi.fns:py-object-hash-not-implemented o)
  (error 'py.exc:type-error))

;; Evaluation

(defun is-true (o)
  "Return T if the object o is considered to be true, otherwise NIL"
  (not (zerop
        (py:ensure-non-negative
            (clpy.ffi.fns:py-object-is-true o)
          (error 'py.exc:python-error)))))

(defun is-false (o)
  "Return NIL if the object o is considered to be true, otherwise t"
  (not (zerop
        (py:ensure-non-negative
            (clpy.ffi.fns:py-object-not o)
          (error 'py.exc:python-error)))))

(defun type (o)
  "When O is no-NULL, returns a type object corresponding to the object type of object O"
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-type o)
    (error 'py.exc:system-error)))

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

(defun dir (o)
  "Return the length of O."
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-dir o)
    (error 'py.exc:python-error)))

(defun get-iter (o)
  "Return a new iterator for object argument, or the object itself if the object is already an iterator. This is equivalent to the Python expression ``iter(o)''"
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-get-iter o key)
    (error 'py.exc:type-error)))

(defun get-a-iter (o)
  "Return an AsyncIterator for the given AsyncIterable object O. This is the equivqlent to the Python expression ``aiter(o)''"
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-object-get-a-iter o key)
    (error 'py.exc:type-error)))

;; Only available since Python 3.12
;;(defgeneric get-type-data (o cls)
;;  (:documentation "Get a pointer to subclass-specific data reserved for CLS"))
;;(defun get-type-data ((o py-object) (cls py-object))
;;  (let ((res (clpy.ffi.fns:py-object-get-type-data o cls)))
;;    (when (cffi:null-pointer-p (autowrap:ptr res))
;;      (error 'simple-error "Encounter error when evaluting o."))))

;;(defgeneric get-type-data-size (cls)
;;  (:documentation "Return the size of the instance memory space reserved for CLS,
;;i.e. the size of the memory GET-TYPE-DATA returns."))
;;(defun get-type-data-size ((cls py-object))
;;  (let ((res (clpy.ffi.fns:py-object-get-type-data-size cls)))
;;    (when (minusp res)
;;      (error 'simple-error "Encounter error when evaluting o."))))
