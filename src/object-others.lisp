(cl:in-package :clpy.object)

;;(defun rich-compare (o1 o2 op &optional (as :bool))
;;  "Compare the values of O1 and O2 using the operation specified by OPID, which must be of of :LT, :EQ, :NE, :GT, or :GE."
;;  (let ((py-op (match op
;;                (:lt clpy.ffi:+py-lt+)
;;                (:eq clpy.ffi:+py-eq+)
;;                (:ne clpy.ffi:+py-ne+)
;;                (:gt clpy.ffi:+py-gt+)
;;                (:ge clpy.ffi:+py-ge+))))
;;    (match as
;;      (:object
;;       (py:ensure-null-as-nil
;;           (clpy.ffi.fns:py-object-rich-compare o1 o2 op)
;;         (error 'py.exc:python-error)))
;;      (:bool (match
;;                 (clpy.ffi.fns:py-object-rich-compare-bool o1 o2 op)
;;               (1 t)
;;               (0 nil)
;;               (otherwise (error 'py.exc:python-error)))))))
;;
;;;; Representation
;;
;;(defun format (obj &optional format-spec)
;;  "Format OBJ using FORMAT-SPEC. This is equivalent to the Python expression ``format(object, format_spec)''."
;;  (py:ensure-null-as-nil
;;      (clpy.ffi.fns:py-object-format obj format-spec)
;;    (error 'py.exc:python-error)))

;;;; Class relations
;;
;;(defun is-subclass (o cls)
;;  "Return T if the class derived is identical to or derived from the class CLS, otherwise return 0."
;;  (match (clpy.ffi.fns:py-object-is-subclass o cls)
;;    (1 t)
;;    (0 nil)
;;    (-1 (error 'py.exc:python-error))))

(defun is-instance (o cls)
  "Return T if the class derived is identical to or derived from the class CLS, otherwise return 0. CLS can be keyword registered in PY.TYPE:*ASSOC-TYPES*."
  (let ((-cls (if (keywordp cls) (py.type:get cls) cls)))
    (case (clpy.ffi.fns:py-object-is-instance o -cls)
      (1 t)
      (0 nil)
      (-1 (error 'py.exc:python-error)))))

;;(defun hash (o)
;;  "Compute and return the hash value of an object O. On failure,return NIL"
;;  (let ((res (clpy.ffi.fns:py-object-hash o)))
;;    (when (= res -1)
;;      (error 'py.exc:python-error))
;;    res))
;;
;;(defun hash-not-implemented (o)
;;  "Set a TypeError indicating that ``type(o)'' is not hashable."
;;  (clpy.ffi.fns:py-object-hash-not-implemented o)
;;  (error 'py.exc:type-error))

;; Evaluation

(defun is-true (o)
  "Return T if the object o is considered to be true, otherwise NIL"
  (not (zerop
        (clpy.util:ensure-non-negative
            (clpy.ffi.fns:py-object-is-true o)
          (error 'py.exc:python-error)))))

(defun is-false (o)
  "Return NIL if the object o is considered to be true, otherwise t"
  (not (zerop
        (clpy.util:ensure-non-negative
            (clpy.ffi.fns:py-object-not o)
          (error 'py.exc:python-error)))))

;;(defun type (o)
;;  "When O is no-NULL, returns a type object corresponding to the object type of object O"
;;  (py:ensure-null-as-nil
;;      (clpy.ffi.fns:py-object-type o)
;;    (error 'py.exc:system-error)))

;;
;;(defun dir (o)
;;  "Return the length of O."
;;  (py:ensure-null-as-nil
;;      (clpy.ffi.fns:py-object-dir o)
;;    (error 'py.exc:python-error)))
;;
;;(defun get-iter (o)
;;  "Return a new iterator for object argument, or the object itself if the object is already an iterator. This is equivalent to the Python expression ``iter(o)''"
;;  (py:ensure-null-as-nil
;;      (clpy.ffi.fns:py-object-get-iter o key)
;;    (error 'py.exc:type-error)))
;;
;;(defun get-a-iter (o)
;;  "Return an AsyncIterator for the given AsyncIterable object O. This is the equivqlent to the Python expression ``aiter(o)''"
;;  (py:ensure-null-as-nil
;;      (clpy.ffi.fns:py-object-get-a-iter o key)
;;    (error 'py.exc:type-error)))

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
