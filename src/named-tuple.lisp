(defpackage :clpy.named-tuple
  (:nicknames :py.nt)
  (:use :cl :plus-c)
  (:export #:+UNNAMED-FIELD+
	   #:new-type
	   #:new
	   #:get-item
	   #:set-item
	   #:free-registry))

(in-package :clpy.named-tuple)

(cffi:defcvar ("PyStructSequence_UnnamedField" +UNNAMED-FIELD+ :read-only t) :string)

(defvar *nt-type-registry* '())

(defun new-type (name docstring fields &optional n-in-sequence)
  "Utility function to create a new PyStructSequence_Desc.

Must use ``autowrap:free`` before exiting the program."
  (c-let ((nt-type clpy.ffi:py-struct-sequence-desc))
    (setf (nt-type :name) name
	  (nt-type :doc) (or docstring nil))
    (c-let ((nt-fields clpy.ffi:py-struct-sequence-field
		       :count (1+ (length fields))))
      (loop for (name doc) in fields
	    for i from 0 below (length fields)
	    do (if (eq name :unamed)
		   (setf (nt-fields i :name &)
			 +UNNAMED-FIELD+)
		   (setf (nt-fields i :name) name))
	       (setf (nt-fields i :doc) (or doc nil)))
      (let ((n-fields (1+ (length fields))))
	(setf (nt-fields n-fields :name) nil))
      (setf (nt-type :fields &) (autowrap:ptr nt-fields))
      (setf (nt-type :n-in-sequence) (or n-in-sequence (length fields)))
      (let ((res (clpy.util:ensure-null-as-nil
		     (clpy.ffi.fns:py-struct-sequence-new-type nt-type)
		   (clpy.exception:raise-generic-or-python-error
		    :message "Unable to create new named-tuple type."))))
	(push nt-type *nt-type-registry*)
        (clpy.type:define-type-from res name)
	res))))

(defun free-registry ()
  (dolist (nt-type *nt-type-registry*)
    (autowrap:free (nt-type :fields))
    (autowrap:free nt-type))
  (setf *nt-type-registry* '()))

(defun new (type &rest fields)
  (let ((res (clpy.util:ensure-null-as-nil
		 (clpy.ffi.fns:py-struct-sequence-new type)
	       (clpy.exception:raise-generic-or-python-error
		:message "Unable to create a new named tuple."))))
    (loop for field in fields
	  for i from 0 below (length fields)
	  do (if (and (listp field) (not (null field)))
		 (clpy.util:let ((-name (clpy.str:new (car field)))
				 (-value (clpy.smart:new (cdr field))))
		   (clpy.object:set-item -name -value))
		 (clpy.util:let ((-value (clpy.smart:new field)))
		   (set-item i -value))))))

(defun get-item (nt pos)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-struct-sequence-get-item nt pos)
    (clpy.exception:raise-generic-or-python-error)))

(defun set-item (nt pos value)
  (clpy.util:let ((-value (clpy.smart:new value)))
    (clpy.ffi.fns:py-struct-sequence-set-item nt pos -value)))
		    
