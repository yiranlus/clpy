(defpackage :clpy.descriptor
  (:nicknames :py.descr)
  (:use :cl)
  (:export #:new))

(in-package :clpy.descriptor)

(clpy.type:define-type "PyProperty_Type" property)

(clpy.type:define-type "PyGetSetDescr_Type" get-set-descr)
(clpy.type:define-type "PyMemberDescr_Type" member-descr)
(clpy.type:define-type "PyMethodDescr_Type" method-descr)
(clpy.type:define-type "PyWrapperDescr_Type" wrapper-descr)
(clpy.type:define-type "PyClassMethodDescr_Type" class-method-descr)

(defun new (type &key
		   get-set
		   member
		   method
		   wrapper wrapped
		   class-method)
  (declare (ignore wrapped))
  (when (not (= 1 (count-if-not #'null (list get-set member method wrapper class-method))))
    (error "Only one type of descriptor can be created at one time."))
  (clpy.util:ensure-null-as-nil
      (cond
	(get-set (clpy.ffi.fns:py-descr-new-get-set get-set))
	(member (clpy.ffi.fns:py-descr-new-member member))
	(method (clpy.ffi.fns:py-descr-new-method method))
	(wrapper (error "Not supported in limited API."))
	(class-method (clpy.ffi.fns:py-descr-new-class-method class-method)))
    (clpy.exception:raise-generic-or-python-error
     :message "Unable to create the descriptor.")))
