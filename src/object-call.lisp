(in-package :clpy.object)

(defun callable-p (o)
  (plusp (clpy.ffi.fns:py-callable-check o)))

(defun call (callable &key args kwargs)
  (py:ensure-null-as-nil
      (cond
        ((and (null args)
              (null kwargs))
         (clpy.ffi.fns:py-object-call-no-args callable))
        ((null kwargs)
         (clpy.pylet:let* ((-args (py:new `(:l ,@args)))
                           (-args-tuple (py.list:as-tuple -args)))
           (clpy.ffi.fns:py-object-call-object callable -args-tuple)))
        (t (clpy.pylet:let* ((-args (apply #'py.list args))
                             (-args-tuple (py.list:as-tuple -args))
                             (-kwargs (if kwargs (py:new `(:d ,@kwargs)))))
             (clpy.ffi.fns:py-object-call -args-tuple -kwargs))))))
                            
