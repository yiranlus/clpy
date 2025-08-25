(in-package :clpy.object)

(defun callable-p (o)
  (plusp (clpy.ffi.fns:py-callable-check o)))

(defun call (callable &key args kwargs)
  (clpy.util:ensure-null-as-nil
      (cond
        ((and (null args)
              (null kwargs))
         (clpy.ffi.fns:py-object-call-no-args callable))
        ((null kwargs)
         (clpy.util:let* ((-args (apply #'clpy.list:new args))
                          (-args-tuple (clpy.list:as-tuple -args)))
           (clpy.ffi.fns:py-object-call-object callable -args-tuple)))
        (t (clpy.util:let* ((-args (apply #'clpy.list args))
                            (-args-tuple (py.list:as-tuple -args))
                            (-kwargs (if kwargs (apply #'clpy.dict:new kwargs))))
             (clpy.ffi.fns:py-object-call -args-tuple -kwargs))))))
