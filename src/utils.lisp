(in-package :clpy)

(defmacro auto-dec-ref (varlist &body body)
  `(let (,@varlist)
     ,@body
     ,@(loop for i in `(,@varlist)
            if (listp i)
              collect `(dec-ref ,(first i))
            else
              collect `(dec-ref ,i))))
