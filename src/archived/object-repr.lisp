(cl:in-package :clpy.object)

(defun repr (o &key only-ascii)
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
