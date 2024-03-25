(in-package :clpy)

(defun -import (name &key (name-decoder :fs-default))
  (log:debug "Enter PY:-IMPORT function.")
  (match name
    ((type clpy.ffi:py-object)
     (return-from -import
       (py:ensure-null-as-nil (clpy.ffi.fns:py-import-import name)
         (error 'py.exc:import-error))))
    ((type string)
     (log:debug "String NAME" name "detected.")
     (let ((name-v
             (match name-decoder
               (:fs-default (py.unicode:decode-fs-default name))
               (:string (py.unicode:from-string name))
               (otherwise (error 'simple-error
                                 (format nil "Method ~A unsupported." name-decoder))))))
       (when name-v
         (log:debug "Created module name" name-v)
         (let ((res (clpy.ffi.fns:py-import-import name-v)))
           (clpy.ffi.fns:py-dec-ref name-v)
           (when (cffi:null-pointer-p (autowrap:ptr res))
             (error 'py.exc:import-error))
           res))))))

(defun -import-ex (name &key globals locals from-list (level 0))
  (let* ((level (or level 0))
         (name-v name)
         (func (if (stringp name)
                   #'clpy.ffi.fns:py-import-import-module-level
                   (progn
                     (setf name-v (obj-ref name))
                     #'clpy.ffi.fns:py-import-import-module-level-object)))
         (res (apply func name-v globals locals from-list level)))
    (when (cffi:null-pointer-p (autowrap:ptr res))
      (error 'py.exc:import-error))
    res))

(defun import-module (name &key
                             (name-decoder :fs-default)
                             (globals nil globals-p)
                             (locals nil locals-p)
                             (from-list nil from-list-p)
                             (level nil level-p))
  (if (or globals-p locals-p from-list-p level-p)
      (-import-ex name globals locals form-list level)
      (-import name :name-decoder name-decoder)))
