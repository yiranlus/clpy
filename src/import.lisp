(defpackage :clpy.import
  (:nicknames :py.import)
  (:use :cl)
  (:shadow :import)
  (:export #:import))
(in-package :clpy.import)

(defun import (name &key
                      globals locals from-list (level 0) ;; PyImport_ImportModule
                      pycode pathname cpathname)         ;; PyImport_ExecCodeModule
  (declare (ignore locals))
  (clpy.util:ensure-null-as-nil
      (if pycode
          (clpy.pylet:let ((-name (clpy.smart:new name))
                           (-pathname (clpy.smart:new pathname))
                           (-cpathname (clpy.smart:new cpathname)))
            (clpy.ffi.fns:py-import-exec-code-module-object -name pycode -pathname -cpathname))
          (clpy.pylet:let ((-name (clpy.smart:new name))
                           (-globals (if globals (clpy.smart:new `(:d ,globals))))
                           (-locals nil) ;; not used
                           (-from-list (if from-list (clpy.smart:new `(:l from-list)))))
            (clpy.ffi.fns:py-import-import-module-level-object -name -globals -locals -from-list level)))
    (error 'py.exc:generic-error)))

(defun reload (o &key (new-ref nil))
  (let ((res (clpy.util:ensure-null-as-nil
                 (clpy.ffi.fns:py-import-reload-module o)
               (error 'py.exc:generic-error))))
    (unless new-ref
      (clpy.object:dec-xref res))
    res))
