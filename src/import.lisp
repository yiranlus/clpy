(defpackage :clpy.import
  (:nicknames :py.import)
  (:use :cl)
  (:shadow :import)
  (:export #:import
	   #:reload))

(in-package :clpy.import)

(defun import (name &key
                      globals locals from-list (level 0) ;; PyImport_ImportModule
                      pycode pathname cpathname)         ;; PyImport_ExecCodeModule
  "Import a module"
  (declare (ignore locals))
  (clpy.util:ensure-null-as-nil
      (if pycode
          (clpy.util:let ((-name (clpy.smart:new name))
                          (-pathname (clpy.smart:new pathname))
                          (-cpathname (clpy.smart:new cpathname)))
            (clpy.ffi.fns:py-import-exec-code-module-object -name pycode -pathname -cpathname))
          (clpy.util:let ((-name (clpy.smart:new name))
                          (-globals (if globals (apply #'clpy.dict:new globals)))
                          (-locals nil) ;; not used
                          (-from-list (if from-list (apply #'clpy.list:new from-list))))
            (clpy.ffi.fns:py-import-import-module-level-object -name -globals -locals -from-list level)))
    (clpy.exception:raise-generic-or-python-error)))

(defun reload (o &key (new-ref nil))
  (let ((res (clpy.util:ensure-null-as-nil
                 (clpy.ffi.fns:py-import-reload-module o)
               (clpy.exception:raise-generic-or-python-error))))
    (unless new-ref
      (clpy.object:dec-ref res))
    res))
