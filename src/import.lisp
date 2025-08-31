(defpackage :clpy.import
  (:nicknames :py.import)
  (:use :cl)
  (:shadow #:import #:get)
  (:export #:import
           #:--import--
           #:add
           #:reload
           #:import-as ;; utility function

           #:get-magic-number
           #:get-magic-tag
           #:get-module-dict
           #:get-importer
           #:append-inittab))

(in-package :clpy.import)

;; PyImport_ImportModule
;; PyImport_ExecCodeModule
;; PyImport_ImportFrozenModule
(defun import (name                     
               &optional type ;; either NIL, :exec-code, or :frozen
               &rest rest
               &key co pathname cpathname)
  "Import a module.

The module can be a normal module, a code object (specified by PYCODE),
or a frozen module (use FROZEN)."
  (declare (ignore locals rest))
  (clpy.util:ensure-null-as-nil
   (case type
     (:exec-code (clpy.util:let ((-name (clpy.str:new name))
                                 (-pathname (clpy.str:new pathname))
                                 (-cpathname (clpy.str:new cpathname)))
                   (clpy.ffi.fns:py-import-exec-code-module-object -name co -pathname -cpathname)))
     (:frozen (clpy.util:let ((-name (clpy.str:new name)))
                (clpy.ffi.fns:py-import-import-frozen-module-object -name)))
     (otherwise (if (stringp name)
                    (clpy.ffi.fns:py-import-import-module name)
                    (clpy.ffi.fns:py-import-import-module name))))
   (clpy.exception:raise-generic-or-python-error
    :message "Unable to import the module")))

;; PyImport_ImportModuleLevelObject, or
;; PyImport_ImportModuleEx
(defun --import-- (name &key globals locals from-list (level 0))
  "Import the module, but the return function is the top-level module
if from-list is NIL."
  (clpy.util:ensure-null-as-nil
   (clpy.util:let ((-name (clpy.str:new name))
                   (-globals (if (clpy.object:p globals)
                                 (clpy.object:new-ref globals)
                                 (apply #'clpy.dict:new globals)))
                   (-locals nil) ;; not used
                   (-from-list (if (clpy.object:p from-list)
                                   (clpy.object:new-ref from-list)
                                   (apply #'clpy.list:new from-list))))
     (clpy.ffi.fns:py-import-import-module-level-object -name -globals -locals -from-list level))
   (clpy.exception:raise-generic-or-python-error
    :message "Unable to import the module.")))


(defun add (name)
  "Return the module object corresponding to a module name.

The module name can be of the form ``package.module``. This function
won't load the module if it is not loaded. Use :cl:function:`import`
to load the module."
  (clpy.util:ensure-null-as-nil
   (clpy.util:let ((-name (clpy.str:new name)))
     (clpy.ffi.fns:py-import-add-module-object -name))
   (clpy.exception:raise-generic-or-python-error
    :message "Unable to add the module.")))

(defun reload (o &key (new-ref nil))
  (let ((res (clpy.util:ensure-null-as-nil
              (clpy.ffi.fns:py-import-reload-module o)
              (clpy.exception:raise-generic-or-python-error
               :message "Unable to reload the module"))))
    (unless new-ref
      (clpy.object:dec-ref res))
    res))

(defun get-magic-number ()
  (clpy.util:ensure-non-negative
   (clpy.ffi.fns:py-import-get-magic-number)
   (clpy.exception:raise-generic-or-python-error
    :message "Failed to get magic number.")))

(defun get-magic-tag ()
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-import-get-magic-tag)
   (clpy.exception:raise-generic-or-python-error
    :message "Failed to get magic tag.")))

(defun get-module-dict ()
  (clpy.util:ensure-null-as-nil
   (clpy.ffi.fns:py-import-get-module-dict)
   (clpy.exception:raise-generic-or-python-error
    :message "Failed to get module dict.")))

(defun get-module (name)
  (clpy.util:ensure-null-as-nil
   (clpy.util:let ((-name (clpy.str:new name)))
     (clpy.ffi.fns:py-import-get-module -name))
   (clpy.exception:return-or-raise-python-error nil)))

(defun get-importer (path)
  (clpy.util:ensure-null-as-nil
   (clpy.util:let ((-path (clpy.str:new path)))
     (clpy.ffi.fns:py-import-get-importer -path))
   (clpy.exception:raise-generic-or-python-error)))

;; utility macro/function to import function

(defun get (module attrs)
  (clpy.util:let ((-module (import module)))
    (loop for attr in attrs
          collect (clpy.object:get-attr -module attr))))

;; (import-as (((numpy . "numpy") (array . "array")
;;                                (sum   . "sum"))
;;             ("matplotlib" (plt . "pyplot")))
;;   ;; run some code
;; )


(defmacro import-as (import-list &body body)
  (format t "Hello Kitty!~%"))

(defmacro import-as (import-list &body body)
  (if import-list
      (let* ((module-attrs (car import-list))
             (module (car module-attrs))
             (module-sym (if (listp module)
                             (first module)
                             (gensym)))
             (module-name (if (listp module)
                              (second module)
                              module)))
        `(clpy.util:let ((,module-sym (import ,module-name)))
           ,(when (second module-attrs)
              `(clpy.util:let
                   ,(loop for (attr-sym attr-name) in (second module-attrs)
                          collect `(,attr-sym
                                    (clpy.object:get-attr ,module-sym ,attr-name)))
                 (import-as ,(cdr import-list) ,@body)))))
      `(progn ,@body)))
