(defpackage :clpy.doc-exporter
  (:use :cl :asdf)
  (:export :generate-doc-op))

(in-package :clpy.doc-exporter)

(defun export-package-doc (package-name output-dir)
  (let* ((filename (merge-pathnames
                    (format nil "~A.rst" (string-downcase package-name))
                    output-dir))
         (package (find-package package-name)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (format out "~%~A~%" package-name)
      (format out "===============================~%~%")
      (format out ".. cl:package:: ~A~%~%" package-name)
      (do-external-symbols (sym (find-package package))
        (cond
          ((fboundp sym)
           (format out ".. cl:function:: ~A~%~%" sym)
           ;;(format out "   ~A~%~%" (or (documentation sym 'function) "No docstring."))
           )
          ((macro-function sym)
           (format out ".. cl:macro:: ~A~%~%" sym)
           ;;(format out "   ~A~%~%" (or (documentation sym 'function) "No docstring."))
           ))))))

(defun string-prefix-p (prefix string)
  "Returns T if STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defclass generate-doc-op (asdf:non-propagating-operation) ()
  (:documentation "Operation to generate Sphinx-compatible documentation."))

(defmethod asdf:perform ((op generate-doc-op) (system asdf:system))
  (dolist (pkg (list-all-packages))
    (let ((output-dir (cond
                        ((and (string-prefix-p "CLPY." (package-name pkg))
                              (not (string= "CLPY.DOC-EXPORTER" (package-name pkg)))
                              (not (string-prefix-p "CLPY.FFI" (package-name pkg))))
                         #p"docs/references/clpy/")
                        ((string-prefix-p "CLTHON" (package-name pkg))
                         #p"docs/references/clthon/")
                        (t nil))))
      (when output-dir
        (ensure-directories-exist output-dir)
        (export-package-doc (package-name pkg) output-dir)))))
