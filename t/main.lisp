(require :clpy/test)
(require :fiveam)

;; (py:initialize)

;; (unless (py:is-initialized)
;;   (format *error-output* "Unable to intialize Python.")
;;   (exit 1))

(in-package :clpy.test)
(fiveam:run! 'clpy)

;; (py:finalize)
