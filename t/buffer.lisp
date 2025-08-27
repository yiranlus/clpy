(in-package :clpy.test)

(def-suite buffer-test
  :in clpy
  :description "Test CLPY system")

(in-suite buffer-test)

(test make-flags-from-integer-1
  (let ((flags (py.buf:make-flags #x1)))
    (is (= 1 flags))))

(test make-flags-from-integer-2
  (let ((flags (py.buf:make-flags #x1 #x2)))
    (is (= 3 flags))))

(test make-flags-from-keywords-1
  (let ((flags (py.buf:make-flags :nd)))
    (is (= #x0008 flags))))

(test make-flags-from-keywords-2
  (let ((flags (py.buf:make-flags :format :nd)))
    (is (= (logior #x0004 #x0008) flags))))

(test make-flags-from-keywords-3
  (let ((flags (py.buf:make-flags :strides :contig)))
    (is (= (logior #x0008 #x0010 #x0001) flags))))
