(in-package :clpy.test)

(def-suite bytes-test
  :in clpy
  :description "Test CLPY system")

(in-suite bytes-test)

(test size
  (let ((a-str "abcdefg"))
    (py:let ((a-str-py (py.bytes:new a-str)))
      (is (= (length a-str) (py.bytes:size a-str-py))))))

(test get-and-retrive
  (let ((a-str "abcdefg"))
    (py:let ((a-str-py (py.bytes:new a-str)))
      (let ((the-str (py.bytes:as-string a-str-py)))
	(is (string-equal a-str the-str))))))
  
