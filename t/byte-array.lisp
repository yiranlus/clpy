(in-package :clpy.test)

(def-suite byte-array-test
  :in clpy
  :description "Test CLPY system")

(in-suite byte-array-test)

(test new-from-string
  (py:let ((x (py.byte-array:new "abcdefg")))
    (is (= 7 (py.byte-array:size x)))))

(test new-from-object
  (py:let ((x (py.bytes:new "abcdefg")))
    (py:let ((x (py.byte-array:new x)))
      (is (= 7 (py.byte-array:size x))))))

(test concate-byte-array
  (py:let ((x (py.byte-array:new "abcdefg"))
           (y (py.byte-array:new "hijklmn")))
    (py:let ((z (py.byte-array:concat x y)))
      (is (= 14 (py.byte-array:size z)))
      (is (string-equal "abcdefghijklmn"
                        (py.byte-array:as-string z))))))

(test resize-byte-array
  (py:let ((x (py.byte-array:new "abcdefg")))
    (py.byte-array:resize x 14)
    (is (= 14 (py.byte-array:size x)))))
