(in-package :clpy.test)

(def-suite byte-array-test
  :in clpy
  :description "Test CLPY system")

(in-suite byte-array-test)

(test new-from-string
  (py:let ((x (py.ba:new "abcdefg")))
    (is (= 7 (py.ba:size x)))))

(test new-from-object
  (py:let ((x (py.bytes:new "abcdefg")))
    (py:let ((x (py.ba:new x)))
      (is (= 7 (py.ba:size x))))))

(test concate-byte-array
  (py:let ((x (py.ba:new "abcdefg"))
           (y (py.ba:new "hijklmn")))
    (py:let ((z (py.ba:concat x y)))
      (is (= 14 (py.ba:size z)))
      (is (string-equal "abcdefghijklmn"
                        (py.ba:as-string z))))))

(test resize-byte-array
  (py:let ((x (py.ba:new "abcdefg")))
    (py.ba:resize x 14)
    (is (= 14 (py.ba:size x)))))
