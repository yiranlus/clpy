(in-package :clpy.test)

(def-suite unicode-test
  :in clpy
  :description "Test CLPY system")

(in-suite unicode-test)

(test string-type-test
  (py:let ((x (py.str:new "lsjdflkj")))
    (is-true (py.str:p x))))

(test get-and-retrive
  (py:let ((x (py.str:new "this is a string.")))
    (is (string-equal
	 "this is a string."
	 (py.bytes:as-string (py.str:encode x))))))

(test get-and-retrive-fs-default
  (py:let ((x (py.str:new "this is a string." :fs-default)))
    (is (string-equal
	 "this is a string."
	 (py.bytes:as-string (py.str:encode x :fs-default))))))

(test concat-string
  (py:let ((x (py.str:new "Hello "))
	    (y (py.str:new "world!")))
    (py:let ((res (py.str:concat x y)))
      (is (string-equal
	   "Hello world!"
	   (py.bytes:as-string (py.str:encode res)))))))

(test join-string
  (py:let ((l (py.list:new "a" "b" "c" "d"))
	    (sep (py.str:new ",")))
    (py:let ((res (py.str:join sep l)))
      (is (string-equal
	   "a,b,c,d"
	   (py.bytes:as-string (py.str:encode res)))))))

(test split-string
  (py:let ((s (py.str:new "a,b,c,d,e"))
	    (sep (py.str:new ",")))
    (py:let ((res (py.str:split s sep)))
      (is (= 5 (py.list:size res))))))
