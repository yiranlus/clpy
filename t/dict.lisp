(in-package :clpy.test)

(def-suite dict-test
  :in clpy
  :description "Test CLPY system")

(in-suite dict-test)

(test new-empty-dict
  (py:let ((dict (py.dict:new)))
    (is (= 0 (py.dict:size dict)))))

(test new-dict-1
  (py:let ((dict (py.dict:new
		  (list
		   (cons "key 1" "value 1")
		   (cons "key 2" "value 2")
		   (cons "key 3" 3)))))
    (is (= 3 (py.dict:size dict)))
    (py:let ((v (py.str:encode (py.dict:get-item dict "key 1"))))
      (is (string-equal "value 1" (py.bytes:as-string v))))))

(test clear-dict
  (py:let ((dict (py.dict:new
		  (list
		   (cons "key 1" "value 1")
		   (cons "key 2" "value 2")
		   (cons "key 3" 3)))))
    (is (= 3 (py.dict:size dict)))
    (py.dict:clear dict)
    (is (= 0 (py.dict:size dict)))))

(test dict-contains
  (py:let ((dict (py.dict:new
		  (list
		   (cons "key 1" "value 1")
		   (cons "key 2" "value 2")
		   (cons "key 3" 3)))))
    (is (= 3 (py.dict:size dict)))
    (is-true (py.dict:contains dict "key 1"))
    (is-false (py.dict:contains dict "key-non-exist"))))

(test dict-del
  (py:let ((dict (py.dict:new
		  (list
		   (cons "key 1" "value 1")
		   (cons "key 2" "value 2")
		   (cons "key 3" 3)))))
    (is-true (py.dict:contains dict "key 1"))
    (py.dict:del-item dict "key 1")
    (is-false (py.dict:contains dict "key 1"))))

(test dict-keys
  (py:let ((dict (py.dict:new
		  (list
		   (cons "key 1" "value 1")
		   (cons "key 2" "value 2")
		   (cons "key 3" 3)))))
    (py:let ((keys (py.dict:keys dict)))
      (is (= 3 (py.list:size keys))))))

(test dict-values
  (py:let ((dict (py.dict:new
		  (list
		   (cons "key 1" "value 1")
		   (cons "key 2" "value 2")
		   (cons "key 3" 3)))))
    (py:let ((values (py.dict:values dict)))
      (is (= 3 (py.list:size values))))))

(test dict-merge-non-override
  (py:let ((dict-merged (py.dict:new
		  (list
		   (cons "key1" "value 1")
		   (cons "key2" "value 2")
		   (cons "key3" 3))))
	   (dict2 (py.dict:new
		  (list
		   (cons "key3" "value 1 updated")
		   (cons "key4" "value 4")
		   (cons "key5" 7)))))
    (py.dict:merge dict-merged dict2)
    (is (= 5 (py.dict:size dict-merged)))
    (py:let ((key3-v (py.dict:get-item dict-merged "key3")))
      (is-true (py.type:of key3-v :long))
      (is (= 3 (py.number:as-integer key3-v))))))

(test dict-merge-override
  (py:let ((dict-merged (py.dict:new
		  (list
		   (cons "key1" "value 1")
		   (cons "key2" "value 2")
		   (cons "key3" 3))))
	   (dict2 (py.dict:new
		  (list
		   (cons "key3" "value 1 updated")
		   (cons "key4" "value 4")
		   (cons "key5" 7)))))
    (py.dict:merge dict-merged dict2 :override t)
    (is (= 5 (py.dict:size dict-merged)))
    (py:let ((key3-v (py.dict:get-item dict-merged "key3")))
      (is-false (py.type:of key3-v :long))
      (is-true (py.type:of key3-v :str))
      (py:let ((v (py.str:encode key3-v)))
	(is (string-equal "value 1 updated" (py.bytes:as-string v)))))))

'(test dict-merge-update
  (py:let ((dict-merged (py.dict:new
		  (list
		   (cons "key1" "value 1")
		   (cons "key2" "value 2")
		   (cons "key3" 3))))
	   (dict2 (py.dict:new
		  (list
		   (cons "key3" "value 1 updated")
		   (cons "key4" "value 4")
		   (cons "key5" 7)))))
    (py.dict:update dict-merged dict2)
    (is (= 5 (py.dict:size dict-merged)))
    (py:let ((key3-v (py.dict:get-item dict-merged "key3")))
      (is-false (py.type:of key3-v :long))
      (is-true (py.type:of key3-v :str))
      (py:let ((v (py.str:encode key3-v)))
	(is (string-equal "value 1 updated" (py.bytes:as-string v)))))))
