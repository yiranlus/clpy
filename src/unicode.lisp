(cl:defpackage :clpy.str
  (:nicknames :py.str)
  (:use :cl)
  (:shadow #:replace #:find #:count)
  (:export #:new
	   #:encode
	   #:concat
	   #:split
	   #:split-lines
	   #:join
	   #:replace
	   #:compare
	   #:find
	   #:find-char
	   #:count
	   #:contains))

(cl:in-package :clpy.str)

(clpy.type:define-type "PyUnicode_Type" str)

(defun new (str &optional (encoding :fs-default) &key errors stateful byte-order mapping)
  (let ((-errors (if errors errors (cffi:null-pointer)))
	(-byte-order (if byte-order
			 (autowrap:alloc :int)
			 (cffi:null-pointer)))
	(-consumed t))
    (let ((res
	    (py:ensure-null-as-nil
		(cond
		  ((stringp encoding)
		   (clpy.ffi.fns:py-unicode-decode str (length str) encoding -errors))
		  (mapping
		   (clpy.ffi.fns:py-unicode-decode-charmap str (length str) mapping -errors))
		  (stateful
		   (plus-c:c-with ((consumed clpy.ffi:py-ssize-t))
		     (case encoding
		       (:utf8 (clpy.ffi.fns:py-unicode-decode-utf8stateful str (length str) -errors))
		       (:utf32 (clpy.ffi.fns:py-unicode-decode-utf32stateful str (length str) -errors -byte-order))
		       (:utf16 (clpy.ffi.fns:py-unicode-decode-utf16stateful str (length str) -errors -byte-order))
		       (:utf7 (clpy.ffi.fns:py-unicode-decode-utf7stateful str (length str) -errors))
		       (:mbcs (clpy.ffi.fns:py-unicode-decode-mbcs-stateful str (length str) -errors))
		       (otherwise (error (format nil "Decoder of ~A does not have a statefull version." encoding))))
		     (setf -consumed consumed)))
		  (t
		   (case encoding
		     (:locale (clpy.ffi.fns:py-unicode-decode-locale str -errors))
		     (:fs-default (clpy.ffi.fns:py-unicode-decode-fs-default str))
		     (:unicode-escape (clpy.ffi.fns:py-unicode-decode-unicode-escape str (length str) -errors))
		     (:raw-unicode-escape (clpy.ffi.fns:py-unicode-decode-raw-unicode-escape str (length str) -errors))
		     (:utf8 (clpy.ffi.fns:py-unicode-decode-utf8 str (length str) -errors))
		     (:utf32 (clpy.ffi.fns:py-unicode-decode-utf32 str (length str) -errors -byte-order))
		     (:utf16 (clpy.ffi.fns:py-unicode-decode-utf16 str (length str) -errors -byte-order))
		     (:utf7 (clpy.ffi.fns:py-unicode-decode-utf7 str (length str) -errors))
		     (:latin1 (clpy.ffi.fns:py-unicode-decode-latin1 str (length str) -errors))
		     (:ascii (clpy.ffi.fns:py-unicode-decode-ascii str (length str) -errors))
		     (:mbcs (clpy.ffi.fns:py-unicode-decode-mbcs str (length str) -errors)))))
	      (error 'py.exc:generic-error))))
      (when byte-order
	(autowrap:free -byte-order))
      (values res -consumed))))
    
(defun encode (unicode &optional (encoding :fs-default) &key errors mapping code-page)
  (let ((-errors (if errors errors (cffi:null-pointer))))
    (py:ensure-null-as-nil
	(cond
	  ((stringp encoding)
	   (clpy.ffi.fns:py-unicode-as-encoded-string unicode encoding -errors))
	  (mapping
	   (clpy.ffi.fns:py-unicode-as-charmap-string unicode mapping))
	  (t
	   (case encoding
	     (:locale (clpy.ffi.fns:py-unicode-encode-locale unicode -errors))
	     (:fs-default (clpy.ffi.fns:py-unicode-encode-fs-default unicode))
	     (:unicode-escape (clpy.ffi.fns:py-unicode-as-unicode-escape-string unicode))
	     (:raw-unicode-escape (clpy.ffi.fns:py-unicode-as-raw-unicode-escape-string unicode))
	     (:utf8 (clpy.ffi.fns:py-unicode-as-utf8string unicode))
	     (:utf16 (clpy.ffi.fns:py-unicode-as-utf16string unicode))
	     (:utf32 (clpy.ffi.fns:py-unicode-as-utf32string unicode))
	     (:latin1 (clpy.ffi.fns:py-unicode-as-latin1string unicode))
	     (:ascii (clpy.ffi.fns:py-unicode-as-ascii-string unicode))
	     (:mbcs (clpy.ffi.fns:py-unicode-as-mbcs-string unicode))
	     (:code-page (clpy.ffi.fns:py-unicode-encode-code-page code-page unicode -errors)))))
      (error 'py.exc:generic-error))))

;;(defun as-string (unicode &optional (encoding :fs-default))
;;  (py:let ((res 

;;(defun as (unicode format &key to buflen copy_null)
;;  (case format
;;   (:ucs4 (if to
;;	       (clpy.ffi.fns:py-unicode-as-ucs4 unicode to buflen (if copy_null 1 0))
;;	       (clpy.ffi.fns:py-unicode-as-ucs4copy unicode)))
;;    (:wchar (if to
;;		(clpy.ffi.fns:py-unicode-as-wide-char unicode to size)
;;    (:char)

;; operations

(defun concat (left right)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-concat left right)
    (error 'py.exc:generic-error)))

(defun split (unicode sep &optional maxsplit)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-split unicode sep (or maxsplit -1))
    (error 'py.exc:generic-error)))

(defun split-lines (unicode &optional (keep-ends nil))
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-splitlines unicode (if keep-ends 0 1))
    (error 'py.exc:generic-error)))

(defun join (sep seq)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-join sep seq)
    (error 'py.exc:generic-error)))

(defun replace (unicode substr replstr &key maxcount)
  (py:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-replace unicode substr replstr (or maxcount -1))
    (error 'py.exc:generic-error)))

;; compare

(defun compare (left right)
  (clpy.ffi.fns:py-unicode-compare left right))

;; search

(defun find (unicode substr &optional (start 0) end (direction :forward))
  (let ((res (clpy.ffi.fns:py-unicode-find unicode substr
					   start (or end (size unicode))
					   (case direction
					     (:forward 1)
					     (:backward -1)))))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun find-char (unicode ch start end direction)
  (let ((res (clpy.ffi.fns:py-unicode-find-char unicode ch
						start (or end (size unicode))
						(case direction
						  (:forward 1)
						  (:backward -1)))))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun count (unicode substr &optional (start 0) end)
  (let ((res (clpy.ffi.fns:py-unicode-count unicode substr
					    start (or end (size unicode)))))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun contains (unicode substr)
  (let ((res (clpy.ffi.fns:py-unicode-contains unicode substr)))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun is-identifier (unicode)
  "Return T if the string is valid identifier according to the language definition, otherwise nil"
  (zerop (clpy.ffi.fns:py-unicode-is-identifier unicode)))

;; properties

(defun size (unicode)
  (py:ensure-non-negative
      (clpy.ffi.fns:py-unicode-get-length unicode)
    (error 'py.exc:generic-error)))

;; others
