(cl:defpackage :clpy.str
  (:nicknames :py.str)
  (:use :cl)
  (:shadow #:replace #:find #:count #:length)
  (:export #:new
	   #:new-from
	   #:p
	   #:encode
	   #:concat
	   #:split
	   #:split-lines
	   #:join
	   #:replace
	   #:compare
	   #:rich-compare
	   #:find
	   #:find-char
	   #:count
	   #:tail-match
	   #:contains
	   #:length))

(cl:in-package :clpy.str)

(clpy.type:define-type "PyUnicode_Type" str)
(clpy.type:define-type "PyUnicodeIter_Type" str-iter)

(defun p (o)
  (or (clpy.type:of o :str)
      (clpy.type:subtype-p (clpy.object:ob-type o)
			    (clpy.type:get :str))))

(defun exact-p (o)
  (clpy.type:of o :str))

(defun new-from (v)
  "Create a Unicode object from a another object..

`V should be already encoded in Unicode. If you want to create a
Unicode object from a normal string, use :cl:function:new."
  (clpy.ffi.fns:py-unicode-from-string v))

(defun new (str &optional (encoding :fs-default) &key errors stateful byte-order mapping)
  (let ((-errors (if errors errors (cffi:null-pointer)))
	(-byte-order (if byte-order
			 (autowrap:alloc :int)
			 (cffi:null-pointer)))
	(-consumed t))
    (let ((res
	    (clpy.util:ensure-null-as-nil
		(cond
		  ((stringp encoding)
		   (clpy.ffi.fns:py-unicode-decode str (cl:length str) encoding -errors))
		  (mapping
		   (clpy.ffi.fns:py-unicode-decode-charmap str (cl:length str) mapping -errors))
		  (stateful
		   (plus-c:c-with ((consumed clpy.ffi:py-ssize-t))
		     (case encoding
		       (:utf8 (clpy.ffi.fns:py-unicode-decode-utf8stateful str (cl:length str) -errors))
		       (:utf32 (clpy.ffi.fns:py-unicode-decode-utf32stateful str (cl:length str) -errors -byte-order))
		       (:utf16 (clpy.ffi.fns:py-unicode-decode-utf16stateful str (cl:length str) -errors -byte-order))
		       (:utf7 (clpy.ffi.fns:py-unicode-decode-utf7stateful str (cl:length str) -errors))
		       #+win32
		       (:mbcs (clpy.ffi.fns:py-unicode-decode-mbcs-stateful str (cl:length str) -errors))
		       (otherwise (error (format nil "Decoder of ~A does not have a statefull version." encoding))))
		     (setf -consumed consumed)))
		  (t
		   (case encoding
		     (:locale (clpy.ffi.fns:py-unicode-decode-locale str -errors))
		     (:fs-default (clpy.ffi.fns:py-unicode-decode-fs-default str))
		     (:unicode-escape (clpy.ffi.fns:py-unicode-decode-unicode-escape str (cl:length str) -errors))
		     (:raw-unicode-escape (clpy.ffi.fns:py-unicode-decode-raw-unicode-escape str (cl:length str) -errors))
		     (:utf8 (clpy.ffi.fns:py-unicode-decode-utf8 str (cl:length str) -errors))
		     (:utf32 (clpy.ffi.fns:py-unicode-decode-utf32 str (cl:length str) -errors -byte-order))
		     (:utf16 (clpy.ffi.fns:py-unicode-decode-utf16 str (cl:length str) -errors -byte-order))
		     (:utf7 (clpy.ffi.fns:py-unicode-decode-utf7 str (cl:length str) -errors))
		     (:latin1 (clpy.ffi.fns:py-unicode-decode-latin1 str (cl:length str) -errors))
		     (:ascii (clpy.ffi.fns:py-unicode-decode-ascii str (cl:length str) -errors))
		     #+win32
		     (:mbcs (clpy.ffi.fns:py-unicode-decode-mbcs str (cl:length str) -errors)))))
	      (error 'py.exc:generic-error))))
      (when byte-order
	(autowrap:free -byte-order))
      (values res -consumed))))

(clpy.smart:new-hook #'stringp #'new)
    
(defun encode (unicode &optional (encoding :fs-default) &key errors mapping code-page)
  (let ((-errors (if errors errors (cffi:null-pointer))))
    (clpy.util:ensure-null-as-nil
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
	     #+win32
	     (:mbcs (clpy.ffi.fns:py-unicode-as-mbcs-string unicode))
	     #+win32
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
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-concat left right)
    (error 'py.exc:generic-error)))

(defun split (unicode sep &optional maxsplit)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-split unicode sep (or maxsplit -1))
    (error 'py.exc:generic-error)))

(defun split-lines (unicode &optional (keep-ends nil))
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-splitlines unicode (if keep-ends 0 1))
    (error 'py.exc:generic-error)))

(defun join (sep seq)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-join sep seq)
    (error 'py.exc:generic-error)))

(defun replace (unicode substr replstr &key maxcount)
  (clpy.util:ensure-null-as-nil
      (clpy.ffi.fns:py-unicode-replace unicode substr replstr (or maxcount -1))
    (error 'py.exc:generic-error)))

;; compare

(defun compare (left right)
  "Compare two strings, return -1, 0, or 1.

LEFT string should be an Unicode object, RIGHT can be a string or an
Unicode object."
  (clpy.exception:return-or-raise-python-error
   (if (stringp right)
       (clpy.ffi.fns:py-unicode-compare-with-ascii-string left right)
       (clpy.ffi.fns:py-unicode-compare left right))))


(defun rich-compare (left right op)
  "Rich compare two Unicode strings.

The return value can be ``T``, NIL, or Py_NotImplemented. Possible
values for OP are :GT, :GE, :EQ, :NE, :LT, :LE."
  (let ((py-op (case op
		 (:GT clpy.type:+PY-GT+)
		 (:GE clpy.type:+PY-GE+)
		 (:EQ clpy.type:+PY-EQ+)
		 (:NE clpy.type:+PY-NE+)
		 (:LT clpy.type:+PY-LT+)
		 (:LE clpy.type:+PY-LE+))))
    (clpy.util:ensure-null-as-nil
	(clpy.ffi.fns:py-unicode-rich-compare left right py-op))))

;; search

(defun find (unicode substr &optional (start 0) end (direction :forward))
  (let ((res (clpy.ffi.fns:py-unicode-find unicode substr
					   start (or end (length unicode))
					   (case direction
					     (:forward 1)
					     (:backward -1)))))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun find-char (unicode ch start end direction)
  (let ((res (clpy.ffi.fns:py-unicode-find-char unicode ch
						start (or end (length unicode))
						(case direction
						  (:forward 1)
						  (:backward -1)))))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun count (unicode substr &optional (start 0) end)
  (let ((res (clpy.ffi.fns:py-unicode-count unicode substr
					    start (or end (length unicode)))))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun tail-match (unicode substr &key (start 0) end (direction :FORWARD))
  "Return ``T`` if SUBSTR matches unicode[start:end] at the given tail end.

DIRECTION can be :FORWARD which mean a suffix match; or :BACKWARD which
means prefix match."
  (let ((-end (or end (1- (length unicode)))))
    (clpy.util:ensure-non-negative
	(clpy.ffi.fns:py-unicode-tailmatch
	 unicode substr start -end
	 (case direction
	   (:FORWARD 1)
	   (:BACKWARD -1)
	   (otherwise (error 'clpy.exception:generic-error
			     :message "Invalid value for DIRECTION"))))
      (clpy.exception:raise-generic-or-python-error))))

(defun contains (unicode substr)
  (let ((res (clpy.ffi.fns:py-unicode-contains unicode substr)))
    (if (= res -2)
	(error 'py.exc:generic-error)
	res)))

(defun is-identifier (unicode)
  "Return T if the string is valid identifier according to the language definition, otherwise nil"
  (zerop (clpy.ffi.fns:py-unicode-is-identifier unicode)))

;; properties

(defun length (unicode)
  (clpy.util:ensure-non-negative
      (clpy.ffi.fns:py-unicode-get-length unicode)
    (error 'py.exc:generic-error)))


