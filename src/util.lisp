(defpackage :zaz-util
  (:use :cl)
  (:nicknames :z/util)
  (:export :when-let
	   :normalize-funds
	   :create-const-class
	   :random-string
	   :unix-time))

(in-package :zaz-util)

(defun normalize-funds (funds-with-decimals)
  "Divide `FUNDS-WITH-DECIMALS' by 10^8."
  (/ funds-with-decimals (expt 10 8)))

(defmacro when-let ((var fn) &body body)
  "Expand `BODY' in a context where `VAR' is bound to the result of `FN' when `FN' does not return NIL."
  `(let ((,var ,fn))
     (when ,var (progn ,@body))))

(defvar *charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "The default alphabet for `RANDOM-STRING'")

(defun random-string (len &key (alphabet *charset*))
  "Generates a random string of `LEN` characters over `ALPHABET`."
  (let ((seed (make-random-state t))
	(alen (length alphabet)))
    (map 'string
	 (lambda (c)
	   (declare (ignore c))
	   (elt alphabet (random alen seed)))
	 (make-string len))))

(defun lispify-string (string)
  "Returns a lispified representation of `STRING'. Uppercase letters are replaced with
lowercase, with dash prepended, unless it is the initial character which is only lowercased. Underscores are replaced with dashes."
  (let ((param-string (str:param-case string)))
    (if (char= (elt string 0) #\_)
	(str:concat "-" param-string)
	param-string)))

(defun str->varname (str)
  "Transforms `STR' into a representation that is usable as lisp symbol."
  (string-upcase (lispify-string str)))

(defmacro create-const-class (name super &rest slot-names)
  "Defines a class `NAME' derived from `SUPER'
with readable keyword slots named after `SLOT-NAMES' that also have an initializer
of the same keyword name. Each name in `SLOT-NAMES' will be lispified prior usage."
  `(defclass ,name (,super)
     (,@(loop for s in slot-names for symname = (str->varname s)
	      for k = (alexandria:make-keyword symname)
              collecting `(,(intern symname)
			    :reader ,k :initarg ,k)))))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun unix-time ()
  (universal-to-unix-time (get-universal-time)))
