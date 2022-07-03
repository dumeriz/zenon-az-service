(defpackage :zaz-filter
  (:use :cl)
  (:nicknames :z/filter)
  (:export :ignore-list
	   :print-projects-identifiable
	   :latest-projects
	   :add-to-ignore-list
	   :minus-ignored))

(in-package :zaz-filter)

(defparameter *ignore-list* '("e893b" "05ca2" "7a32b" "ebc41" "38eb1" "5f30e" "5b80c" "28a39" "b3697"
			      "22a66" "5eac8" "e7d90" "181d7" "cc88a" "277cc" "2f726" "51db3" "8c5ad"
			      "4749d" "362b2" "e4438" "6de62" "ec5ff" "1c442" "f0ff3" "a3fa2" "5a115"
			      "50b11" "3bf33" "86ca1")
  "List of project id prefixes for proposals that are considered spam")
  
(defun ignore-list () *ignore-list*)

(defun id-prefix (thing)
  (if (stringp thing)
      (subseq thing 0 5)
      (subseq (:id thing) 0 5)))

(defun print-projects-identifiable (projects)
  (maphash (lambda (k v)
	     (format t "~A: ~A~%" (id-prefix k) (:name v)))
	   projects))

(defun project-ids-latest (projects &key (n 10))
  (subseq
   (mapcar #'id-prefix
	   (mapcar #'cdr
		   (sort (copy-seq (mapcar (lambda (k)
					     (cons (:created (gethash k projects)) k))
					   (alexandria:hash-table-keys projects)))
			 #'<
			 :key #'car)))
   (- (hash-table-count projects) n)))

(defun full-id-from-prefix (projects prefix)
  (find prefix (alexandria:hash-table-keys projects)
	:test #'string=
	:key #'id-prefix))

(defun project-name-by-prefix (projects id)
  (let ((full-id (full-id-from-prefix projects id)))
    (z/util:when-let (project (gethash full-id projects))
      (:name project))))

(defun latest-projects (projects &key (n 10))
  (let ((id-prefixes (project-ids-latest projects :n n)))
    (mapcar (lambda (id) (cons id (project-name-by-prefix projects id)))
	    id-prefixes)))
     
(defun project-by-prefix (projects id)
  (let ((full-id (full-id-from-prefix projects id)))
    (gethash full-id projects)))

(defun add-to-ignore-list (shorted-id)
  (setf *ignore-list*
	(remove-duplicates 
	 (cons shorted-id *ignore-list*)
	 :test #'string=)))

(defun is-on-ignorelist (key)
  (member (id-prefix key) *ignore-list* :test #'string=))
   
(defun minus-ignored (projects)
  (when projects
    (let ((ignored (remove-if-not #'is-on-ignorelist (alexandria:hash-table-keys projects))))
      (loop for k in ignored do (remhash k projects))
      projects)))
