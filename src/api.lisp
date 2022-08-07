(defpackage :zaz-api
  (:use :cl)
  (:nicknames :z/api)
  (:export :current-pillar-names
	   :pillar-total-votes
	   :current-projects))

(in-package :zaz-api)

(defvar *lock* (bt:make-lock) "Locking access to `ZENON/CONN:WITH-NODE-AT'")

(defmacro with-node-at (var endpoint &body body)
  "Wraps `ZENON/CONN:WITH-NODE-AT' into a mutex locked context.'"
  `(bt:with-lock-held (*lock*)
     (handler-case
	 (zenon/conn:with-node-at ,var ,endpoint ,@body)
       (t (c) (v:error :api "Condition during connection with ~A: ~A" endpoint c)))))

(defun current-pillar-names (endpoint)
  "Get all pillar names from the node connection at `ENDPOINT'"
  (flet ((pillar-names (list)
	   (mapcar (lambda (p) (:name p)) list)))
    (let ((page-entries 50))
      (with-node-at client endpoint
	(v:info :api "Requesting pillar names")
	(loop
	   for page from 0
	   for result = (pillar:get-all client page page-entries)
	   while (:list result)
	   nconcing (pillar-names (:list result)))))))

(defun pillar-total-votes (endpoint name-or-names ids)
  (flet ((get-single-pillar-votes (name client)
	   (remove-if-not #'identity
			  (accelerator:get-pillar-votes client name ids))))
    (with-node-at client endpoint
      (if (listp name-or-names)
	  (mapcar (lambda (name)
		    (cons name (get-single-pillar-votes name client)))
		  name-or-names)
	  (get-single-pillar-votes name-or-names client)))))
  
(defun current-projects (endpoint)
  "Get all current az-projects from the node connection at `ENDPOINT'"
  (let ((page-entries 50))
    (with-node-at client endpoint
      (v:info :api "Requesting AZ-projects")
      (loop
	 for page from 0
	 for result = (accelerator:get-all client page page-entries)
	 while (:list result)
	 nconcing (:list result) into projects
	 finally (return projects)))))
