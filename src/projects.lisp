(defpackage :zaz-projects
  (:use :cl)
  (:nicknames :z/projects)
  (:export :project
	   :project-phase
	   :votes
	   :import-api-projects
	   :phase-was-added
	   :phase-was-updated
	   :status-changed
	   :votes-changed
	   :same
	   :copy
	   ;; project props
	   :id :name :created :description :url :znn :qsr :status :votes
	   :no :yes :abstain
	   :owner :phases
	   :project-id
	   :active?))

(in-package :zaz-projects)

(defclass zaz-type () ())

(z/util:create-const-class
 votes zaz-type "no" "yes" "abstain")

(z/util:create-const-class
 project-base zaz-type "id" "name" "created" "description" "url" "znn" "qsr" "status" "votes")

(z/util:create-const-class
 project project-base "owner" "phases")

(z/util:create-const-class project-phase project-base "project-id")

(defmethod copy-base-project ((p project-base) cls)
  (make-instance cls
		 :id (:id p)
		 :name (:name p)
		 :created (:created p)
		 :description (:description p)
		 :url (:url p)
		 :znn (:znn p)
		 :qsr (:qsr p)
		 :status (:status p)
		 :votes (copy (:votes p))))
  
(defgeneric copy (thing)
  (:documentation "Create a deep copy of `THING'")
  (:method ((v votes))
    (make-instance 'votes :no (:no v) :yes (:yes v) :abstain (:abstain v)))
  (:method ((p project))
    (let ((project (copy-base-project p 'project)))
      (setf (slot-value project 'owner) (:owner p)
	    (slot-value project 'phases) (mapcar #'copy (:phases p)))
      project))
  (:method ((p project-phase))
    (let ((phase (copy-base-project p 'project-phase)))
      (setf (slot-value phase 'project-id) (:project-id p))
      phase)))

;;; Local definitions

(defun import-votes (api-votes)
  (let ((no (:no api-votes))
	(yes (:yes api-votes)))
    (make-instance 'votes :no no :yes yes :abstain (- (:total api-votes) yes no))))

(defun import-phase (api-phase)
  (let ((phase (:phase api-phase))
	(votes (:votes api-phase)))
    (make-instance 'project-phase
		   :id (:id  phase)
		   :name (:name phase)
		   :created (:creation-timestamp phase)
		   :description (:description phase)
		   :url (:url phase)
		   :znn (z/util:normalize-funds (:znn-funds-needed phase))
		   :qsr (z/util:normalize-funds (:qsr-funds-needed phase))
		   :status (:status phase)
		   :project-id (:project-id phase)
		   :votes (import-votes votes))))
  
(defun import-phases (api-phases)
  (mapcar #'import-phase api-phases))

(defun import-project (api-project)
  "Imports the `API-PROJECT' that was returned from `CL-ZENON' into local representation.
Mainly access to some slots is simplified and the coin values are normalized to 0 decimals."
  (let ((votes (import-votes (:votes api-project)))
	(phases (import-phases (:phases api-project))))
    (make-instance 'project
		   :id (:id api-project)
		   :name (:name api-project)
		   :created (:creation-timestamp api-project)
		   :description (:description api-project)
		   :url (:url api-project)
		   :znn (z/util:normalize-funds
			 (:znn-funds-needed api-project))
		   :qsr (z/util:normalize-funds
			 (:qsr-funds-needed api-project))
		   :status (:status api-project)
		   :owner (:owner api-project)
		   :votes votes
		   :phases phases)))

(defun project-table-from-api-result (api-projects)
  "Converts the list of api-returned data in `API-PROJECTS' into a hashtable in local representation."
  (let ((ht (make-hash-table :test #'equal))
	(projects (mapcar #'import-project api-projects)))
    (dolist (p projects)
      (setf (gethash (:id p) ht) p))
    ht))

;;; API

;; Wrapping PROJECT-TABLE-FROM-API-RESULT for export
(defun import-api-projects (api-projects)
  "Converts the list of api-returned data in `API-PROJECTS' into a hashtable in local representation."
  (project-table-from-api-result api-projects))

(defgeneric same (x y)
  (:documentation "Equality comparison for local types")
  (:method ((v1 votes) (v2 votes))
    (and (= (:yes v1) (:yes v2))
	 (= (:no v1) (:no v2))
	 (= (:abstain v1) (:abstain v2))))
  (:method ((p1 project-base) (p2 project-base))
    (string= (:id p1) (:id p2))))

(defgeneric votes-changed (p1 p2)
  (:documentation "T if the `VOTES' field in `P-REF' differs from that in `P-NEW'.")
  (:method ((p1 project-base) (p2 project-base))
    (not (same (:votes p1) (:votes p2)))))

(defgeneric status-changed (p1 p2)
  (:documentation "T if the `STATUS' field in `P-REF' differs from that in `P-NEW'.")
  (:method ((p1 project-base) (p2 project-base))
    (not (= (:status p1) (:status p2)))))

(defmethod phase-was-updated ((p1 project) (p2 project))
  (when (same p1 p2)
    (let ((ph1 (:phases p1))
	  (ph2 (:phases p2)))
      (and (= (length ph1) (length ph2))
	   (not (null ph1))
	   (not (equal (:id (car (last ph1)))
		       (:id (car (last ph2)))))))))

(defmethod phase-was-added ((p1 project) (p2 project))
  (when (same p1 p2)
    (not (= (length (:phases p1)) (length (:phases p2))))))

(defun active? (p)
  "T if the `p' is in status VOTING or ACTIVE"
  (< (:status p) 2))
