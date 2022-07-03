(in-package :zaz-test)

(def-suite* zaz-projects-db :in zaz-tests)

(defun project-with-phases-copy (projects)
  (z/projects:copy
   (loop for p in (alexandria:hash-table-values projects)
      if (:phases p) do (return p))))

(def-fixture archived-projects ()
  (setq *projects* (import-projects))
  (&body)
  (setq *projects* nil))

;; Inserting a new project returns an instance of z/db:project-new
(def-test empty-db-new-project ()
  (z/db:reset-db)
  (with-fixture archived-projects ()
      (let* ((id (car (alexandria:hash-table-keys *projects*)))
	     (ht (make-hash-table :test #'equal)))
	(setf (gethash id ht) (gethash id *projects*))
	(is (typep (car (z/db:import-projects ht)) 'z/db:project-new)))))

;; Updating the status of a project returns a
;; - status-update-project if no new phase was added
;; - phase-new if a new phase was added
(def-test project-new-status ()
  (z/db:reset-db)
  (with-fixture archived-projects ()
    (z/db:import-projects *projects*)
    (let* ((project1 (z/projects:copy (caddr (alexandria:hash-table-values *projects*))))
	   (project2 (z/projects:copy (cadddr (alexandria:hash-table-values *projects*))))
	   (new-phase (make-instance 'z/projects:project-phase
				     :id "abcd"
				     :votes (make-instance 'z/projects:votes :yes 0 :no 0 :abstain 0)
				     :status 0))
	   (ht (make-hash-table :test #'equal)))
      (setf (slot-value project1 'zaz-projects::status)
	    (1+ (or (:status project1) 0))
	    (slot-value project1 'zaz-projects::status)
	    (1+ (or (:status project2) 0))
	    (slot-value project2 'zaz-projects::phases)
	    (cons new-phase (:phases project2))
	    (gethash (:id project1) ht) project1
	    (gethash (:id project2) ht) project2)
      (let ((result (z/db:import-projects ht)))
	(is-true (member 'z/db:status-update-project
			 result :key #'type-of))
	(is-true (member 'z/db:phase-new
			 result :key #'type-of))))))

;; Updating the votes for a project returns a
;; - votes-update-project if the status did not change
;; - status-update-project if the status did change
;; - phase-new if the status did change and a new phase was added
(def-test project-new-votes ()
  (with-fixture archived-projects ()
    ;; find one with an existing phase
    (let* ((project1 (project-with-phases-copy *projects*))
	   (ht (make-hash-table :test #'equal)))
      ;; update votes for project
      (reset-db-from *projects*)
      (setf (gethash (:id project1) ht) project1
	    (slot-value (:votes project1) 'z/projects::yes) 100)
      (is (typep (car (z/db:import-projects ht)) 'z/db:votes-update-project))
      ;; update votes for project and its status
      (reset-db-from *projects*)
      (setf (slot-value project1 'z/projects::status) 100)
      (is (typep (car (z/db:import-projects ht)) 'z/db:status-update-project))
      ;; update votes for project and its status and add a phase
      (reset-db-from *projects*)
      (setf (slot-value project1 'z/projects::phases) (cons (car (:phases project1)) (:phases project1)))
      (is (typep (car (z/db:import-projects ht)) 'z/db:phase-new)))))

;; Updating the votes for a phase returns a
;; - votes-update-project if the phase-status did not change
;; - status-update-phase if the status did change
;; - phase-new if the status did change and a new phase was added
(def-test project-phase-new-votes ()
  (with-fixture archived-projects ()
    ;; find one with an existing phase
    (let* ((project1 (project-with-phases-copy *projects*))
	   (ht (make-hash-table :test #'equal)))
      ;; changing votes for current phase
      (setf (gethash (:id project1) ht) project1
	    (slot-value (:votes (car (last (:phases project1)))) 'z/projects::yes) 100)
      (reset-db-from *projects*)
      (is (car (z/db:import-projects ht)) 'z/db:votes-update-phase 
	  :test #'typep)
      ;; changing votes and status for current phase
      (setf (slot-value (car (last (:phases project1))) 'z/projects::status) 100)
      (reset-db-from *projects*)
      (is (car (z/db:import-projects ht)) 'z/db:status-update-phase 
	  :test #'typep)
      ;; changing votes, status for recent phase and adding a new one
      (setf (slot-value project1 'z/projects::phases) (cons (car (:phases project1)) (:phases project1))
	    (slot-value (car (last (:phases project1))) 'z/projects::id) "abc")
      (reset-db-from *projects*)
      (is (car (z/db:import-projects ht)) 'z/db:phase-new 
	  :test #'typep))))

;; Adding a phase to a project returns a db:phase-new
(def-test project-new-phase ()
  (z/db:reset-db)
  (with-fixture archived-projects ()
    (z/db:import-projects *projects*)
    ;; find one with an existing phase
    (let* ((project (project-with-phases-copy *projects*))
	   (phases (:phases project))
	   (ht (make-hash-table :test #'equal)))
      (setf (gethash (:id project) ht) project
	    (slot-value project 'z/projects::phases) (cons (car phases) phases)
	    (slot-value (car (last phases)) 'z/projects::id) "abcde")
      (is (car (z/db:import-projects ht)) 'z/db:phase-new 
	  :test #'typep))))

;; Updating the phase-id of a project returns a phase-update-existing
(def-test project-new-phase-id ()
  (z/db:reset-db)
  (with-fixture archived-projects ()
    (z/db:import-projects *projects*)
    ;; find one with an existing phase
    (let ((project (project-with-phases-copy *projects*))
	  (ht (make-hash-table :test #'equal)))
      (setf (gethash (:id project) ht) project
	    (slot-value (car (:phases project)) 'z/projects::id) "abcde")
      (is (car (z/db:import-projects ht))
	  'z/db:phase-update-existing
	  :test #'typep))))
		 
;; Updating the status of a phase returns a
;; - status-update-phase if no new phase is added and the phase-id did not change
;; - phase-update-existing if no new phase is added and the phase-id changed
;; - phase-new if a new phase is added
(def-test project-new-phase-status ()
  (with-fixture archived-projects ()
    ;; find one with an existing phase
    (let ((new-status (project-with-phases-copy *projects*))
	  (new-status-new-id (project-with-phases-copy *projects*))
	  (new-status-new-phase (project-with-phases-copy *projects*))
	  (ht (make-hash-table :test #'equal)))

      ;; new status
      (setf (gethash (:id new-status) ht) new-status
	    (slot-value (car (last (:phases new-status))) 'z/projects::status) 10)
      (reset-db-from *projects*)
      (is (typep (car (z/db:import-projects ht)) 'z/db:status-update-phase))

      ;; new status new phase-id
      (setf (gethash (:id new-status) ht) new-status-new-id
	    (slot-value (car (last (:phases new-status-new-id))) 'z/projects::status) 10
      	    (slot-value (car (last (:phases new-status-new-id))) 'z/projects::id) "abc")
      (reset-db-from *projects*)
      (is (car (z/db:import-projects ht)) 'z/db:phase-update-existing
	  :test #'typep)

      ;; new status added phase
      (setf (gethash (:id new-status) ht) new-status-new-phase
	    (slot-value (car (:phases new-status-new-phase)) 'z/projects::status) 10
	    (slot-value new-status-new-phase 'z/projects::phases) (cons (car (:phases new-status-new-phase))
									  (:phases new-status-new-phase))
      	    (slot-value (car (last (:phases new-status-new-phase))) 'z/projects::id) "abc")
      (reset-db-from *projects*)
      (is (car (z/db:import-projects ht)) 'z/db:phase-new
	  :test #'typep))))

