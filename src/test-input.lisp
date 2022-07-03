;;;; Unit for manual test executions to test some features of external clients

(defpackage :zaz-test-input
  (:use :cl)
  (:nicknames :z/ti)
  (:export :random/add
	   :random/change-state
	   :random/change-votes
	   :random/phase-add
	   :random/phase-change-state
	   :random/phase-change-votes
	   :random/phase-reset))

(in-package :zaz-test-input)

;;; Helper

(defmacro with-projects-backup (&body body)
  (alexandria:with-gensyms (original)
    `(let ((,original (z/db:projects)))
       (unwind-protect (progn ,@body)
	 (setf z/db::*projects* ,original)))))

(defun random-id ()
  (z/util:random-string 64))

(defun random-status ()
  (random 5))

(defun random-url ()
  (str:concat "https://www." (z/util:random-string 10) ".org"))

(defun votes/new-random ()
  (let ((yes (random 20))
	(no (random 20)))
    (make-instance 'accelerator::votes :no no :yes yes :total (+ yes no) :id (random-id))))

(defun phase/new-random (project-id)
  (let ((timestamp (- (z/util:unix-time) (random (* 30 24 60 60))))
	(znn (random (* 2000 (expt 10 8))))
	(qsr (random (* 10000 (expt 10 8)))))
    (make-instance 'accelerator::project-phase
		   :id (random-id) :project-id project-id
		   :name (z/util:random-string 10) :description (z/util:random-string 30) :url (random-url)
		   :znn-funds-needed znn :qsr-funds-needed qsr
		   :creation-timestamp timestamp :accepted-timestamp 0 :status (random-status))))
    
(defun phases/new-random (project-id amount)
  (mapcar (lambda (_)
	    (make-instance 'accelerator::phases
			   :phase (phase/new-random project-id)
			   :votes (votes/new-random)))
	  (alexandria:iota amount)))

(defun project/new-random (phase-count)
  (let* ((id (random-id))
	 (timestamp (- (z/util:unix-time) (* 30 24 60 60)))
	 (znn (random (* 5000 (expt 10 8))))
	 (qsr (random (* 5000 (expt 10 8))))
	 (phases (when (< 0 phase-count) (phases/new-random id phase-count))))
    (make-instance 'accelerator::accelerator-project
		   :id id :name (z/util:random-string 5) :owner (z/util:random-string 10)
		   :description (z/util:random-string 30) :url (random-url)
		   :znn-funds-needed znn :qsr-funds-needed qsr :creation-timestamp timestamp :last-update-timestamp timestamp
		   :status (random-status) :phase-ids (mapcar (lambda (p) (:id (:phase p))) phases)
		   :votes (votes/new-random) :phases phases)))

;;; Executed from every test function

(defun run-test (new-db)
  (let ((update (z/db:import-projects new-db)))
    (format t "~A~%" update)
    ;; (z/db:add-updates update)
    ;; (z/db:make-backup)
    (mapc (lambda (u)
	    (jsown:to-json u))
	    ;;(z/chan:publish (jsown:to-json u)))
	  update)))

(defmacro with-test-run (var &body body)
  `(with-projects-backup
     (let ((,var (z/db:projects)))
       (progn ,@body)
       (run-test ,var))))

;;; Changes to projects

(defun make-project-update (projects)
  (z/projects:import-api-projects
   (if (listp projects) projects (list projects))))

(defun insert-project-update (db p)
  (let ((new (make-project-update p)))
    (alexandria:maphash-keys (lambda (k) (setf (gethash k db) (gethash k new)))
			     new)))

;; publishing type project:new
(defun random/add ()
  (with-test-run db
    (insert-project-update db (project/new-random 0))))

;; publishing type project:status-update
(defun random/change-state ()
  (flet ((non-finished-project (db)
	   (loop for p being the hash-value in db if (< (:status p) 4) do (return p))))
    (with-test-run db
      (let ((p (non-finished-project db)))
	(setf (slot-value p 'z/projects:status) (1+ (:status p)))))))

;; publishing type project:votes-update
(defun random/change-votes ()
  (flet ((some-project (db)
	   (nth (random (hash-table-count db)) (alexandria:hash-table-values db))))
    (with-test-run db
      (let ((p (some-project db)))
	(setf (slot-value (:votes p) 'z/projects:yes) (1+ (:yes (:votes p))))))))

;;; Changes to phases

(defun api-votes-from-votes (votes)
  (make-instance 'accelerator::votes :no (:no votes) :yes (:yes votes) :total (+ (:yes votes) (:no votes) (:abstain votes))))

(defun api-phase-from-phase (p)
  (make-instance 'accelerator::project-phase
		 :id (:id p) :project-id (:project-id p)
		 :name (:name p) :description (:description p) :url (:url p)
		 :znn-funds-needed (* (:znn p) (expt 10 8)) :qsr-funds-needed (* (:qsr p) (expt 10 8)) :creation-timestamp (:created p)
		 :accepted-timestamp 0 :status (:status p)))

(defun api-project-from-db (project)
  (make-instance 'accelerator::accelerator-project
		 :id (:id project) :name (:name project) :owner (z/util:random-string 10)
		 :description (:description project) :url (:url project)
		 :znn-funds-needed (* (:znn project) (expt 10 8)) :qsr-funds-needed (* (:qsr project) (expt 10 8))
		 :creation-timestamp (:created project) :last-update-timestamp (:created project)
		 :status (:status project) :phase-ids (mapcar (lambda (p) (:id p)) (:phases project))
		 :votes (api-votes-from-votes (:votes project)) :phases (mapcar (lambda (ph)
										  (make-instance 'accelerator::phases
												 :phase (api-phase-from-phase ph)
												 :votes (api-votes-from-votes (:votes ph))))
										(:phases project))))

;; publishing type phase:new
(defun random/phase-add ()
  (with-test-run db
    (let* ((p (api-project-from-db
	       (nth (random (hash-table-count db))
		    (alexandria:hash-table-values db))))
	   (new (phase/new-random 0)))
      (setf (slot-value p 'accelerator::phases) (append (:phases p)
							(list (make-instance 'accelerator::phases
									     :phase new
									     :votes (votes/new-random))))
	    (slot-value p 'accelerator::phase-ids) (mapcar (lambda (p) (:id (:phase p))) (:phases p)))
      (insert-project-update db p))))

;; publishing type phase:update
(defun random/phase-reset ()
  (with-test-run db
    (let* ((p (api-project-from-db (loop for v being the hash-values in db if (:phases v) do (return v))))
	   (ph (car (last (:phases p)))))
      (setf (slot-value (:phase ph) 'accelerator::id) (random-id))
      (insert-project-update db p))))

;; publishing type phase:status-update
(defun random/phase-change-state ()
  (with-test-run db
    (let* ((p (api-project-from-db (loop for v being the hash-values in db if (:phases v) do (return v))))
	   (ph (car (last (:phases p)))))
      (setf (slot-value (:phase ph) 'accelerator::status) (1+ (:status (:phase ph))))
      (insert-project-update db p))))

;; publishing type phase:votes-update
(defun random/phase-change-votes ()
  (with-test-run db
    (let* ((p (api-project-from-db (loop for v being the hash-values in db if (:phases v) do (return v))))
	   (ph (car (last (:phases p)))))
      (setf (slot-value (:votes ph) 'accelerator::yes) (1+ (:yes (:votes ph))))
      (insert-project-update db p))))

(defun send-all-test-data (&optional fileout)
  (labels ((pub (j)
	     (z/chan:publish j))
	   (to-file (j)
	     (format fileout "~A~%" j))
	   (send (data)
	     (let ((j (jsown:to-json data)))
	       (if fileout (to-file j) (pub j)))))
    (unless fileout (send (jsown:new-js ("type" "demo-updates-start"))))
    (dolist (fn '(random/add random/change-state random/change-votes
		  random/phase-add random/phase-change-state random/phase-change-votes random/phase-reset))
      (send (car (funcall fn))))
    (unless fileout (send (jsown:new-js ("type" "demo-updates-end"))))))

(defun test-data-to-file (fname)
  (with-open-file (f fname
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (send-all-test-data f)))

