(defpackage :zaz-projects-db
  (:use :cl)
  (:nicknames :z/db)
  (:export :try-restore
	   :make-backup
	   :reset-db
	   :program-folder
	   ;; project related
	   :import-projects
	   :projects
	   :project-ids
	   :project-phase-ids
	   :get-project
	   :get-by-id
	   :get-project-by-name
	   ;; update classes
	   :project-new
	   :votes-update-project
	   :votes-update-phase
	   :status-update-project
	   :status-update-phase
	   :phase-new
	   :phase-update-existing
	   :old
	   :new
	   ;; update class
	   :update
	   :timestamp
	   :kind
	   :data
	   ;; update functions
	   :make-update
	   :add-updates
	   :updates-since
	   :recent-updates-timestamps))

(in-package :zaz-projects-db)

(defvar *lock* (bt:make-lock))

(defvar *projects* (make-hash-table :test #'equal)
  "`*PROJECTS*' is the local db - a hashtable storing projects under their `ID'.")

(defvar *updates* '()
  "`*UPDATES*' is the queue of the most recent updates to projects.")

(defvar +max-updates+ 50 "The maximum size of `*UPDATES*'")

(defvar *data-dir* "zaz"
  "Where backup data and other program data is stored, relative to ~/.local/share/")

(defvar *backup-file* (make-pathname :name "backup" :type "cls")
  "File storing the serialization of the existing local database, see `MAKE-BACKUP'.")

(defun reset-db ()
  (v:info :db "DB-Reset")
  (bt:with-lock-held (*lock*)
    (setq *projects* (make-hash-table :test #'equal)
	  *updates* '())))

(defclass db-update ()
  ((timestamp :initarg :ts :initform (z/util:unix-time) :reader ts)
   (data :initarg :data :reader data)))

(defun copy-projects (&key ids)
  (loop
     with ht = (make-hash-table :test (hash-table-test *projects*))
     for k being the hash-keys in *projects* using (hash-value v)
     when (or (not ids) (member k ids :test #'equal))
         do (setf (gethash k ht) (z/projects:copy v))
     finally (return ht)))

(defun program-folder ()
  (merge-pathnames (make-pathname :directory `(:relative ".local" "share" ,*data-dir*))
		   (user-homedir-pathname)))

(defun rest-of-list (lst to-remove)
  "Returns the remainder of `LST' after removing the first `TO-REMOVE' elements."
  (loop for i from 0 for x on lst if (<= to-remove i) do (return x)))

(defun add-updates (updates)
  "Add updates to `*UPDATES*', treating that place as a fifo-queue."
  (flet ((make-db-update (u)
	   (v:info :db "Adding ~A-update" (type-of u))
	   (make-instance 'db-update :data u :ts (:ts u))))
    (let ((to-remove (max 0 (- (apply #'+ (mapcar #'length (list updates *updates*)))
			       +max-updates+))))
      (setf *updates* (append (rest-of-list *updates* to-remove)
			      (mapcar #'make-db-update updates))))))

(defun ensure-backup-location ()
  (let ((folder (program-folder)))
    (ensure-directories-exist folder)
    folder))

(defun store (project)
  "Stores `PROJECT' in the local db; assumes `*LOCK*' held."
  (let ((old (gethash (:id project) *projects*)))
    (setf (gethash (:id project) *projects*) project)
    old))
  
(defclass update ()
  ((old :initarg :old :reader :old)
   (new :initarg :new :reader :new)
   (timestamp :initform (z/util:unix-time) :reader :ts)))

(z/util:create-const-class project-new update)
(z/util:create-const-class votes-update-project update)
(z/util:create-const-class votes-update-phase update)
(z/util:create-const-class status-update-project update)
(z/util:create-const-class status-update-phase update)
(z/util:create-const-class phase-new update)
(z/util:create-const-class phase-update-existing update)

(defgeneric get-update (x y)
  ;; fallback to not have to check for empty phases
  (:method (x y) (error "Should not be called: x=~A, y=~A" x y))
  (:method (x (p z/projects:project))
    'project-new)
  (:method ((existing z/projects:project) (new z/projects:project))
    (cond ((z/projects:status-changed existing new)
	   ;; status of the project changed. Check this after phase-updated/added
	   ;; in case both things happen simultaneously
	   'status-update-project)
	  ((z/projects:votes-changed existing new)
	   ;; votes on the project changed
	   'votes-update-project)
	  ((z/projects:phase-was-added existing new)
	   ;; a new phase started
	   'phase-new)
	  ((z/projects:phase-was-updated existing new)
	   'phase-update-existing)))

  ;; Only phases in the new project
  (:method (x (phases-new cons))
    'phase-new)
  
  ;; Specialization for lists of phases.
  ;; New phase and updated phase already been handled in the projects-specialization.
  (:method ((phases-existing cons) (phases-new cons))
    (let ((current-old (car (last phases-existing)))
	  (current-new (car (last phases-new))))
      (cond ((< (length phases-existing) (length phases-new))
	     'phase-new)
	    ((z/projects:votes-changed current-old current-new)
	     'votes-update-phase)
	    ((z/projects:status-changed current-old current-new)
	     'status-update-phase)))))

(defun insert (project)
  "Stores `PROJECT' in the database and returns an object describing
the change if the project is new or changed."
  (let* ((existing (store project))
	 (project-update (get-update existing project))
	 (phase-update (when (:phases project)
			 (get-update (or (null existing) (:phases existing))
				     (:phases project)))))
    ;; preferrably returning phase updates, in case both exist
    (z/util:when-let (update (or phase-update project-update))
      (v:info :db "Got update ~A for ~A" update (:name project))
      (make-instance update :old existing :new project))))

;;; API

(defun import-projects (project-ht)
  "Stores the projects from hash-table `PROJECT-HT' in the local db.
For each entry that is new or changed wrt the existing db, an update class is returned."
  (when (hash-table-p project-ht)
    (v:info :db "Importing ~A projects" (hash-table-count project-ht))
    (bt:with-lock-held (*lock*)
      (remove-if-not #'identity
		     (mapcar #'insert
			     (alexandria:hash-table-values
			      project-ht))))))

(defun project-exists (table project)
  "`T if the `PROJECT' already exists in the HASH-TABLE `TABLE'."
  (bt:with-lock-held (*lock*)
    (gethash (:id project) table)))

(defun projects (&key ids)
  "Returns a fresh copy of the current content of the database or the subset denoted by `IDS'."
  (bt:with-lock-held (*lock*)
    (copy-projects :ids ids)))

(defun updates-since (ts)
  (bt:with-lock-held (*lock*)
    (loop for x on *updates* when (<= ts (timestamp x))
          do (return (mapcar #'copy-update x)))))

(defun recent-updates-timestamps (n)
  "The timestamps of the last n updates"
  (bt:with-lock-held (*lock*)
    (loop for i below n
          for x in (reverse *updates*) collect x into lst
	  finally (return (reverse (mapcar #'ts lst))))))

(defun make-backup ()
  "Generates a backup for the current local db"
  (bt:with-lock-held (*lock*)
    (let ((file (merge-pathnames *backup-file* (ensure-backup-location))))
      (v:info :db "Saving backup in ~A" file)
      (cl-store:store `(:projects ,*projects* :updates ,*updates*) file))))

(defun load-backup ()
  (let ((file (merge-pathnames *backup-file* (ensure-backup-location))))
    (v:info :db "Loading backup from ~A" file)
    ;; doing python-style conditionals
    (ignore-errors (cl-store:restore file))))

(defun try-restore ()
  "Attempts to restore the database from a backup; non-NIL only when data was found."
  (z/util:when-let (backup (load-backup))
    (v:info :db "Restoring db from backup")
    (bt:with-lock-held (*lock*)
      (setf *projects* (getf backup :projects)
	    *updates* (getf backup :updates)))))

(defun get-project (id)
  "Retrieves the project stored under `ID' from the local db."
  (bt:with-lock-held (*lock*)
    (gethash id *projects*)))

(defun get-project-by-name (name)
  "Retrieves all projects with name `NAME' (exact match) from the local db."
  (bt:with-lock-held (*lock*)
    (loop for v being the hash-values in *projects*
	  when (equal name (:name v)) collect v)))

(defun get-by-id (id)
  "Retrieves the project for `ID', which can be a project or a phase id."
  (bt:with-lock-held (*lock*)
    (or (gethash id *projects*)
	(loop for project in (alexandria:hash-table-values *projects*)
	      when (member id (:phases project) :test #'equal :key :id) do (return project)))))

(defun project-ids (&key only-active)
  "Returns the ids of all or active projects (status < 2)"
  (bt:with-lock-held (*lock*)
    (let ((all (alexandria:hash-table-keys *projects*)))
      (if (not only-active)
	  all
	  (loop for key in all when (z/projects:active? (gethash key *projects*))
	        collect key)))))

(defun project-phase-ids (project-id)
  "Returns the ids of all phases for a project."
  (bt:with-lock-held (*lock*)
    (z/util:when-let (p (gethash project-id *projects*))
      (mapcar :id (:phases p)))))

(defun get-project-from-db (project)
  "Extracts the entry for `PROJECT' from the local db."
  (bt:with-lock-held (*lock*)
    (gethash (:id project) *projects*)))

(defun print-all-projects (&optional (projects *projects*))
  (loop for k being the hash-key of projects using (hash-value p)
        do (format t "~A => ~A~%" k (:name p))))

(defun print-recent-update (n)
  (let ((u (nth (- (length *updates*) n) *updates*)))
    (format t "~A~%~A~%" (data u) (:new (data u)))
    (:new (data u))))

(defun recent-update-json (n &optional updates)
  (let* ((lst (or updates *updates*))
	 (u (nth (- (length lst) n) lst)))
    (format t "For ~A:~%" (:name (:new (data u))))
    (jsown:to-json (data u))))

;;; Helper; don't use

(defun -update-database-with-timestamps ()
  "Helper method to include the timestamp retroactively after db-format update"
  (let ((updates (try-restore)))
    (when (slot-boundp (data (car updates)) 'timestamp)
      (error "The updates seem to be in the correct format already"))
    (loop for x in updates do (setf (slot-value (data x) 'timestamp) (ts x)) collect x)))
