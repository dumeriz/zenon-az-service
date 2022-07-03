(defpackage :zaz-requests
  (:use :cl)
  (:nicknames :z/reqs)
  (:import-from :z/db :timestamp :kind
		:project-new
		:votes-update-project
		:votes-update-phase
		:status-update-project
		:status-update-phase
		:phase-new
		:phase-update-existing
		:old
		:new)
  (:import-from :z/projects
		:id :name :created :description :url :znn :qsr :status :votes
		:no :yes :abstain
		:owner :phases
		:project-id)
  (:export :*requests*
	   :handle-request))

(in-package :zaz-requests)

(defvar *requests* '("last-updates" ; parameter n, replies with the timestamps of the recent n <= 50 updates
		     "updates-since" ; parameter timestamp, replies with the updates since that ts
		     "projects" ; no parameter, replies with all existing projects
		     "active-projects" ; no parameter, replies with projects that are in status Voting or Active
		     "project-current-phase")) ; parameter project-id; replies with the current project phase

(defclass request ()
  ((r :initarg :r :reader r)
   (data :initarg :data :reader data)))

(defclass last-updates-request (request) ())

(defclass updates-since-request (request) ())

(defclass projects-request (request) ())

(defclass active-projects-request (request) ())

(defclass project-current-phase-request (request) ())

(defclass reply () ())

(defclass last-updates-reply (reply)
  ((timestamps :initarg :timestamps :reader timestamps)))

(defclass updates-since-reply (reply)
  ((updates :initarg :updates :reader updates)))

(defclass projects-reply (reply)
  ((projects :initarg :projects :reader projects)))

;;(defclass active-projects-reply (reply)
;;  ((projects :initarg :projects :reader projects)))

(defclass project-current-phase-reply (reply)
  ((phase :initarg :phase :reader project-phase)))

(defmethod jsown:to-json ((u z/db:update))
  (jsown:to-json (jsown:new-js ("timestamp" (timestamp u))
			       ("type" (kind u))
			       ("update" (z/db:data u)))))

(defmethod jsown:to-json ((v z/projects:votes))
  (jsown:to-json (jsown:new-js ("yes" (:yes v))
			       ("no" (:no v))
			       ("abstain" (:abstain v)))))

(defmethod jsown:to-json ((p z/projects:project))
  (jsown:to-json (jsown:new-js ("id" (:id p))
			       ("name" (:name p))
			       ("owner" (:owner p))
			       ("created" (:created p))
			       ("description" (:description p))
			       ("url" (:url p))
			       ("znn" (:znn p))
			       ("qsr" (:qsr p))
			       ("status" (:status p))
			       ("phases" (:phases p))
			       ("votes" (:votes p)))))

(defmethod jsown:to-json ((phase z/projects:project-phase))
  (jsown:to-json (jsown:new-js ("id" (:id phase))
			       ("pid" (:project-id phase))
			       ("name" (:name phase))
			       ("created" (:created phase))
			       ("description" (:description phase))
			       ("url" (:url phase))
			       ("znn" (:znn phase))
			       ("qsr" (:qsr phase))
			       ("status" (:status phase))
			       ("votes" (:votes phase)))))

(defmethod jsown:to-json ((u z/db:project-new))
  (let ((p (:new u)))
    (jsown:to-json (jsown:new-js ("timestamp" (:ts u))
				 ("type" "project:new")
				 ("id" (:id p))
				 ("data" p)))))

(defmethod jsown:to-json ((u z/db:votes-update-project))
  (jsown:to-json (jsown:new-js ("timestamp" (:ts u))
			       ("type" "project:votes-update")
			       ("id" (:id (:new u)))
			       ("data" (:votes (:new u))))))

(defmethod jsown:to-json ((u z/db:votes-update-phase))
  (let ((phase (car (last (:phases (:new u))))))
    (jsown:to-json (jsown:new-js ("timestamp" (:ts u))
				 ("type" "phase:votes-update")
				 ("id" (:id phase))
				 ("pid" (:id (:new u)))
				 ("data" (:votes phase))))))

(defmethod jsown:to-json ((u z/db:status-update-project))
  (jsown:to-json (jsown:new-js ("timestamp" (:ts u))
			       ("type" "project:status-update")
			       ("id" (:id (:new u)))
			       ("old" (:status (:old u)))
			       ("new" (:status (:new u))))))

(defmethod jsown:to-json ((u z/db:status-update-phase))
  (flet ((phase-status (p)
	   (when p (:status (car (last (:phases p)))))))
    (jsown:to-json (jsown:new-js ("timestamp" (:ts u))
				 ("type" "phase:status-update")
				 ("id" (:id (car (last (:phases (:new u))))))
				 ("pid" (:id (:new u)))				 
				 ("old" (phase-status (:old u)))
				 ("new" (phase-status (:new u)))))))

(defmethod jsown:to-json ((u z/db:phase-new))
  (let ((new-phase (car (last (:phases (:new u))))))
    (jsown:to-json (jsown:new-js ("timestamp" (:ts u))
				 ("type" "phase:new")
				 ("id" (:id new-phase))
				 ("data" new-phase)))))

(defmethod jsown:to-json ((u z/db:phase-update-existing))
  (let ((new-phase (car (last (:phases (:new u)))))
	(old-phase (car (last (:phases (:old u))))))
    (jsown:to-json (jsown:new-js ("timestamp" (:ts u))
				 ("type" "phase:update")
				 ("id" (:id new-phase))
				 ("old" (:id old-phase))
				 ("data" new-phase)))))

(defmethod jsown:to-json ((u last-updates-reply))
  (jsown:to-json (timestamps u)))

(defmethod jsown:to-json ((u updates-since-reply))
  (jsown:to-json (updates u)))

(defmethod jsown:to-json ((p projects-reply))
  (jsown:to-json (projects p)))

;;(defmethod jsown:to-json ((p active-projects-reply))
;;  (let ((filtered (loop for p in (projects p) when (< 2 (:status p)) collect p)))
;;    (jsown:to-json filtered)))

(defun collect-active-projects ()
  (let ((ht (make-hash-table :test #'equal)))
    (loop for k being the hash-keys in (z/db:projects) using (hash-value v)
          when (< (:status v) 2) do (setf (gethash k ht) v))
    ht))
  
(defmethod jsown:to-json ((p project-current-phase-reply))
  (jsown:to-json (project-phase p)))

(defgeneric reply (request)
  (:documentation "Handles a specific request; replies with json data.")
  (:method ((request last-updates-request))
    (jsown:to-json
     (handler-case
	 (let ((n (read-from-string (if (data request) (first (data request)) "10"))))
	   (make-instance 'last-updates-reply :timestamps (z/db:recent-updates-timestamps n)))
       (t (c) (jsown:new-js
		("error" (format nil "Error: ~A" c)))))))
  (:method ((request updates-since-request))
    (jsown:to-json
     (handler-case
	 (let ((ts (read-from-string (first (data request)))))
	   (make-instance 'updates-since-reply
			  :updates (z/db:updates-since ts)))
       (t (c) (jsown:new-js
		("error" (format nil "No valid timestamp in data: ~a" c)))))))
  (:method ((request projects-request))
    (let ((projects (z/db:projects :ids (when (data request)
					  (jsown:parse (first (data request)))))))
      (jsown:to-json
       (make-instance 'projects-reply :projects projects))))
  (:method ((request active-projects-request))
    (let ((projects (collect-active-projects)))
      (jsown:to-json
       (make-instance 'projects-reply :projects projects))))
  (:method ((request project-current-phase-request))
    (jsown:to-json
     (handler-case
	 (let ((p (z/db:get-project (first (data request)))))
	   (if (or (not p) (not (:phases p)))
	       (jsown:new-js
		 ("error" (format nil "Can't find matching project for request")))
	       (let ((phase (car (last (:phases p)))))
		 (make-instance 'project-current-phase-reply
				:phase phase))))
       (t (c)
	 (jsown:new-js
	   ("error" (format nil "Failed to get phase for request: ~a" c))))))))

(defun handle-request (request data)
  "Constructs the handler name from `REQUEST' and returns its result after applying to `DATA'"
  (let* ((sym (intern (string-upcase (str:concat request "-request")) :zaz-requests))
	 (js (reply (make-instance sym :r request :data data))))
    (v:debug :rep "Request: ~A => ~A" request js)
    js))
