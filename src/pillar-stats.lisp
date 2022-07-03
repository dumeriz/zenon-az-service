(defpackage :zaz-pillar-stats
  (:use :cl)
  (:nicknames :z/p-stats)
  (:export :get-current
	   :pillar-participation-rate
	   :ppr-pillar
	   :ppr-votes
	   :ppr-rate
	   :ppr-active-votes
	   :ppr-active-rate))

(in-package :zaz-pillar-stats)

;; update runs every 2 hours
(defvar *update-period* 7200
  "Minimum period in seconds between two updates")

(defvar *last-update* 0
  "Timestamp of the last successfull update")

(defun get-pillar-names (endpoint)
  (z/api:current-pillar-names endpoint))

(defstruct (pillar-participation-rate (:conc-name ppr-))
  ;; active-* only considers projects/phases that are currently active or voted on
  pillar votes active-votes rate active-rate)

(defun gen-rates (pillar-name all-votes all-ids active-ids)
  (let ((active-votes
	 (remove-if-not (lambda (v)
			  (member v active-ids :test #'equal))
			all-votes
			:key :id)))
    (v:info :stats "For ~A: votes=~A (~A active) for ~A/~A possible"
	    pillar-name (length all-votes)
	    (length active-votes) (length all-ids)
	    (length active-ids))
    (make-pillar-participation-rate
     :pillar pillar-name
     :votes all-votes
     :active-votes active-votes
     :rate (/ (length all-votes) (length all-ids))
     :active-rate (/ (length active-votes) (length active-ids)))))

(defun update-due ()
  (< *update-period* (- (get-universal-time) *last-update*)))

(defun project-and-phase-ids (&key active)
  "Collects the ids for all projects and phases, possibly limited to those still `ACTIVE'"
  (flet ((get-phase-ids (project-id)
	   (let ((all (z/db:project-phase-ids project-id)))
	     (if active (last all) all)))
	 (rem-nils (lst)
	   (remove-if-not #'identity lst)))
    (let ((p-ids (z/db:project-ids :only-active active)))
      (rem-nils (apply #'append p-ids (mapcar #'get-phase-ids p-ids))))))

(defun collect-pillar-votes-for (client pillars ids)
  "For a list of pillar names `PILLARS' and a list of `IDS',
collect a list of cons cells with the pillar name in the first and a list of `PILLAR-VOTE' objects
in the second place."
  (z/api:pillar-total-votes client pillars ids))

(defun missed-votes (pillar endpoint &key active-only)
  "Used to check from the REPL which votes have been missed by a specific pillar"
  (let* ((ids (project-and-phase-ids :active active-only))
	 (votes (z/api:pillar-total-votes endpoint pillar ids)))
    (values
     (mapcar #'z/db:get-by-id
	     (remove-if (lambda (id)
			  (member id votes :test #'equal :key :id))
			ids))
     ids
     votes)))

(defun get-current (endpoint &key force pillar-list)
  (when (or force pillar-list (update-due))
    (let ((active-ids (project-and-phase-ids :active t))
	  (all-ids (project-and-phase-ids))
	  (pillars (or pillar-list (get-pillar-names endpoint))))
      (labels ((get-pillar-votes ()
		 (v:info :stats "Getting votes")
		 (collect-pillar-votes-for endpoint pillars all-ids))
	       (pillar-participation-rates (p-name-votes)
		 (v:info :stats "Generating rates")
		 (gen-rates (car p-name-votes)
			    (cdr p-name-votes) all-ids active-ids))
	       (run-update ()
		 (v:info :stats "Running update")
		 (mapcar #'pillar-participation-rates
			 (get-pillar-votes))))
	(v:info :stats "Getting ~:[scheduled~;forced~] update" (or force pillar-list))
	(z/util:when-let
	 (update (handler-case (run-update)
		   (t (c) (v:error :stats "~A" c))))
	 (unless pillar-list
	   (setq *last-update* (get-universal-time))
	   (v:info :stats "Update finished at ~A" *last-update*))
	 update)))))
