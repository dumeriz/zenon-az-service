(defpackage :zaz-main
  (:use :cl)
  (:nicknames :z)
  (:import-from :z/util :when-let)
  (:export :init :main :stop))

(in-package :zaz-main)

(defvar *thread* nil "Stores the background update thread instance.")
(defvar *stopped* nil "When set `*THREAD*' stops. Don't set manually, use `STOP'.")

(defun setup-logging (&key data-dir)
  (let ((logfile-template (merge-pathnames
			   (make-pathname :directory
					  `(:relative ".cache" ,data-dir "log")
					  :name "cl.log")
			   (user-homedir-pathname))))
    (v:add-pipe (v:define-pipe ()
		  (v:rotating-file-faucet :template logfile-template
					  :interval :weekly)))))

(defun info (&rest args)
  (apply #'v:info :main args))

(defun silent (&rest args)
  "Only goes to logfile, not stdout"
  (apply #'v:debug :main args))

(defun get-current-projects (endpoint)
  "Retrieves the current az-projects from `ENDPOINT' and translates them into internal types."
  (handler-case
    (z/projects:import-api-projects
     (z/api:current-projects endpoint))
    (t (c)
      (v:error :main "~A" c))))

(defun update-from (endpoint)
  "Requests the current accelerator projects and generates `PROJECT' instances.
Requests are directed to a node at `ENDPOINT'. Results are stored in the local db
and from the change set compared with its previous state, update instances are
created via `PROJECT-UPDATE-FOR-*', depending on the update type."
  (info "Running update")
  (z/db:import-projects
   (z/filter:minus-ignored
    (get-current-projects endpoint))))

(defun send-updates (updates)
  "Delivers `UPDATES' to the zmq publisher channel package."
  (when updates
    (mapc (lambda (u)
	    (let ((js (jsown:to-json u)))
	      (silent "Publishing update js")
	      (z/chan:publish (jsown:to-json u))))
	  updates)))

(defun maybe-send-pillar-stats (endpoint &key force)
  (when-let (update (z/p-stats:get-current endpoint :force force))
    (info "Publishing pillar rates update for ~A pillars" (length update))
    (z/chan:publish "pillar-stats" (jsown:to-json update))))

(defun add-updates-make-backup (updates)
  (info "Got ~A updates" (length updates))
  (z/db:add-updates updates)
  (z/db:make-backup))

(defun run-update (endpoint)
  "Initiates a backup and update of the local db from a zenon node.
The node must have the 'embedded.accelerator.getAll' api function enabled
and be reachable via `ENDPOINT'.
Updates will be stored in the local db and send out via a zmq publisher channel."
  (let ((updates (update-from endpoint)))
    (add-updates-make-backup updates)
    (send-updates updates)
    (maybe-send-pillar-stats endpoint)))

(defun init (&optional data-dir)
  (setup-logging :data-dir (or data-dir "zaz"))
  (when data-dir
    (v:info :main "Initializing with data-dir in ~A" (z/db:program-folder))
    (setf z/db::*data-dir* data-dir))
  (z/db:try-restore))

(defun stop ()
  "Triggers `*THREAD*' to stop and joins it."
  (info "Stopping outbound channels")
  (z/chan:stop)
  (setq *stopped* t)
  (info "Waiting for *THREAD* to join")
  (when (and (bt:threadp *thread*)
	     (bt:thread-alive-p *thread*))
    (bt:join-thread *thread*))
  (setq *thread* nil))

(defun wait (seconds)
  "Does nothing for `SECONDS' seconds.
Checks `*STOPPED*' every 10 seconds so that it never blocks longer if `*STOPPED' is set."
  (loop for i upto (/ seconds 10) until *stopped*
        do (sleep 10)))
        
(defun main-loop (endpoint cycle-minutes ports)
  "Background loop that calls `RUN-UPDATE' on the provided `ENDPOINT' every `CYCLE-MINUTES'."
  (info "Starting outbound channels")
  (z/chan:create #'z/reqs:handle-request z/reqs:*requests* ports)
  (info "Starting background update loop with period=~A minutes" cycle-minutes)
  (loop
     with seconds = (* cycle-minutes 60)
     until *stopped* do
       (progn
	 (run-update endpoint)
	 (wait seconds)))
  (info "Background update loop done"))

(defun main (endpoint &key data-dir (cycle-minutes 10) pub rep)
  "Initializes the local db from `DATA-DIR' and starts `MAIN-LOOP' in a thread stored in `*THREAD*'.
Pass a single folder name as `DATA-DIR' to not use the default (~/.local/share/zaz/).
Optionally define both `PUB' and `REP' as fixed port numbers for the request- and publish channels.
If not, they'll be set randomly."
  (unless *thread*
    (let ((ports
	   (cond ((and pub rep)
		  (z/chan::make-ports :rep rep :pub pub))
		 ((or pub rep)
		  (error "Define both REP and PUB or neither"))
		 (t nil))))
      (init data-dir)
      (setq *stopped* nil
	    *thread* (bt:make-thread
		      (lambda ()
			(main-loop endpoint cycle-minutes ports))
		      :name "update-thread")))))
