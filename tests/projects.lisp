(in-package :zaz-test)

(def-suite* zaz-projects :in zaz-tests)

(def-test copies ()
  (with-fixture archived-projects ()
    (let ((p (car (alexandria:hash-table-values *projects*))))
      (is (z/projects:same (:votes p) (:votes (z/projects:copy p))))
      (is (z/projects:same p (z/projects:copy p))))))

(def-test votes-comparison ()
  (flet ((with-added-vote (project choice)
	   (let* ((abstain (:abstain (:votes project)))
		  (yes (:yes (:votes project)))
		  (no (:no (:votes project)))
		  (votes (make-instance 'z/projects::votes
				       :abstain (if (eq choice :abstain) (1+ abstain) abstain)
				       :yes (if (eq choice :yes) (1+ yes) yes)
				       :no (if (eq choice :no) (1+ no) no)))
		  (copy (z/projects:copy project)))
	     (setf (slot-value copy 'z/projects::votes) votes)
	     copy)))
    (with-fixture archived-projects ()
      (let* ((project (car (alexandria:hash-table-values *projects*))))
	(is (not (z/projects:votes-changed project project)))
	(is (z/projects:votes-changed (with-added-vote project :yes) project))
	(is (z/projects:votes-changed (with-added-vote project :no) project))
	(is (z/projects:votes-changed (with-added-vote project :abstain) project))))))
	
