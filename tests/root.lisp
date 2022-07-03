(in-package :zaz-test)

(def-suite zaz-tests
    :description "Test suite for the zaz system")

(defun import-projects ()
  (flex:with-input-from-sequence (s +binary-projects+)
    (z/projects:import-api-projects (cl-store:restore s))))

(defvar *projects* nil "Stores the imported projects for tests")

(def-fixture archived-projects ()
  (setq *projects* (import-projects))
  (&body)
  (setq *projects* nil))

(defun reset-db-from (projects)
  (z/db:reset-db)
  (z/db:import-projects projects))
  
