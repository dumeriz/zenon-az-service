(defsystem "zaz"
  :version "1.0.0"
  :author "Dumeril"
  :license "GPL3"
  :depends-on (:alexandria
	       :verbose
	       :uiop
	       :str
	       :bordeaux-threads
	       :pzmq
	       :jsown
               :jsonrpc
	       :cl-store
	       :cl-zenon)

  :components ((:module "src"
		:serial t
                :components
                ((:file "util")
		 (:file "api")
		 (:file "projects")
		 (:file "projects-db")
		 (:file "pillar-stats")
		 (:file "filter")
		 (:file "outbound/zmq")		 
		 (:file "outbound/channel")
		 (:file "outbound/pub")
		 (:file "outbound/rep")
		 (:file "outbound/requests")		 
		 (:file "outbound/channels")
		 (:file "main"))))
  :description "Backend to serve bots with information about zenon accelerator-z projects"
  :in-order-to ((test-op (test-op :zaz/test))))

(defsystem "zaz/test"
  :author "Dumeril"
  :license "MIT"
  :description "Test system for various functionality of zaz (coverage incomplete)"
  :depends-on (:zaz
	       :alexandria
	       :flexi-streams
               :fiveam)
  :components ((:module "tests"
		:serial t
                :components
                ((:file "package")
		 (:file "root")
		 (:file "data")
		 (:file "projects")
		 (:file "projects-db"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run-all-tests)))
