(defpackage :zaz-channels
  (:use :cl)
  (:nicknames :z/chan)
  (:export :create
	   :publish
	   :stop))

(in-package :zaz-channels)

(defvar *context* (pzmq:ctx-new) "The zmq context")

(defstruct ports rep pub)
(defstruct channels rep pub)

(defparameter *channels* nil)

(defun create (request-handler valid-requests &optional ports)
  "Initializes publisher and replier channels on zmq context `*CONTEXT*'.
If `PORTS' of type `PORTS' is NIL, both channels are established on random ports.
If any port in `PORTS' is NIL, that channel will be setup randomly.
Any received request from `VALID-REQUESTS' will be send to `REQUEST-HANDLER', which must reply with a string."
  (let ((publisher (make-instance 'z/pub:channel :port (when ports
							 (ports-pub ports))))
	(replier (make-instance 'z/rep:channel :port (when ports
						       (ports-rep ports)))))
    (setf *channels* (make-channels :rep replier
				    :pub publisher))
    (z/ch:setup publisher *context*)
    (z/ch:setup replier *context*)
    (z/rep:set-handler request-handler valid-requests)
    (v:info :chan
	    "Setup publisher on ~A, reply-channel on ~A"
	    (z/ch:port publisher) (z/ch:port replier))))

(defun reply-port ()
  (when *channels*
    (channels-rep *channels*)))

(defun pub-port ()
  (when *channels*
    (channels-pub *channels*)))

(defun publish (msg &rest more)
  "Distribute `MSG' and optional `MORE' messages through the publisher."
  (z/util:when-let (publisher (pub-port))
    (apply #'z/pub:publish publisher msg more)))

(defun stop ()
  (v:info :chan "Stopping channels")
  (when *channels*
    (z/ch:teardown (channels-rep *channels*))
    (z/ch:teardown (channels-pub *channels*)))
  (setf *channels* nil))
