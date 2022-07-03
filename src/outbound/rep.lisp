(defpackage :zaz-reply
  (:use :cl)
  (:nicknames :z/rep)
  (:import-from :z/util
		:when-let)
  (:export :channel
	   :setup
	   :set-handler))

(in-package :zaz-reply)

(defvar *stopped* nil
  "Signals the request listener to stop.")
(defvar *handler* nil
  "Stores the external handler for all valid requests")
(defvar *requests* '()
  "List of request strings that are accepted by `*HANDLER*'")

(defun request-reply (data)
  (when data
    (v:info :rep "Received request ~A" data))
  (when-let (request (first data))
    (if (and request *handler* (member request *requests* :test #'equal))
	(funcall *handler* request (rest data))
	(error "Unknown request ~A" request))))

(defun listener (socket)
  (v:info :rep "Starting listener")
  (loop
     until *stopped*
     for strings = (z/zmq:receive-strings socket)
     for reply = (handler-case (request-reply strings)
		   (t (c)
		     (v:info :rep "Error handling request ~A: ~A" strings c)
		     (jsown:to-json
		      (jsown:new-js ("error" (format nil "~A" c))))))
     when reply do (progn
		     (v:info :rep "Response: ~A" reply)
		     (z/zmq:send socket reply))))

(defclass channel (z/ch:channel)
  ((thread :accessor thread :initform nil)))

(defmethod z/ch:setup ((chan channel) context)
  (let ((options '((:rcvtimeo . 1000)
		   (:sndtimeo . 100)
		   (:linger . 200))))
    (with-slots (z/ch:socket z/ch:port thread) chan
      (setf z/ch:socket (pzmq:socket context :rep))
      (z/zmq:set-socket-options z/ch:socket options)
      (v:info :rep "Binding reply socket")
      (setf *stopped* nil
	    z/ch:port (z/zmq:bind-to z/ch:socket z/ch:port :public t)
	    thread (bt:make-thread (lambda ()
				     (listener z/ch:socket))
				   :name "REQ/REP-Listener")))))

(defmethod z/ch:teardown ((channel channel))
  (v:info :rep "Tearing down replier")
  (setq *stopped* t)
  (z/util:when-let (thread (thread channel))
    (when (bt:thread-alive-p thread)
      (bt:join-thread thread))))

(defun set-handler (handler valid-requests)
  (setf *handler* handler
	*requests* valid-requests))
