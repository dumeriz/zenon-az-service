(defpackage :zaz-channel
  (:use :cl)
  (:nicknames :z/ch)
  (:export :channel :setup :teardown :port :socket))

(in-package :zaz-channel)

(defclass channel ()
  ((port :initarg :port :reader port)
   (socket :initform nil :accessor socket)
   (active :initform nil :accessor active)))

(defgeneric setup (channel context)
  (:documentation "Initializes `channel' using ZMQ context `CONTEXT'")
  (:method :after (c ctx)
	   (when (port c) (setf (active c) t))))

(defgeneric teardown (channel)
  (:documentation "Closes the channel")
  (:method :after (c) (setf (active c) nil)))

