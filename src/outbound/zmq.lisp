(defpackage :zaz-zmq
  (:use :cl)
  (:nicknames :z/zmq)
  (:export :bind-to
	   :bind-random
	   :set-socket-options
	   :receive-strings
	   :send))

(in-package :zaz-zmq)

(defun bind-random (socket &key (linger 200) (public nil))
  "Binds `SOCKET' to a random port on 127.0.0.1, returns port.
Sets the linger sockopt to `LINGER' (in ms) as well, preventing long blocks on shutdown."
  (let ((host (if public "*" "127.0.0.1")))
    (v:info :zmq "Binding random socket on tcp://~A" host)
    (pzmq:bind socket (format nil "tcp://~A:*" host))
    (pzmq:setsockopt socket :linger linger)
    (let* ((ep (pzmq:getsockopt socket :last-endpoint))
	   (port-pos (position #\: ep :from-end t)))
      (values
       (read-from-string (subseq ep (1+ port-pos)))))))

(defun bind-to (socket port &key (linger 20) (public nil))
  "Binds `SOCKET' on 127.0.0.1, returns the bound port.
If `PORT' is not NIL, that port is used, else see `BIND-RANDOM'.
Sets the linger sockopt to `LINGER' (in ms) as well, preventing long blocks on shutdown."
  (let ((host (if public "*" "127.0.0.1")))
    (if port
	(progn
	  (v:info :zmq "Binding socket to tcp://~A:~A" host port)
	  (pzmq:bind socket (format nil "tcp://~a:~a" host port))
	  (pzmq:setsockopt socket :linger linger)
	  port)
	(bind-random socket :linger linger :public public))))

(defun set-socket-options (socket opt-vals)
  "Sets options for `SOCKET' from `OPT-VALS', in which each entry is a cons cell
with a valid zmq options key in the car and the corresponding value in the cdr."
  (mapcar (lambda (opt-val)
	    (pzmq:setsockopt socket (car opt-val) (cdr opt-val)))
	  opt-vals))

(defun receive-strings (socket)
  "Receives string messages from `SOCKET' into a list.
This receives a single message from the socket which may consist of several message parts.
All parts are expected to be ascii-strings and are collected into a list.
No error handling is done; if receive fails, NIL is returned. That's most likely because
there's no message available and the socket timed out, but it could be an ignored error."
  (ignore-errors
    (loop collecting (pzmq:recv-string socket :encoding :ascii)
          while (pzmq:getsockopt socket :rcvmore))))
		    
(defun send (socket string)
  "Sends `STRING' through `SOCKET'."
  (pzmq:send socket string))
