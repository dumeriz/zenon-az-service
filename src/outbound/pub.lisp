(defpackage :zaz-publisher
  (:use :cl)
  (:nicknames :z/pub)
  (:export :channel
	   :bind
	   :publish))

(in-package :zaz-publisher)

(defmethod jsown:to-json ((zps z/p-stats:pillar-participation-rate))
  (jsown:to-json (jsown:new-js ("name" (z/p-stats:ppr-pillar zps))
			       ("rate" (float (z/p-stats:ppr-rate zps)))
			       ("active_rate" (float (z/p-stats:ppr-active-rate zps))))))

(defclass channel (z/ch:channel) ())

(defmethod z/ch:setup ((channel channel) context)
  (let ((options '((:rcvtimeo . 1000)
		   (:sndtimeo . 100)
		   (:linger . 200))))
    (with-slots (z/ch:socket z/ch:port) channel
      (setf z/ch:socket (pzmq:socket context :pub))
      (z/zmq:set-socket-options z/ch:socket options)
      (setf z/ch:port (z/zmq:bind-to z/ch:socket z/ch:port :public t)))))

(defmethod z/ch:teardown ((channel channel))
  (v:info :publisher "Tearing down publisher"))
  
(defun publish (channel msg &rest more)
  "Publishes `MSG' and, if given, `MORE' as a multipart message."
  (flet ((send (s more)
	   (let ((dbg (subseq s 0 (min (length s) 25))))
	     (v:info :publisher "Publishing ~A~A" dbg (if (< 25 (length s)) "..." ""))
	     (v:debug :publisher "Exact: ~A" dbg))
	   (pzmq:send (z/ch:socket channel)
		      s
		      :dontwait t
		      :sndmore more)))
    (send msg more)
    (maplist (lambda (rest)
	       (send (car rest) (cdr rest)))
	     more)))
   


  

