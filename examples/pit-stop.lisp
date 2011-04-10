;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/pit-stop.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: pit-stop.lisp,v 1.13 2006/01/15 03:42:52 dalal Exp $

(in-package :user)

(defapplication "Pit Stop"
    :init (initialize-sim))

;;;
;;; The idea of this simulation is that a driver is driving down the
;;; the road. Every once in a while, the driver reads a sign; the driver
;;; can remember the last 7 signs.
;;;
;;; Eventually, the driver gets hungry. If the driver remembers a
;;; a recent rest stop sign, s/he stops, otherwise s/he keeps looking
;;; for a rest stop sign.
;;; 
;;; S/he will only stop once in the simulation, although s/he gets
;;; hungry twice in the simulation.
;;;
;;; About 1/2 the time, s/he will see the rest stop sign before
;;; getting hungry for the first time.
;;; 
;;;
;;; Details: the messages from the signs are randomly signalled from
;;;  outside the agent. Because I'm not modelling vision here, these
;;;  get put right into the agent's mind via COGEVENT.
;;; 
;;; The simulation ends when a (END OF ROAD) sign is encountered. This
;;; is scheduled outside the agent, too.
;;;
;;; The procedures TRACK-SIGNS and TRACK-HUNGER are just for debugging.
;;;
;;; It's possible that the agent will never stop for food, if the
;;; rest stop sign is (a) more than 7 signs previous (simulating that
;;; the agent has forgotten it, (b) more than 30 minutes previous (simulating
;;; that it's too far to go back) and no rest stops are seen before
;;; the end of the road.

;;;
;;; -- some probability stuff. 


(defparameter *sign-messages* 
    '(|burma shave|
      |stay at schulumps hotel|
      |gas next left|
      |see worlds largest chicken|
      |only 1000 miles to sea shell city|
      |rest stop|))


;; six signs -- with .99 confidence, we'll see (rest stop) in about 26
;; trials.

(defparameter *sign-trials* 
    (trials-required .99
		     (/ 1 (length *sign-messages*))))

(defun random-elt (x)
  (elt x (random (length x))))

(defun random-sign ()
  (random-elt  *sign-messages*))

(defun random-signs (n agent)
  (dotimes (i n)
    (cogevent  `(message-read vision = ,(random-sign))
	       agent
	       :trigger-asa T)))

(defun last-message (agent)
  (cogevent  `(message-read vision = |end of road|)
	     agent
	     :trigger-asa T))

(defun signal-hunger (agent)
  (cogevent  `(hunger increased)
	     agent
	     :trigger-asa T))

(defun initialize-sim ()
  (let ((my-agent
	 (make-instance 'agent :name "Mr. Creosote"
	   :locale (make-instance 'locale)
	   :initial-task '(do-domain))))
    (dotimes (i *sign-trials*)
      (schedule
       (list (random-value (* i 60 1000) (* (* i 60 1000) .15)) 'ms)
       (random-signs 1 my-agent)))
    (schedule 
     (list (* (+ *sign-trials* 30) 60 1000) 'ms)
     (last-message my-agent))
    (schedule 
     (list (* 60 4 1000) 'ms)
     (signal-hunger my-agent))
    (schedule
     (list (* 60 10 1000) 'ms)
     (signal-hunger my-agent))
    (show hunger)
    (show message-read)
    (show interrupted)
    (show resumed)
    
    ))


(defun random-value (mean diff)
  (floor (random-triangular (+ mean diff) diff)))

;;; --------------------------------
;;; PDL Starts here 
;;; 
;;; --------------------------------

;; (use (hand) for 100 +/- 50 ms)
(primitive
    (index (use resource (?resource) for ?mean +/- ?std ?units))
  (profile ?resource)
  (duration (list (floor (random-value ?mean ?std)) ?units)))

(primitive
    (index (print ?x . ?y))
  (on-start (format t "~a~{ ~a~}~%" ?x ?y)))

(procedure
    (index (drive))
  (step s1 (use resource (car) for 60 +/- 15 min))
  (step term (terminate) (waitfor ?s1)))

(procedure
    (index (track signs))
  (profile vision)
  (step s2 (print "Saw a sign" ?message)
	(waitfor (:measurement o1 (message-read vision = ?message))))
  (step s3 (reset +this-task+)
	(waitfor ?s2)))

(procedure
    (index (track hunger))
  (profile stomach)
  (step s2 (print "Stomach says I am hungry")
	(waitfor (hunger increased)))
  (step s3 (reset +this-task+)
	(waitfor ?s2)))

(procedure 
    (index (make rest stop))
  (step s1 (use resource (car) for 5 +/- 1 min)
	(waitfor (:measurement o1 (message-read vision = |rest stop|)
			       :timestamp (>= (- (start-of +this-task+) P30M)))))
  (step s2 (print "Stopping for food."))
  (step s3 (cogevent (hunger diminished) +self+)
	(waitfor ?s1))
  (step s4 (terminate)
	(waitfor ?s1 ?s2 ?s3)))

(procedure  
    (index (do-domain))
  (log ( (message-read vision) :count-limit 7) ) ;; remember last 7 signs ...
  (step s1 (drive)
	(priority 1))
  ;(step s2 (track signs))
  ;(step s3 (track hunger))
  (step s4 (make rest stop)
	(priority 9)
	(waitfor (hunger increased)))
  (step term (terminate)
	(waitfor (message-read vision = |end of road|))))



;; (startapp)