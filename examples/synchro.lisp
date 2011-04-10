;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/synchro.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: synchro.lisp,v 1.2 2006/03/15 23:25:11 will Exp $

(in-package :cl-user)

;;; This simple application indicates one way of synchronizing agents'
;;; communications with one another. The basic task is to chatter with
;;; one another -- in this case, just to say hello and goodbye. First
;;; an agent senses the availability of the other agent, while
;;; publishing its own availability. It starts chattering when it
;;; senses the other agent. It says hello, waits for the other agent
;;; to say hello, and then says goodbye. When an agent hears the other
;;; agent say goodbye, it knows its time to stop chattering.
;;; 

(defapplication "Synchro"
    :init (init-synchro))

(defun init-synchro ()
  (let* ((k (make-instance 'locale :name "Kitchen")))
    (let ((a (make-instance 'agent :name "Able"
			    :locale k
			    :initial-task '(chatter)))
	  (b (make-instance 'agent :name "Baker"
			    :locale k
			    :initial-task '(chatter))))
      (show available-to-talk)
      (show hello)
      (show goodbye)
      (values a b k))))

;;; the constraint system requires functions of two places, so
;;; we define a 'not equal' predicate. By the way, if these were
;;; real numbers, we could use /=.

(defun neq (a b)
  (not (eql a b)))

;;; chattering means talking to one another. 

(procedure (chatter)
  
  ;; start talking to another agent when you sense the other agent
  
  (step (talk to ?other)
	(waitfor (:measurement (available-to-talk ?other = true)
			       :object (neq +self+))))

  ;; publish a available-to-talk message for the other agent to sense
  
  (step (publish (available-to-talk +self+ = true)))
  
  ;; quit when done talking, and you've heard the other agent
  ;; say goodbye.
  
  (step (terminate)
	(waitfor ?talk (goodbye from ?other))))

;;; Say hello, wait for hello from other, then say goodbye

(procedure (talk to ?other)
  (step hi (speak (hello from +self+)))
  (step gb (speak (goodbye from +self+))
	(waitfor (hello from ?other)))
  (step (terminate)
	(waitfor ?gb)))

;;; We use a 'speak' primitive just to allow INFORM to take some
;;; time -- here, the length of the message in seconds.

(primitive (speak ?message)
  (duration (list (length ?message) 'seconds))
  (on-completion (inform ?message)))

