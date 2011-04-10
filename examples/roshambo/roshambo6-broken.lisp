;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/roshambo/roshambo6-broken.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: roshambo6-broken.lisp,v 1.3 2006/01/15 03:42:55 dalal Exp $

(in-package :cl-user)

(defapplication "Roshambo 6 -- broken"
    :init (initialize-sim))

(defun initialize-sim ()
  (let ((locale (make-instance 'locale :name 'gameroom))
	(auditory-router (make-instance 'ps-router :name 'auditory-router))
	(visual-router (make-instance 'ps-router :name 'visual-router)))
    (make-instance 'agent
      :name 'jill
      :locale locale
      :use-bundles '(:paper-strategy)
      :routers (list auditory-router visual-router)
      :initial-task '(play roshambo with jack))
    (make-instance 'agent
      :name 'jack
      :locale locale
      :use-bundles '(:rock-strategy)
      :routers (list auditory-router visual-router)      
      :initial-task '(play roshambo with jill))))

(procedure 
  (index  (play roshambo with ?opp-name))
  (step (find agent ?opp-name => ?opponent))
  (step (prime)
	(waitfor <?find>))
  (step (choose gesture => ?mine))
  (step (gesture <?mine>)
	(waitfor <?prime> <?choose>))
  (step (determine winner +self+ <?mine> <?opponent> <?other> => (?winner ?winning))
	(waitfor ?gesture (gestured <?opponent> ?other)))
  (step (say winner is <?winner>)
	(waitfor <?determine>))
  (step (terminate)
	(waitfor <?say>)))

(procedure :seq (index (prime))
   (step (gesture priming)))
		       

(primitive (index (gesture ?gesture))
  (profile hand)
  (duration (500 ms))
  (on-completion 
   (inform `(gestured ,+self+ ,?gesture)
	   :router (router-named 'visual-router))))
				    

(primitive (index (say . ?something))
  (profile voice)
  (duration (list (* (length ?something) 400) 'ms))
  (on-completion 
   (inform `(said ,@?something)
	   :router (router-named 'auditory-router))))

(primitive (find agent ?name)
  (duration (0 ms))
  (return (find-agent ?name)))

(primitive 
    (index (determine winner ?person1 ?gesture1 ?person2 ?gesture2))
  (profile brain)
  (duration (300 ms))
  (return 
    (determine-roshambo-winner ?person1 ?gesture1 ?person2 ?gesture2)))

(defun determine-roshambo-winner (person1 gesture1 person2 gesture2)
  (cond
   ((eq gesture1 gesture2)
    (list '|a tie| gesture1))
   ((or (and (eq gesture1 'rock)
	     (eq gesture2 'scissors))
	(and (eq gesture1 'scissors)
	     (eq gesture2 'paper))
	(and (eq gesture1 'paper)
	     (eq gesture2 'rock)))
    (list person1 gesture1))
   (t (list person2 gesture2))))


(in-apex-bundle :rock-strategy)

(primitive (choose gesture)
  (profile brain)
  (duration (500 ms))
  (return 'rock))


(in-apex-bundle :paper-strategy)

(primitive (choose gesture)
  (profile brain)
  (duration (500 ms))
  (return 'paper))