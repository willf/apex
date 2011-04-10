;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/roshambo/roshambo7.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: roshambo7.lisp,v 1.3 2006/01/15 03:42:55 dalal Exp $

(in-package :cl-user)

(defapplication "Roshambo 7"
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
      :initial-task '(play roshambo 3 times with jack))
    (make-instance 'agent
      :name 'jack
      :locale locale
      :use-bundles '(:rock-strategy)
      :routers (list auditory-router visual-router)
      :initial-task '(play roshambo 3 times with jill))))

(defparameter *games* (list))
(defun games (player) (or (getf *games* player) 0))
(defun (setf games) (val player)
  (setf (getf *games* player) val))

(procedure :seq
  (index (play roshambo ?n times with ?opp-name))
  (step s2 (play roshambo with ?opp-name)
	(repeating :until (>= (games +self+) ?n))))

(procedure (index (play roshambo with ?opp-name))
  (step (find agent ?opp-name => ?opponent))
  (step (prime) (waitfor <?find>))
  (step (choose gesture => ?mine))
  (step (gesture <?mine>) (waitfor <?prime> <?choose>))
  (step (determine winner +self+ <?mine> <?opponent> <?other> => 
          (?winner ?winning))
          (waitfor ?gesture 
             (:in-order
                    (gestured ?opponent = priming)
                    (gestured ?opponent = ?other))))
  (step (record game <?mine> <?opponent> <?other> <?winner> <?winning>)
	(waitfor <?determine>))
  (step (terminate) (waitfor <?record>)))


(procedure :seq (index (prime))
   (step (gesture priming)))
		       

(primitive (index (gesture ?gesture))
  (profile hand)
  (duration (500 ms))
  (on-start
   (inform `(gestured ,+self+ = ,?gesture)
	   :router (router-named 'visual-router))))
				    

(primitive (index (say . ?something))
  (profile voice)
  (duration (list (* (length ?something) 400) 'ms))
  (on-start
   (inform `(said ,@?something)
	   :router (router-named 'auditory-router))))

(primitive (find agent ?name)
  (duration (0 ms))
  (return (find-agent ?name)))

(primitive (increase game count) 
  (duration 0)
  (return (incf (games +self+))))

(procedure 
    (index (record game ?mine ?opponent ?other ?winner ?winning))
  (step (increase game count))
  (step (say winner is <?winner>))
  (step (terminate)
	(waitfor ?increase ?say)))

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

(in-apex-bundle :scissors-strategy)

(primitive (choose gesture)
  (profile brain)
  (duration (500 ms))
  (return 'scissors))




