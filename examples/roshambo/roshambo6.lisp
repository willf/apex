;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/roshambo/roshambo6.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: roshambo6.lisp,v 1.3 2006/01/15 03:42:55 dalal Exp $

(in-package :cl-user)

(defapplication "Roshambo 6"
    :init (play-roshambo))


(defun play-roshambo ()
  (let ((gameroom (make-instance 'locale :name 'gameroom)))
    (let ((jill (make-instance 'agent
		  :name "Jill"
		  :locale gameroom
		  :use-bundles '(:player :rock-strategy)
		  :initial-task '(play roshambo)))
	  (jack (make-instance 'agent
		  :name "Jack"
		  :locale gameroom
		  :use-bundles '(:player :paper-strategy)		  
		  :initial-task '(play roshambo)))
	  (ref  (make-instance 'agent
		  :name "Referee"
		  :locale gameroom
		  :use-bundles '(:referee)
		  :initial-task '(conduct roshambo game))))
      (list jack jill ref))))

(primitive (gesture ?gesture)
  (profile hand)
  (duration (500 ms))
  (on-completion (inform `(gestured ,+self+ ,?gesture))))


(in-apex-bundle :player)

(procedure :sequential
  (play roshambo)
  (step (sign in)
	(waitfor (gestured ?ref hello)))
  (step (play roshambo once)
	(waitfor (gestured <?ref> go))
	)
  (step (sign out)))

(procedure :seq (sign in)
  (profile hand)
  (step (gesture raised-hand)))

(procedure :seq (sign out)
  (profile hand)
  (step (gesture lowered-hand)))




(procedure :seq (play roshambo once)
  (step (choose gesture => ?gesture))
  (step (gesture ?gesture)))


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


(in-apex-bundle :referee)

(procedure :sequential
  (index (recruit roshambo players))
  (step (terminate >> (<?player1> <?player2>))
	(waitfor (:and
		  (gestured ?player1 raised-hand)
		  (gestured ?player2 raised-hand)
		  :constraints (not (eql ?player1 ?player2))
		  ))))

(procedure :sequential
 (index (watch game between ?player1 ?player2))
 (step (terminate >> (<?play1> <?play2>))
       (waitfor (:and
		 (gestured <?player1> ?play1)
		 (gestured <?player2> ?play2)))))

(primitive 
    (index (determine winner ?person1 ?gesture1 ?person2 ?gesture2))
  (return 
    (cond
     ((eql <?gesture1> <?gesture2>) '|a tie|)
     ((or (and (eq <?gesture1> 'rock)
	       (eq <?gesture2> 'scissors))
	  (and (eq <?gesture1> 'scissors)
	       (eq <?gesture2> 'paper))
	  (and (eq <?gesture1> 'paper)
	       (eq <?gesture2> 'rock))) 
      <?person1>)
     (t <?person2>))))

(procedure :seq (conduct roshambo game)
  (step (gesture hello))
  (step (recruit roshambo players => (?player1 ?player2)))
  (step (gesture go))
  (step (watch game between ?player1 ?player2 => (?play1 ?play2)))
  (step (determine winner <?player1> <?play1> <?player2> <?play2> => ?winner))
  (step (gesture ?winner)))