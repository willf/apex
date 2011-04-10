;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/hand-custom.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: hand-custom.lisp,v 1.6 2006/01/15 03:42:56 dalal Exp $

(in-package :user)

;;; Except for turn-dial (adapted from Kitchen World), these procedures
;;; are all adapted from hand.lisp in the Human library distributed with
;;; Apex.  All procedures here differ from their originals only in
;;; adding the Attention resource to their profile.

(procedure
 ;; t means ?obj not already grasped
 (index (grasp ?obj with ?hand if-needed t))  
 (profile (?hand 8 10) (attention))
 (step s1 (grasp ?obj with ?hand taking (1 sec)))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))

;;; --- Strike object

(procedure
 (index (strike ?obj1 against ?obj2 using ?hand))
 (profile (?hand 8 10) (attention))
 (step s1 (strike ?obj1 against ?obj2 with ?hand taking (2 secs)))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))


;;; --- Release object

(procedure
 (index (release ?obj from ?hand))
 (profile (?hand 8 10) (attention))
 (step s1 (release ?obj from ?hand taking (800 ms)))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))

;;; --- Turn dial

(procedure
 (index (turn-dial ?dial ?setting using ?hand))
 (profile (?hand 10 10) (attention))
 (step s1 (turn ?dial to ?setting with ?hand taking (1 sec)))
 (step s2 (terminate) (waitfor ?s1)))

(primitive
 (index (turn ?object to ?setting with ?hand taking ?duration))
 (duration ?duration)
 (on-completion
  (signal-event (turned ?object ?setting))))


;;; --- Move object

;;; Puts an object at the location of another object.  If the hand
;;; being used to move the object is already holding something, the
;;; held object will be dropped.  If the object is already at the
;;; target location, this procedure will terminate without taking
;;; action.

(procedure
 (index (move ?object1 ?preposition ?object2 using ?hand))
 (profile (?hand 8 10) (attention))
 (step s1 (grasp ?object1 with ?hand))
 (step s2 (move ?object1 ?preposition ?object2 with ?hand taking 
                (1200 ms))
       (waitfor ?s1))
 (step s3 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s5 (terminate) (waitfor ?s2)))

