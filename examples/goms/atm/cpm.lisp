;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/goms/atm/cpm.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: cpm.lisp,v 1.4 2006/01/15 03:42:53 dalal Exp $

;;; Example application: Withdrawal of money from at ATM using the CPM
;;; approach.

;;; CPM-GOMS analyzes human-computer interaction in terms of very
;;; low-level (Cognitive, Perceptual and Motor) behaviors.  It thus
;;; provides a more finely-detailed analysis of behavior than other GOMS
;;; methods.  By accounting for limited concurrency and interleaving of
;;; low-level behaviors, this method accounts for overlapping execution
;;; of behavior at the higher level of interface-level action.  CPM-GOMS
;;; tends to produce very accurate predictions of task duration for
;;; highly practiced tasks.

(in-package :user)

(defapplication "ATM-CPM World"
    :libraries ("atm")
    :files ("common.lisp")
    :init (initialize-atm-common))

;;; Top level procedure and goal.

(procedure
 (index (get ?amt from atm))
 (step s1 (initiate ATM session) (rank 1))
 (step s2 (withdraw ?amt from atm) (rank 2))
 (step s3 (end ATM session) (rank 3))
 (step s4 (end-trial) (waitfor ?s1 ?s2 ?s3)))

(procedure :ranked
 (index (initiate ATM session))
 (insert card)
 (enter password))

(procedure :ranked
 (index (withdraw ?amt from ATM))
 (press withdraw-key)
 (press checking-account-key)
 (enter ?amt using keypad)
 (retrieve money))

(procedure :ranked
 (index (end ATM session))
 (press NO-key)
 (retrieve card)
 (retrieve receipt))

(procedure
 (index (insert card))
 (step s1 (slow-move-click card-slot))
 (step s2 (terminate) (waitfor ?s1)))

(procedure :ranked
 (index (enter password))
 (remember PIN)
 (press key-4 on keypad)
 (press key-9 on keypad)
 (press key-0 on keypad)
 (press key-1  on keypad)
 (press OK-key))

(procedure
 (index (press withdraw-key))
 (step s1 (fast-move-click withdraw-key))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 (index (press checking-account-key))
 (step s1 (fast-move-click checking-key))
 (step s2 (terminate) (waitfor ?s1)))

(procedure :ranked
 (index (enter ?amt using keypad))    
 ;; todo: synthesize this sequence at runtime
 (press key-8 on keypad)
 (press key-0 on keypad)
 (press CORRECT-key))

(procedure 
 (index (retrieve money))
 (step s1 (slow-move-click money-slot))
 (step s2 (terminate) (waitfor ?s1)))

(procedure 
 (index (retrieve card))
 (step s1 (slow-move-click card-slot))
 (step s2 (terminate) (waitfor ?s1)))

(procedure 
 (index (retrieve receipt))
 (step s1 (slow-move-click money-slot))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 (index (press ?key on keypad))
 (step s1 (fast-move-click ?key))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 (index (press OK-key))
 (step s1 (slow-move-click OK-key))
 (step s2 (terminate) (waitfor ?s1)))

(procedure 
 (index (press NO-key))
 (step s1 (slow-move-click NO-key))
 (step s2 (terminate) (waitfor ?s1)))

(procedure 
 (index (press CORRECT-key))
 (step s1 (slow-move-click CORRECT-key))
 (step s2 (terminate) (waitfor ?s1)))


;;; Slow Move Click

(procedure
 (index (slow-move-click ?target))
 (step c1 (initiate-move-cursor ?target))
 (step m1 (move-cursor ?target) 
                    (waitfor ?c1))
 (step c2 (attend-target ?target))
 (step c3 (initiate-eye-movement ?target)  
                    (waitfor ?c2))
 (step m2 (eye-movement ?target)  
                    (waitfor ?c3))
 (step p1 (perceive-target-complex ?target)  
                    (waitfor ?m2))
 (step c4 (verify-target-position ?target)  
                    (waitfor ?c3 ?p1))
 (step c5 (attend-cursor-at-target ?target)  
                    (waitfor ?c4))
 (step w1 (WORLD new-cursor-location ?target)  
                    (waitfor ?m1))
 (step p2 (perceive-cursor-at-target ?target)  
                    (waitfor ?p1 ?c5 ?w1))
 (step c6 (verify-cursor-at-target ?target)  
                    (waitfor ?c5 ?p2))
 (step c7 (initiate-click ?target)  
                    (waitfor ?c6 ?m1))
 (step m3 (mouse-down ?target)  
                    (waitfor ?m1 ?c7))
 (step m4 (mouse-up ?target)  
                    (waitfor ?m3))
 (step t1 (terminate)  
                    (waitfor ?m4))
)

;;; Fast Move Click

(procedure
 (index (fast-move-click ?target))
 (step c1 (initiate-move-cursor ?target))
 (step m1 (move-cursor ?target) 
                    (waitfor ?c1))
 (step c2 (attend-target ?target))
 (step c3 (initiate-eye-movement ?target)  
                    (waitfor ?c2))
 (step m2 (eye-movement ?target)  
                    (waitfor ?c3))
 (step p1 (perceive-target-complex-R ?target)  
                    (waitfor ?m2))
 (step c4 (verify-target-position ?target)  
                    (waitfor ?c3 ?p1))
 (step w1 (WORLD new-cursor-location ?target)  
                    (waitfor ?m1))
 (step c7 (initiate-click ?target)  
                    (waitfor ?c4 ?m1))
 (step m3 (mouse-down ?target)  
                    (waitfor ?m1 ?c7))
 (step m4 (mouse-up ?target)  
                    (waitfor ?m3))
 (step t1 (terminate)  
                    (waitfor ?m4))
)

 
;;; Lowest level primitives.

;;; World stuff

;;; This needs to be changed to an event in the simworld that results in
;;; a cogevent that the perceive-target will waitfor

(procedure
 (index (WORLD new-cursor-location ?target))
 (profile gaze)
 (step s1 (use resource gaze for (0 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s2 (terminate)
       (waitfor ?s1)))

;;; Visual perception

;;; The following two procedures visually perceive a target on screen.
;;; The complex one is for something like a six letter word or well
;;; learned code (290 ms for the perception).  The binary one is for
;;; perceiving only whether a signal is present or not, e.g., Wayne's
;;; World (100 ms for the perception).

;;; The following perceive-target-complex DOES NOT release the
;;; vision-block resource.  It is sufficient for the SLOW -M/C template
;;; from Milliseconds Matter, but when we get to implementing the
;;; FAST-M/C we are going to need a preceive-target-complex that does
;;; release the vision-block.

(procedure :sequential
 (index (perceive-target-complex ?target))
 (use resource vision for (290 ms)))

(procedure :sequential
 (index (perceive-target-complex-R ?target))
 (use resource vision for (290 ms))
 (release-resource vision-block :ancestor 2))


(procedure :sequential
 (index (perceive-target-binary ?target))
 (use resource vision for (100 ms)))

;;; This is a special case of perceive-target-binary that is used so
;;; often that it is given its own name.  used in Wayne's World to
;;; detect the coincidence of the target and cursor.  It releases the
;;; vision-block resource because it is the last perceive in the
;;; SLOW-M/C template and it isn't used anywhere where that wouldn't be
;;; appropriate.

(procedure :sequential
 (index (perceive-cursor-at-target ?target))
 (use resource vision for (100 ms))
 (release-resource vision-block :ancestor 2))


;;; Cognition stuff

;;; This procedure sets up a hold on the virtual resource
;;; right-hand-block; perhaps a parallel procedure that does not set up
;;; a hold on this resource should be created for templates that have an
;;; initiate move cursor in the middle.

(procedure
 (index (initiate-move-cursor ?target))
 (profile memory right-hand-block)
 (step s1 (use resource memory for (50 ms)))
 (step s2 (hold-resource right-hand-block :ancestor 2) 
          (waitfor ?s1))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s2)))

(procedure
 (index (attend-target ?target))
 (profile memory vision-block)
 (step s1 (use resource memory for (50 ms)))
 (step s2 (hold-resource vision-block :ancestor 2) 
          (waitfor ?s1))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s2)))

(procedure
 (index (initiate-eye-movement ?target))
 (profile memory)
 (step s1 (use resource memory for (50 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))

(procedure
 (index (verify-target-position ?target))
 (profile memory)
 (step s1 (use resource memory for (50 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 (index (attend-cursor-at-target ?target))
 (profile memory vision-block)
 (step s1 (use resource memory for (50 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))

(procedure
 (index (verify-cursor-at-target ?target))
 (profile memory)
 (step s1 (use resource memory for (50 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 (index (initiate-click ?target))
 (profile memory right-hand-block)
 (step s1 (use resource memory for (50 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 (index (remember ?something))
 (profile memory)
 (step s1 (use resource memory for (50 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s2 (terminate) (waitfor ?s1)))


;;; This uses the gaze resource which is some combination of CPM-GOMS'
;;; visual perception and eye-movement resources.  we will think about
;;; the implication of this after the tutorial

(procedure
 (index (eye-movement ?target))
 (profile gaze)
 (step s1 (use resource gaze for (50 ms)))
 (step r (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s2 (terminate) (waitfor ?s1)))


;;; Right hand stuff

(procedure :sequential
 (index (move-cursor ?target))
 (mouse-time ?target => ?time)
 (move-mouse to ?target with right-hand taking ?time))

(procedure :sequential
 (index (mouse-down ?target))
 (mouse-object => ?object)
 (mouse-down on ?object with right-hand taking (100 ms)))


(procedure :sequential
 (index (mouse-up ?target))
 (mouse-object => ?object)
 (mouse-up on ?object with right-hand taking (100 ms))
 (release-resource right-hand-block :ancestor 2))
