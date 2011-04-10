;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/kitchen-pdl.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: kitchen-pdl.lisp,v 1.8 2006/01/15 03:42:57 dalal Exp $

(in-package :user)

;;; PDL for the kitchen

;;; --- Do Domain -------------------------------------------------------


(procedure
 (index (do-household-tasks))
 (step s1 (kitchen-tasks))
 (step s2 (living-room-tasks))
 (step s3 (front-porch-tasks))
 (step st (end-trial) (waitfor ?s1 ?s2 ?s3)))

;;; ---- Kitchen tasks -----------------------------------------------

;;; MD: sPick and sSoup in this procedure seem to not
;;; terminate, when either uncommented in the given context, or run as
;;; the sole steps of the procedure.  Details precede each.  To be
;;; further investigated. 

;;; DF 9/1/2005 If the sWatchTV or  sPlant task is included
;;; the living room tasks check-window and check-lamp along with the
;;; front porch task get some fresh air along remain in an enabled
;;; state and are never allocated the necessary resources specified
;;; in their profile clauses.
;;;
;;; If sPhone is included there is a problem interrupting the answer
;;; phone with hand task. An error is generated at around 400 ms.

(procedure 
 (index (kitchen-tasks))
 (step s2 (make-fried-egg) (priority 2))
 (step sMacaroni (make-macaroni) (priority 2))
 (step sTrash (watch-for-full-trash) (repeating) (priority 3))
 (step sDish (wash-dishes) (priority 2))
 (step sPopcorn (make-popcorn) (priority 2))
 (step sFill (fill-cups) (priority 2))
 ;(step sPhone (phone) (priority 3) (repeating))
 ;(step sWatchTV (watch-for-watch-tv) (priority 2) (repeating))
 ;(step sPlant (keep-eye-on-plant) (priority 0) (repeating))
 ;;; ! sSoup gets stuck in checking the drawers 
 ;(step sSoup (make-soup) (priority 2))
 ;;; This also gets stuck in checking the drawers, with the substep
 ;;; 'in-same-locale?' (a non-durative primitive) never seeming to
 ;;; complete.
 ;;; (step sPick (pick-flower) (priority 0))
 (step s3 (terminate) 
       (waitfor ?s2 ?sMacaroni ?sPopcorn ?sFill))) ;?sSoup ?sPick

;;------ *globalAlarmTime*---------------------------------------------

;;*globalAlarmTime* is a remnant from the class project
;;for timing the egg.  It can be handled better, and I 
;;will probaly change it when I move to that phase

(defvar *globalAlarmTime*)
(setf *globalAlarmTime* -1)

;;; ----- Make Fried Egg --------------------------------------------

(procedure 
 (index (make-fried-egg))
 (profile presence attention)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 5))
 (step s0 (grasp ?pan with right-hand)
       (waitfor (shape ?pan pan) ?move-to))
 (step s1 (assemble ingredients fried-egg) (waitfor ?s0))
 (step s2 (turn-dial front-left-dial 4 using right-hand) 
       (waitfor ?s1 (shape front-left-dial dial)))
 (step s3 (move ?pan onto front-left-burner using right-hand) 
       (waitfor ?s2 (shape ?pan pan)))
 (step s4 (release ?pan from right-hand) 
       (waitfor ?s3))
 (step s5 (crack-open ?eggshell over ?pan with left-hand) 
       (waitfor ?s4 (shape ?eggshell eggshell)))
 (step stime (beginTiming) 
       (waitfor ?s5))
 (step sTrash (trash ?eggshell in ?can) 
       (waitfor (shape ?can trashcan) ?s5))
 (step sCheck (check-on-egg ?em) 
       (waitfor (alarm-rings) (shape ?em eggmass)))
 (step setime (endTiming ?texture)
       (waitfor (egg-ready ?texture)))
 (step s6 (turn-dial front-left-dial 0 using right-hand) 
       (waitfor ?setime))
 (step sStopCheck (terminate ?sCheck) 
       (waitfor (egg-ready ?anything)))
 (step s7 (move ?pan onto back-right-burner using right-hand)
       (waitfor ?s6))
 (step s8 (release ?pan from right-hand) 
       (waitfor ?s7))
 (step s9 (terminate)
       (waitfor ?s8)))

;;; --- Assemble ingredients---------------------------------------------------

;;; Procedures should normally contain appropriate preparatory actions 
;;; -- e.g. assembling (acquiring and putting in easy reach) ingredients 
;;; for a cooking task.  Currently, the kitchenworld assembles the needed
;;; ingredients to fry an egg at the start of the simulation trial. Thus,
;;; the following is a null procedure. 

(procedure
 (index (assemble ingredients ?food-item))
 (step s1 (terminate)))

;;; --- Crack open eggshell ----------------------------------------------

(procedure
 (index (crack-open ?shell over ?object with ?hand))
 (profile (?hand 8 10) (attention)  )
 (step s1 (move ?shell over ?object using ?hand))
 (step s2 (strike ?shell against ?object using ?hand) 
       (waitfor ?s1))
 (step s3 (pull-apart ?shell with ?hand)  
       (waitfor ?s2 (texture ?shell cracked)))
 (step s4 (move ?shell onto counter using ?hand) 
       (waitfor ?s3 (shape ?em eggmass)))
 (step s5 (release ?shell from ?hand) 
       (waitfor ?s4))
 (step s6 (terminate) 
       (waitfor ?s5)))

(primitive
 (index (check-on-egg ?eggmass))
 (profile attention)
 (update (100 ms)
	 (let ((tex-em (texture ?eggmass)))
	   (unless (or (equal tex-em 'runny) (equal tex-em 'liquid))
	     (progn (inform `(egg-ready ,tex-em))
                    (complete-the-activity (action +this-task+)))))))
                                        ;(complete))))))

;;; --- Pull apart -------------------------------------------------------

;;; The person needs both hands to pull something apart

(procedure
 (index (pull-apart ?object with ?hand))
 (profile (?hand 8 10) (attention))
 (step getDuration (getNormRandinMS 1500 200 => ?d))
 (step s1 (pull-apart ?object with ?hand taking ?d)
       (waitfor ?getDuration))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

;;; --- Striking ----------------------------------------------------

(procedure
 (index (strike ?obj1 against ?obj2 using ?hand))
 (profile (?hand 8 10) (attention))
 (step getDuration (getNormRandinMS 200 20 => ?d))
 (step s1 (strike ?obj1 against ?obj2 taking ?d)
       (waitfor ?getDuration))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

;;; Profile-less version of one in hand library
(primitive
 (index (strike ?obj1 against ?obj2 taking ?duration))
 (duration ?duration)
 (on-completion
  (signal-event (struck ?obj1 ?obj2))))

;;; --- Make Macaroni --------------------------------------------------

(procedure 
 (index (make-macaroni))
 (profile micro presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (put ?macaroni in ?microwave) 
       (waitfor (shape ?macaroni macaroni) (shape ?microwave microwave)
		?move-to))
 (step strash (trash ?mWrap in ?can) 
       (waitfor (shape ?can trashcan) 
                (shape ?mWrap macaroni-wrapper) ?s1))
 (step s2 (settings for ?macaroni on ?microwave)  
       (waitfor ?s1))
 (step s4 (remove ?macaroni from ?microwave) 
       (waitfor (microwave-beep)))
 (step s5 (terminate) 
       (waitfor ?s4 ?strash)))

(procedure
 (index (put ?food in ?microwave))
 (profile attention  )
 (step s1 (grab ?food with right-hand))
 (step s2 (grab ?microwave with left-hand))
 (step s3 (insert ?food in ?microwave with right-hand) 
       (waitfor ?s1 ?s2))
 (step s4 (let-go ?food from right-hand) 
       (waitfor ?s3))
 (step s5 (let-go ?microwave from left-hand) 
       (waitfor ?s4))
 (step s7 (terminate) 
       (waitfor ?s5)))

(primitive
 (index (grab ?object with ?hand))
 (profile ?hand)
 (duration (100 ms)))

(primitive 
 (index (let-go ?object from ?hand))
 (profile ?hand)
 (duration (500 ms)))

(procedure
 (index (settings for ?food on ?microwave))
 (profile attention  )
 (step s2 (fix ?microwave with right-hand) 
       (waitfor (microwave-unresponsive)))
 (step s3 (terminate ?s1) 
       (waitfor (microwave-unresponsive)))
 (step s1 (dial-in ?microwave ?food))
 (step sr (reset +this-task+) 
       (waitfor ?s2))
 (step st (terminate) 
       (waitfor (set-time ?mw 100) (power-setting ?mw 8))))

(procedure
 (index (dial-in ?microwave ?food))
 (profile attention  )
 (step sP (select-power ?microwave for ?food with left-hand))
 (step s1 (set-time ?microwave ?food with left-hand) 
       (waitfor ?sP))
 (step s3 (terminate) 
       (waitfor ?s1)))


(procedure
 (index (select-power ?microwave for ?food with ?hand))
 (profile ?hand)
 (step pre (getNormRandinMS 500 20 => ?d))
 (step s1 (set power of ?microwave to 8 with ?hand taking ?d)
       (waitfor ?pre))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

(primitive
 (index (set power of ?microwave to ?power with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (change-power-setting ?microwave ?power))))

(procedure
 (index (set-time ?microwave ?food with ?hand))
 (profile ?hand)
 ;;; eventually, want :time to be determined by a call to a learning 
 ;;; function?
 (step pre (getNormRandinMS 500 20 => ?d))
 (step s1 (set microwave time of ?microwave to 100                        
               with ?hand taking ?d)
       (waitfor ?pre))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

(primitive
 (index (set microwave time of ?microwave to ?time with ?hand 
             taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (set-time ?microwave ?time))))

(procedure
 (index (remove ?food from ?microwave))
 (profile attention)
 (step s1 (grab ?microwave with left-hand))
 (step s2 (grab ?food with right-hand) 
       (waitfor ?s1))
 (step s3 (take-out ?microwave ?food with right-hand) 
       (waitfor ?s2))
 (step s4 (let-go ?food from right-hand) 
       (waitfor ?s3))
 (step s5 (let-go ?microwave from left-hand) 
       (waitfor ?s4))
 (step s7 (terminate) 
       (waitfor ?s5)))

(procedure
 (index (take-out ?microwave ?food with ?hand))
 (profile ?hand)
 (step pre (getNormRandinMS 500 20 => ?d))
 (step s1 (take ?food out of ?microwave with ?hand taking ?d)
       (waitfor ?pre))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

(primitive
 (index (take ?food out of ?microwave with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (take-out ?microwave ?food))))

(procedure
 (index (insert ?food in ?microwave with ?hand))
 (profile ?hand)
 (step pre (getNormRandinMS 500 20 => ?d))
 (step s1 (insert ?food in ?microwave with ?hand taking ?d)
       (waitfor ?pre))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

(primitive
 (index (insert ?food in ?microwave with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (put-in ?microwave ?food))))

;;;eventually want the select clause to be based on learning.... maybe
(procedure
 (index (fix ?microwave with ?hand))
 (profile attention )
 (step s1 (?action ?microwave with ?hand)
       (select ?action (if (/= (random 3) 0) 
                           (if (= (random 2) 0) 'hit 'shake) 'plug-in)))
 (step st (terminate) 
       (waitfor ?s1)))

(procedure
 (index (hit ?microwave with ?hand))
 (profile ?hand)
 (step pre (getNormRandinMS 500 20 => ?d))
 (step s1 (hit ?microwave with ?hand taking ?d)
       (waitfor ?pre))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

(primitive
 (index (hit ?microwave with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (hit-microwave ?microwave))))

;; shaking doesn't do anything, here just to give learning something to 
;; learn to rule out
(primitive
 (index (shake ?microwave with ?hand))
 (profile ?hand)
 (duration (500 ms))) 


(procedure
 (index (plug-in ?microwave with ?hand))
 (profile ?hand)
 (step pre (getNormRandinMS 500 20 => ?d))
 (step s1 (plug in ?microwave with ?hand taking ?d)
       (waitfor ?pre))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?s1)))

(primitive
 (index (plug in ?microwave with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (plug-in ?microwave))))

(primitive
 (index (trash ?item in ?can))
 (profile attention)
 (duration (100 ms))
 (on-start
  (if (and  (equal '(has-bag) (state ?can))
	    (not (full (bag ?can))))
      (progn (setf (contents (bag ?can))
               (append (contents (bag ?can))
                       (list ?item)))
	     (when (>= (length (contents (bag ?can))) 2)
               (inform `(full ,?can ,(bag ?can)) :router *walter-router*)
	       (setf (full (bag ?can)) t)))
    (inform `(cannot-throw-away ,?item) :router *walter-router*))))

(procedure 
 (index (watch-for-full-trash))
 (step s1 (empty-trash ?can ?oldbag) 
       (waitfor (full ?can ?oldbag)))
 (step st (terminate) 
       (waitfor ?s1)))

(procedure
 (index (empty-trash ?can ?oldbag))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (take trash ?oldbag from ?can) 
       (waitfor ?move-to))
 (step s2 (line ?can with ?bag) 
       (waitfor ?s1 (shape ?bag in-box)))
 (step st (terminate) 
       (waitfor ?s2)))

(primitive
 (index (take trash ?bag from ?can))
 (profile attention)                            
 (duration (200 ms))
 (on-start
  (setf (shape ?bag) '(thrown-out))
  (setf (bag ?can) nil)
  (setf (state ?can) '(no-bag))))

(primitive
 (index (line ?can with ?bag))
 (profile attention)
 (duration (100 ms))
 (on-start
  (setf (shape ?bag) '(in-can))
  (setf (bag ?can) ?bag)
  (setf (state ?can) '(has-bag))))

;;; --- Wash  dishes ---------------------------------------------------

(procedure
 (index (wash-dishes))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (wash ?dish1 with left-hand) 
       (waitfor (shape ?dish1 dish-1) ?move-to))
 (step s2 (wash ?dish2 with left-hand) 
       (waitfor (shape ?dish2 dish-2)))
 (step s3 (dry ?dish1 with right-hand) 
       (waitfor ?s1))
 (step s4 (dry ?dish2 with right-hand) 
       (waitfor ?s2))
 (step st (terminate)  
       (waitfor ?s3 ?s4)))

(primitive
 (index (wash ?dish with ?hand))
 (profile ?hand attention )
 (duration (100 ms))
 (on-start
  (setf (state ?dish) '(clean wet))))

(primitive
 (index (dry ?dish with ?hand))
 (profile ?hand attention )
 (duration (100 ms))
 (on-start
  (setf (state ?dish) '(clean dry))))

;;; --- Make Popcorn ----------------------------------------------------

(procedure
 (index (make-popcorn))
 (profile micro presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6)) 
 (step s1 (put ?pop in ?microwave) 
       (waitfor (shape ?pop popcorn) (shape ?microwave microwave)
		?move-to))
 (step strash (trash ?pWrap in ?can) 
       (waitfor (shape ?can trashcan) 
                (shape ?pWrap popcorn-wrapper) ?s1))
 (step s2 (settings for ?pop on ?microwave) 
       (waitfor ?s1))
 (step s3 (remove ?pop from ?microwave) 
       (waitfor (microwave-beep)))
 (step s4 (terminate) 
       (waitfor ?s3 ?strash)))

(primitive
 (index (trash ?item in ?can))
 (profile attention)
 (duration (100 ms))
 (on-start
  (if (and  (equal '(has-bag) (state ?can))
	    (not (full (bag ?can))))
      (progn (setf (contents (bag ?can))
               (append (contents (bag ?can))
                       (list ?item)))
	     (when (>= (length (contents (bag ?can))) 2)
               (inform `(full ,?can ,(bag ?can)) :router *walter-router*)
	       (setf (full (bag ?can)) t)))
    (inform `(cannot-throw-away ,?item) :router *walter-router*))))

(procedure 
 (index (watch-for-full-trash))
 (step s1 (empty-trash ?can ?oldbag) 
       (waitfor (full ?can ?oldbag)))
 (step st (terminate) 
       (waitfor ?s1)))

(procedure
 (index (empty-trash ?can ?oldbag))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (take trash ?oldbag from ?can) 
       (waitfor ?move-to))
 (step s2 (line ?can with ?bag) 
       (waitfor ?s1 (shape ?bag in-box)))
 (step st (terminate) 
       (waitfor ?s2)))

(primitive
 (index (take trash ?bag from ?can))
 (profile attention)                            
 (duration (200 ms))
 (on-start
  (setf (shape ?bag) '(thrown-out))
  (setf (bag ?can) nil)
  (setf (state ?can) '(no-bag))))

(primitive
 (index (line ?can with ?bag))
 (profile attention)
 (duration (100 ms))
 (on-start
  (setf (shape ?bag) '(in-can))
  (setf (bag ?can) ?bag)
  (setf (state ?can) '(has-bag))))

;;; --- Wash  dishes ---------------------------------------------------

(procedure
 (index (wash-dishes))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (wash ?dish1 with left-hand) 
       (waitfor (shape ?dish1 dish-1) ?move-to))
 (step s2 (wash ?dish2 with left-hand) 
       (waitfor (shape ?dish2 dish-2)))
 (step s3 (dry ?dish1 with right-hand) 
       (waitfor ?s1))
 (step s4 (dry ?dish2 with right-hand) 
       (waitfor ?s2))
 (step st (terminate)  
       (waitfor ?s3 ?s4)))

(primitive
 (index (wash ?dish with ?hand))
 (profile ?hand attention )
 (duration (100 ms))
 (on-start
  (setf (state ?dish) '(clean wet))))

(primitive
 (index (dry ?dish with ?hand))
 (profile ?hand attention )
 (duration (100 ms))
 (on-start
  (setf (state ?dish) '(clean dry))))

;;; --- Fill Measuring Cup ---------------------------------------------

(procedure
 (index (fill-cups))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step sFill (fill ?measuring-cup) 
       (waitfor (shape ?measuring-cup empty-cup) ?move-to))
 (step sFill2 (fill ?h2) (waitfor (shape ?h2 empty-cup-2)))
 (step st (terminate) (waitfor (:and ?sFill ?sFill2))))

(procedure 
 (index (fill ?measuring-cup))
 (step s1 (fill-one ?measuring-cup with left-hand))
 (step s2 (terminate) 
       (waitfor ?s1))
 (step s3 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6)))

(procedure 
 (index (fill-one ?measuring-cup with ?hand))
 (profile ?hand attention  )
 (step pre (getNormRandinMS 1500 200 => ?d))
 (step s1 (fill one ?measuring-cup taking ?d)
       (waitfor ?pre))
 (step s2 (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) (waitfor ?s1)))

(primitive
 (index (fill one ?measuring-cup taking ?duration))
 (duration ?duration)
 (on-completion
  (inform `(filled ,?measuring-cup)
          :router *walter-router*)))

;;; ------- Phone --------------------------------------------------------

;;; Most of this was borrowed from Hello.lisp, but the old 
;;; version that used activities.

(procedure
 (index (phone))
 (step sPhone (answer-phone ?phone with right-hand) 
       (waitfor (ringing ?phone)))
 (step sMissed (terminate) 
       (waitfor (silent ?telephone)))
 (step sAnswered (terminate) 
       (waitfor ?sPhone)))

(procedure
 (index (answer-phone ?phone with ?hand))
 (profile ?hand attention presence )
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step pickup (pickup-phone ?phone with ?hand))
 (step talk (say-hello) 
       (waitfor ?pickup))
 (step hangup (hang-up ?phone) 
       (waitfor ?talk))
 (step stop (terminate) 
       (waitfor ?hangup)))

;;; The phone is answered by starting the picking-up-phone activity (the
;;; chosen duration is arbitrary).  This procedure completes when that
;;; activity completes.

(procedure
 (index (pickup-phone ?phone with ?hand))
 (profile ?hand attention)
 (step getDuration (getNormRandinMS 900 300 => ?d))
 (step pickup (pickup-phone ?phone with ?hand taking ?d)
       (waitfor ?getDuration))
 (step reset (reset +this-task+) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step terminate (terminate) 
       (waitfor ?pickup)))

(primitive
 (index (pickup-phone ?phone with ?hand taking ?duration))
 (duration ?duration)
 (on-completion
  (setx (grasped-object ?hand) ?phone)
  (setx (state ?phone) 'engaged)))
 
;;; Speech is uttered by starting the speaking activity (the chosen
;;; duration is arbitrary).  This procedure completes when that activity
;;; completes.

(procedure
 (index (say-hello))
 (profile voice)
 (step getDuration (getNormRandinMS 500 50 => ?d))
 (step talk (speak "Hello?" taking ?d)
       (waitfor ?getDuration))
 (step terminate (terminate) 
       (waitfor ?talk)))

(primitive
 (index (speak ?utterance taking ?duration))
 (profile voice)
 (duration ?duration)
 (on-completion
  (inform `(said ,?utterance))))
                     
;;; hang-up
(primitive
 (index (hang-up ?phone))
 (on-start (setx (state ?phone) 'silent)))

;;; --- TV tasks -------------------------------------------------------

(procedure
 (index (watch-for-watch-tv))
 (step s1 (watch-tv ?tv for ?time) 
       (waitfor (interesting ?tv ?time)))
 (step st (terminate) 
       (waitfor ?s1)))

(procedure
 (index (watch-tv ?tv for ?time))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (look at ?tv for ?time) 
       (waitfor ?move-to))
 (step s2 (change channel of ?tv) 
       (waitfor (this channel is boring)))
 (step s3 (change volume of ?tv) 
       (waitfor (I should change the volume)))
 (step st (terminate) 
       (waitfor ?s2 ?s3)))

(primitive
 (index (look at ?tv for ?time))
 (profile attention )
 (duration (300 ms))
 (on-completion 
  (inform `(this channel is boring) :router *walter-router*)))

(primitive
 (index (change channel of ?tv))
 (profile attention )
 (duration (100 ms))
 (on-start
  (setf (channel ?tv) (1+ (random 10))))
 (on-completion
  (inform `(I should change the volume) :router *walter-router*)))

(primitive
 (index (change volume of ?tv))
 (profile attention  )
 (duration (100 ms))
 (on-start
  (setf (volume ?tv) (1+ (random 10)))))

;;; --- Plant tasks-------------------------------------------------

(procedure
 (index (keep-eye-on-plant))
 (step sMild (water ?plant with left-hand) 
       (waitfor (getting-dry ?plant)) (priority 0))
 (step sMedium (water ?plant with left-hand) 
       (waitfor (i-should-water ?plant)) (priority 1))
 (step s1 (terminate ?sMild) 
       (waitfor (i-should-water ?plant)))
 (step sHot (water ?plant with left-hand) 
       (waitfor (going-to-wilt ?plant)) (priority 2))
 (step s2 (terminate ?sMedium) 
       (waitfor (going-to-wilt ?plant)))
 (step st (terminate +this-task+) 
       (waitfor (watered ?plant))))

(procedure
 (index (water ?plant with ?hand))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (watering ?plant with ?hand) 
       (waitfor ?move-to))
 (step st (terminate) 
       (waitfor ?s1)))

(primitive
 (index (watering ?plant with ?hand))
 (profile attention ?hand)
 (duration (100 ms))
 (on-start
  (setf (thirst ?plant) (random 5)))
 (on-completion
  (inform `(watered ,?plant) 
          :router *walter-router*)))

;;; --- Pick Flower--------------------------------------------------------

(procedure
 (index (pick-flower))
 (step s1 (pick ?flower from ?plant) 
       (waitfor (ready-to-pick ?flower ?plant)))
 (step st (terminate) 
       (waitfor ?s1)))

(procedure
 (index (pick ?flower from ?plant))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (get-from-drawer vase) 
       (waitfor ?move-to))
 (step s2 (pick ?flower from ?plant with right-hand) 
       (waitfor ?move-to))
 (step s3 (put ?flower into ?vase) 
       (waitfor (taken-from-drawer ?vase vase) ?s2))
 (step s4 (terminate) 
       (waitfor ?s3)))

(primitive 
 (index (pick ?flower from ?plant with ?hand))
 (profile attention  ?hand)
 (duration (100 ms))
 (on-start 
  (setf (flower ?plant) nil)))

(primitive
 (index (put ?flower into ?vase))
 (profile attention )
 (duration (100 ms))
 (on-start
  (setf (flower ?vase) ?flower)))

;;-------Check drawers----------------------------------------

;;Checks all drawers that are closed, shows their contents,
;;and takes out anything of type ?type.
(procedure
 (index (get-from-drawer ?type))
 (step s1 (check ?drawer) 
       (waitfor (shape ?drawer closed-drawer))) ; (repeating))
 (step s2 (take ?item out-of ?target-drawer with left-hand) 
       (waitfor (isa-in ?item ?type ?target-drawer)))
 (step sC (close-drawers) 
       (waitfor ?s2))
 (step s3 (terminate ?s1) 
       (waitfor (isa-in ?item ?type ?any-drawer)))
 (step s4 (terminate +this-task+) 
       (waitfor ?sC)))

(procedure
 (index (check ?drawer))
 (profile attention presence )
 (step move-to (move-to-locale-of ?drawer))
 (step on-resume (move-to-locale-of ?drawer) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (open ?drawer) 
       (waitfor ?move-to))
 (step s2 (look-in ?drawer) 
       (waitfor ?s1))
 (step s3 (terminate) 
       (waitfor ?s2)))

;;This is a bit of a cheat, but I figured I could 
;;get away with it.

(primitive 
 (index (close-drawers))
 (on-start
  (setf (shape (find-instance 'drawer-1)) '(closed-drawer))
  (setf (shape (find-instance 'drawer-2)) '(closed-drawer))
  (setf (shape (find-instance 'drawer-3)) '(closed-drawer))
  (setf (shape (find-instance 'drawer-4)) '(closed-drawer))))

(primitive
 (index (open ?drawer))
 (profile attention)
 (duration (100 ms))
 (on-start
  (setf (shape ?drawer) '(open-drawer))))

(primitive
 (index (look-in ?drawer))
 (profile attention )
 (duration (100 ms))
 (on-start 
  (if (equal (shape ?drawer) '(open-drawer))
      (show-contents (contents ?drawer)
		     ?drawer +self+))))

(primitive
 (index (close ?drawer))
 (on-start
  (setf (shape ?drawer) '(closed-drawer))))

;;;This is a way for me to alert the agent to what is in a drawer
;;;the cogevent isa-in might be a little awkward, but it's supposed
;;;to allow the agent to take out anything that ISA type of item
;;;he wants that is IN the drawer. hence, isa-in.
;;;NOTE: an isa slot is required for any item inside a drawer

(defun show-contents (contents drawer agent)
  (if (null contents)
      nil
    (progn (inform
	    (list 'isa-in (first contents) 
                  (isa (first contents)) drawer) :router *walter-router*)
	   (show-contents (rest contents) drawer agent)))) 

(primitive
 (index (take ?item out-of ?drawer with ?hand))
 (profile attention ?hand)
 (locals (dur (+ (current-time) (* 50 (mass ?item) (mass ?item)))) 
         (start nil))
 (update (10 ms) (if (>= (current-time) dur) (complete)))
 (on-start
  (setf start (current-time)))
 (on-completion
  (inform `(taken-from-drawer ,?item ,(isa ?item)) 
          :router *walter-router*)
  (setf (contents ?drawer) (remove ?item (contents ?drawer)))))

;;; --- Make Soup

(procedure 
 (index (make-soup))
 (profile presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (get-recipe vegetable-soup) (waitfor ?move-to))
 (step s2 (get-ingredients-from ?recipe) 
       (waitfor (found-recipe ?recipe)))
 (step sPot (prepare-pot-for-soup) (waitfor ?s2))
 (step s3 (follow ?recipe) (waitfor ?sPot))
 (step s4 (move ?pot onto counter using right-hand) 
       (waitfor ?s3 (shape ?pot pot)))
  (step s5 (terminate) (waitfor ?s4)))

(procedure
 (index (prepare-pot-for-soup))
 (step s1 (get-from-drawer pot))
 (step s2 (move ?pot onto front-right-burner using left-hand) 
       (waitfor (taken-from-drawer ?pot pot)))
 (step s3 (soup-in ?pot) (waitfor ?s2))
 (step s4 (terminate) (waitfor ?s3)))

(primitive
 (index (soup-in ?pot))
 (on-start
  (assert-physob-relation `(in ,(find-instance 'soup-1) ,?pot))))

(procedure
 (index (get-recipe ?recipe))
 (step s1 (get-cookbook))
 (step s2 (lookup-recipe ?recipe in ?cookbook) 
       (waitfor (taken-from-drawer ?cookbook cookbook)))
 (step s3 (terminate) (waitfor ?s2)))

(procedure
 (index (get-cookbook))
 (step s1 (get-from-drawer cookbook))
 (step s2 (terminate) (waitfor ?s1)))


(primitive
 (index (lookup-recipe ?recipe in ?cookbook))
 (profile attention )
  (duration (100 ms))
 (on-completion
  (let* ((recipe-list (recipes ?cookbook))
	 (desired-recipe 
	  (loop for r in recipe-list 
		do (if (equal (name r) ?recipe) (return r)))))
    (if (or (null desired-recipe) (null recipe-list))
	(inform `(no-recipe-named ,?recipe) 
                :router *cook-router*)
      (inform `(found-recipe ,desired-recipe) 
              :router *cook-router*)))))


(procedure
 (index (get-ingredients-from ?recipe))
 (step s1 (make-ingredients-list ?recipe))
 (step s2 (take-ingredients-out ?ingredients) 
       (waitfor (found-ingredient-list ?ingredients)))
 (step s3 (terminate) (waitfor ?s2)))

(primitive
 (index (make-ingredients-list ?recipe))
 (profile attention )
 (duration (100 ms))
 (on-completion
  (inform `(found-ingredient-list ,(ingredients ?recipe)) 
          :router *cook-router*)))

(procedure
 (index (take-ingredients-out ?ingredients))
 (step s1 (keep-an-eye-out-for ?type) (forall ?type in ?ingredients))
 (step s2 (open-drawers))
 (step s3 (close-drawers) (waitfor ?s1))
 (step s4 (terminate) (waitfor ?s3)))

(procedure
 (index (open-drawers))
 (step s1 (check ?drawer) 
       (waitfor (shape ?drawer closed-drawer)) (repeating)))

(procedure
 (index (keep-an-eye-out-for ?type))
 (step s1 (take ?item out-of ?drawer with right-hand) 
       (waitfor (isa-in ?item ?type ?drawer)))
 (step st (terminate) (waitfor ?s1)))

(procedure
 (index (follow ?recipe))
 (step s1 (get-and-do-next-instruction ?recipe) (repeating))
 (step s2 (terminate) (waitfor (no-more-instructions ?recipe))))

(procedure
 (index (get-and-do-next-instruction ?recipe))
 (profile attention )
 (step s1 (get-next-instruction ?recipe))
 (step s2 (execute-instruction ?instruction)
       (waitfor ?s1 (next-instruction ?instruction)))
 (step st (terminate) (waitfor ?s2)))

(primitive
 (index (get-next-instruction ?recipe))
 (duration (100 ms))
 (on-completion
  (let* ((index (current-instruction ?recipe))  
	 (next-instruction (nth index (instructions ?recipe))))
    (if (null next-instruction)
	(inform `(no-more-instructions ,?recipe) :router *cook-router*) 
      (progn
	(setf (current-instruction ?recipe) (1+ index))
	(inform `(next-instruction ,next-instruction) :router *cook-router*))))))

(procedure
 (index (execute-instruction ?instruction))
  (step sRead (instruction-to-inform ?instruction))
 (step s1 (dice ?item with right-hand) (waitfor (dice ?item)))
 (step s2 (add ?item2 to ?container) 
       (waitfor (add ?item2 to ?container)))
 (step s3 (saute ?itemlist in ?container2 for ?duration) 
       (waitfor (saute ?itemlist in ?container2 for ?duration)))
 (step s4 (cook ?duration) 
       (waitfor (cook ?duration)))
 (step st (terminate) 
       (waitfor ?s1 ?s2 ?s3 ?s4)))

(primitive
 (index (instruction-to-inform ?instruction))
 (on-start
  (let ((directive (directive ?instruction)))
    (inform (directive) :router *cook-router*))))

(primitive
 (index (dice ?item with ?hand))
 (profile ?hand attention )
 (duration (100 ms))
 (on-completion
  (setf (shape ?item) (append (shape ?item) '(diced)))))

(primitive
 (index (add ?item to ?container))
 (profile attention )
 (duration (100 ms))
 (on-completion
  (setf (contents ?container) (append (contents ?container) 
                                      (list ?item)))
  (setf (contents (find-instance 'soup-1)) 
	(append (contents (find-instance 'soup-1)) (list ?item)))
(assert-physob-relation `(in ,?item ,?container)))

)

(procedure
 (index (saute ?itemlist in ?container for ?duration))
 (step s1 (add ?item to ?container) (forall ?item in ?itemlist))
 (step s2 (turn-dial front-right-dial 2 using left-hand) (waitfor ?s1))
 (step s3 (wait ?duration) (waitfor ?s2))
 (step s4 (terminate) (waitfor ?s3)))

(primitive
 (index (wait ?duration))
 (locals (beginning 0) (dur 0))
 (on-start (setf beginning (current-time)) 
           (setf dur (parse-time ?duration)))
 (update (10 ms) 
	 (if (>= (current-time) (+ beginning dur)) 
	     (complete-the-activity (action +this-task+)))))

(procedure
 (index (cook ?duration))
 (step s1 (turn-dial front-right-dial 5 using right-hand))
 (step s2 (wait ?duration) (waitfor ?s1))
 (step s3 (turn-dial front-right-dial 0 using left-hand) (waitfor ?s2))
 (step s4 (terminate) (waitfor ?s3)))

;;; ---------- Locale switching stuff ----------------------------------

;;; There are a lot of functions in this section, only a few of which I 
;;; ended up using for the actual locale switching, but I thought some of
;;; them might be useful in otherapplications

;; might want to extend to include components of agent, as well as things
;; it is holding. For instance, at the moment the right and left hand 
;; stay in the kitchen when the agent moves.

(defmethod switch-locale (agent (new-locale locale))
  (progn
    (setf (contents (locale agent)) 
      (remove agent (contents (locale agent)) :test #'equal))
    (setf (locale agent) new-locale)
    (setf (contents (locale agent)) 
      (append (list agent) (contents (locale agent))))))

(defmethod find-locale-of ((obj visob))
  (locale (visob obj)))

(defmethod find-locale-of ((obj appob))
  (locale obj))

(defun go-to-locale-of (obj agent)   
  (switch-locale agent (find-locale-of obj)))

(procedure
 (index (move-to ?locale))
 (profile attention)
 (step s1 (same-locale? ?locale))
 (step s2 (move-directly-to ?locale) 
       (waitfor (need-move-to)))
 (step s3 (terminate) 
       (waitfor (:or ?s2 (no-need-move-to)))))

(primitive
 (index (same-locale? ?locale))
 (on-start
  (if (equal ?locale (name (locale +self+)))
      (inform `(no-need-move-to) 
              :router *walter-router* )
    (inform `(need-move-to) 
            :router *walter-router* ))))
   
;;;This function requires knowing that kitchen is locale-1,
;;;living-room is locale-2, and front-porch is locale-3.

(primitive
 (index (move-directly-to ?locale))
 (duration (100 ms))
 (on-completion
  (if (equal ?locale 'kitchen)                      
      (switch-locale +self+ (find-instance 'locale-1))
    (if (equal ?locale 'living-room)
	(switch-locale +self+ (find-instance 'locale-2))
      (switch-locale +self+ (find-instance 'locale-3))))))

(procedure
 (index (move-to-locale-of ?obj))
 (step s1 (in-same-locale? ?obj))
 (step s2 (move-locale-needed ?obj) 
       (waitfor (not-same-locale +self+ ?obj)))
 (step s3 (terminate) 
       (waitfor (same-locale +self+ ?obj)  ?s2)))

(primitive
 (index (in-same-locale? ?object))
 (on-start
  (if (equal (find-locale-of ?object) (locale +self+))
       (inform `(same-locale ,+self+ ,?object))
    (inform `(not-same-locale ,+self+ ,?object)))))

(primitive
 (index (move-locale-needed ?obj))
 (duration (15 secs))
 (profile attention  )
 (on-start
  (let ((desired-locale (find-locale-of ?obj)))
    (switch-locale +self+ desired-locale))))

;;; --- Timing procedures -------------------------------------------

;;; Begin Timing is a procedure that starts the timing for a particular
;;; task.  It calls functions from timing.lisp, and right now only
;;; times the egg cooking.  This is all a relic from the class project,
;;; but the work this summer does not replace this sort of functionality
;;; so I decided to leave it in  

(primitive
 (index (wait ?duration))
 (locals (beginning 0) (dur 0))
 (on-start (setf beginning (current-time)) 
           (setf dur (parse-time ?duration)))
 (update (10 ms) 
	 (if (>= (current-time) (+ beginning dur)) 
	     (complete-the-activity (action +this-task+)))))

(primitive
 (index (getNormRandinMS ?mean ?sd))
 (return (list (round (normRand ?mean ?sd)) 'ms)))

(primitive
 (index (beginTiming))
 (return 
   (progn
     (startTaskTiming 'cookTask (list 'cookEgg) (current-time))
     (let ((dur (first (guessDuration (list 'cookEgg))))
	   (now (round (/ (current-time) 1000))))    
       (if (equal dur -1)
           (setf *globalAlarmTime* (1+ now))
	 (setf *globalAlarmTime* (+ (- now 5) (round (/ dur 1000)))))))))

;;Allows the estimation of the duration of egg-cooking to be scaled
;;based on how overcooked it was

(primitive
 (index (endTiming ?texture))
 (return
   (cond
    ((equal ?texture 'spongy)
     (endTaskTiming 'cookTask (current-time)))
    ((equal ?texture 'crispy)
     (endTaskTiming 'cookTask (current-time))
     (scaleMostRecentDuration '(cookEgg) 0.75))
    ((equal ?texture 'very-crispy)
     (endTaskTiming 'cookTask (current-time))
     (scaleMostRecentDuration '(cookEgg) 0.5))
    (t
     (endTaskTiming 'cookTask (current-time))
     (scaleMostRecentDuration '(cookEgg) 0.25)))))



