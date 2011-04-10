;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/living-room-pdl.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: living-room-pdl.lisp,v 1.8 2006/01/15 03:42:57 dalal Exp $

;;; Walter's procedures for the Living Room

;(in-package :user)
(in-package :common-lisp-user)

(procedure
 (index (living-room-tasks))
 (step playcd (change-cd ?cd)        
       (waitfor (music-stopped ?cd)) (priority 5))
 (step dog (dog-stuff) (priority 2))
 (step window (check-window) (priority 1))
 (step check-lamp (check-lamp ?lamp) 
       (waitfor (shape ?lamp bright-lamp)) (priority 1))
 (step comp (computer-stuff) (priority 2))
 (step terminate (terminate) 
       (waitfor ?playcd ?dog ?comp ?window ?check-lamp)))

;;; --- Change-cd --------------------------------------------------

(procedure
 (index (change-cd ?cd))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (restart-music ?cd with left-hand) 
       (waitfor ?move-to))
 (step st (terminate) 
       (waitfor ?s1)))

(primitive
 (index (restart-music ?cd with ?hand))
 (profile ?hand)
 (duration (100 ms)))

;;; --- Actions for Dog ----------------------------------------

(procedure
 (index (dog-stuff))
 (step s1 (feed-dog) 
       (waitfor (barking ?dog)))
 (step s2 (pet-dog) 
       (waitfor (leaning-on-cook ?dog)))
 (step s3 (move-to-kitchen) 
       (waitfor ?s2))
 (step sulk (help-poor-doggy ?dog) 
       (waitfor (sulking ?dog)) (priority 5))
 (step s3 (terminate) 
       (waitfor ?s1 ?s2)))

(procedure
 (index (move-to-kitchen))
 (profile attention presence)
 (step move-to (move-to kitchen))
 (step on-resume (move-to kitchen) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s2 (terminate) (waitfor ?move-to)))

(procedure
 (index (feed-dog))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (give-dog-food-in ?dish) 
       (waitfor ?move-to (shape ?dish dog-dish)))
 (step s2 (terminate) (waitfor ?s1)))

(primitive
 (index (give-dog-food-in ?dish))
 (duration (100 ms))
 (on-completion
  (setf (food ?dish) t)
  (inform (list 'food-in-dish ?dish)
          :router *walter-router*)))

(procedure ;;for when the dog sulks
 (index (help-poor-doggy ?dog))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (get-dog-treat) 
       (waitfor ?move-to))
 (step s2 (tell-sit ?dog) 
       (waitfor ?s1))
 (step s3 (give-treat ?dog) 
       (waitfor (sitting ?dog)))
 (step s4 (terminate) 
       (waitfor ?s3)))

(primitive
 (index (get-dog-treat))
 (duration (100 ms)))

(primitive
 (index (tell-sit ?dog))
 (duration (100 ms))
 (on-start
  (inform `(sit-dog) 
          :router *walter-router*)))

(primitive 
 (index (give-treat ?dog))
 (duration (200 ms))
 (on-start (setf (mood ?dog) 'happy)))

(procedure
 (index (pet-dog))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (pet-the-dog) 
       (waitfor ?move-to))
 (step st (terminate) 
       (waitfor ?s1)))

(primitive
 (index (pet-the-dog))
 (duration (100 ms))
 (on-start
  (inform `(petting-dog) 
          :router *walter-router*)))

;;; --- Actions for Computer -----------------------------------------

(procedure
 (index (computer-stuff))
 (step s1 (turn-comp-on))
 (step s2 (check-email) 
       (waitfor ?s1))
 (step s3 (play-game) 
       (waitfor (no-more-email)))       ;(no-new-email)))
 (step s4 (turn-comp-off) 
       (waitfor ?s3))                   ; simplified condition for termination
 (step st (terminate) 
       (waitfor ?s4))) 

(procedure
 (index (turn-comp-on))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (turn-on-computer) 
       (waitfor ?move-to))
 (step st (terminate) 
       (waitfor (computer-on))))

(primitive
 (index (turn-on-computer))
 (duration (100 ms))
 (on-completion (inform `(turned-on-by ,+self+) 
                        :router *walter-router*)))

(procedure
 (index (check-email))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (get-email) 
       (waitfor ?move-to))
 (step s2 (read-email) 
       (waitfor (new-email)))
 (step s3 (terminate) 
       (waitfor (:or ?s2 (no-new-email)))))

(primitive
 (index (get-email))
 (profile attention)
 (duration (100 ms))
 (on-completion
  (inform `(want-to-check-email) 
          :router *walter-router*)))

(procedure
 (index (read-email))
 (profile attention)
 (step s1 (practice)
       (waitfor (upcoming-fencing-tournament)))
 (step s2 (vacuum) 
       (waitfor (friend-coming-to-visit)))
 (step s3 (spam)
       (waitfor (money-problems-getting-you-down?)))
 (step st (terminate) 
       (waitfor (no-more-email))))

(primitive
 (index (practice))
 (on-start
  (inform `(i-should-practice-fencing) 
          :router *walter-router*)))

(primitive
 (index (vacuum))
 (on-start
  (inform `(i-should-vacuum) 
          :router *walter-router*)))

(primitive
 (index (spam))
 (on-start
  (inform `(stupid-spam) 
          :router *walter-router*)))

(procedure
 (index (play-game))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (ask-computer-to-play) 
       (waitfor ?move-to))
 (step s2 (play-with-computer) 
       (waitfor (ready-for-move)) (responding))
 (step st (terminate) 
       (waitfor (:or (human-wins) (human-loses)))))

(primitive
 (index (ask-computer-to-play))
 (profile attention)
 (duration (100 ms))
 (on-start (inform `(want-to-play-game) 
                   :router *walter-router*)))

(primitive
 (index (play-with-computer))
 (profile attention)
 (duration (100 ms))
 (on-completion
  (inform `(human-move ,(pick-number)) 
          :router *walter-router*)))

(procedure
 (index (turn-comp-off))
 (duration (100 ms))   
 (step st (terminate)))

;;; --- Window--------------------------------------------------

(procedure
 (index (check-window))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (check-window ?window) 
       (waitfor (shape ?window window) ?move-to))
 (step s2 (terminate) 
       (waitfor ?s1)))

(procedure
 (index (check-window ?window))
 (step s1 (close-if-necessary ?window))
 (step s2 (terminate) 
       (waitfor ?s1)))

(primitive
 (index (close-if-necessary ?window))
 (duration (100 ms))
 (locals (w nil))
 (on-start (setf w ?window))
 (on-completion
  (unless (equal (weather-outside w) 'sunny)
    (setf (opened w) 'closed))))

;;; --- Lamp -----------------------------------------------------------

(procedure
 (index (check-lamp ?lamp))
 (profile attention presence)
 (step move-to (move-to living-room))
 (step on-resume (move-to living-room) 
       (waitfor (resumed +this-task+)) (priority 6))
 (step s1 (turn-off ?lamp) 
       (waitfor ?move-to))
 (step st (terminate) 
       (waitfor ?s1)))

(primitive
 (index (turn-off ?lamp))
 (duration (100 ms))
 (on-completion
  (setf (shape ?lamp) '(not-bright))
  (setf (power ?lamp) 'off)))

;; --- pick-number -----------------------------------------------------

(defun pick-number () 
  (random 10))


 

 
