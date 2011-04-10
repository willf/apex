;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/dog.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: dog.lisp,v 1.2 2006/01/15 03:42:56 dalal Exp $

;;; --- Dog----------------------------------------------------------


(in-package :user)

(defclass dog (agent physob)
  ((sleeping 
    :accessor sleeping 
    :initarg 
    :sleeping 
    :initform t)
   (hunger 
    :accessor hunger 
    :initarg 
    :hunger 
    :initform 0)
   (mood 
    :accessor mood 
    :initarg 
    :mood 
    :initform 'asleep)
   (barking-at 
    :accessor barking-at 
    :initarg 
    :barking-at 
    :initform 'nothing)))

(defclass food-dish (physob)
  ((food 
    :accessor food 
    :initform nil)
   (shape 
    :initform '(dog-dish))))

(in-apex-bundle :dog)

(procedure
 (index (be dog))
 (step s0 (watch-for-human) 
       (waitfor (dog-awake)))
 (step s1 (update-hunger) 
       (waitfor (dog-awake)))
 (step s2 (get-food) 
       (waitfor (hungry)))              ; (repeating))
 (step s3 (try-to-get-pet) 
       (waitfor (walter-in-room)) (responding)) ; (repeating))
 (step s4 (sit) 
       (waitfor (sit-dog)))
 (step s9 (wake-up) 
       (waitfor (wake-up-dog))))

(primitive
 (index (sit))
 (duration (100 ms)) 
 (on-completion
  (inform `(sitting ,+self+) 
          :router *walter-router*)))

(primitive
 (index (watch-for-human))
 (locals (times-not-in-room 0) (already-tried-this-time nil))
 (update (100 ms)
         (if (equal (locale (find-instance `human-1)) (locale +self+))
	     (if (not already-tried-this-time)
                 (progn 
                   (inform `(walter-in-room) 
                           :router *walter-router*)           
                   (setf already-tried-this-time t)
                   (setf times-not-in-room 0)))       
           (progn
	     (if already-tried-this-time
		 (progn	(inform `(cook-left) 
                                :router *walter-router*)
			(setf already-tried-this-time nil)))
           ;;; ! hack for faster testing ; was 60
	     (if (> (incf times-not-in-room) 5)
		 (progn (setf (mood +self+) 'sulking)
			(inform (list 'sulking +self+)
                                :router *walter-router*)
                        (setf times-not-in-room 0)))))))


;; get dog to learn which one works best for getting cook's attention?
;; will have problems with timing of task-start and 
;; reception of cogevent.  deal with it later?

(procedure
 (index (try-to-get-pet))
 (profile doggy-attention)
 (step s1 (wag-tail))
 (step s2 (approach-cook) 
       (waitfor ?s1))
 (step s3 (lean-on-cook) 
       (waitfor ?s2))
 (step s4 (puppy-dog-eyes) 
       (waitfor ?s3))
 (step s5 (sulk) 
       (waitfor ?s4))
 (step s6 (happy-dog) 
       (waitfor (petting-dog))) 
 (step stfail (terminate) 
       (waitfor (cook-left)))
 (step stsuccess (terminate) 
       (waitfor ?s6))
 (step st (terminate) 
       (waitfor ?s5)))

(primitive
 (index (happy-dog))
 (duration (100 ms)) 
 (on-start
  (inform `(dog-is-happy ,+self+) 
          :router *walter-router*)))

(primitive
 (index (wag-tail))
 (duration (100 ms)) 
 (on-start
  (inform `(wagging-tail ,+self+) 
          :router *walter-router*)))

(primitive
 (index (approach-cook))
 (duration (100 ms)) 
 (on-start
  (inform `(approaching-cook ,+self+) 
          :router *walter-router*)))

(primitive
 (index (lean-on-cook))
 (duration (100 ms)) 
 (on-start
  (inform `(leaning-on-cook ,+self+) 
          :router *walter-router*)))

(primitive
 (index (puppy-dog-eyes))
 (duration (100 ms)) 
 (on-start
  (inform `(puppy-dog-eyes ,+self+) 
          :router *walter-router*)))

(primitive
 (index (sulk))
 (duration (100 ms)) 
 (on-start
  (inform `(sulking ,+self+) 
          :router *walter-router*)
  (setf (mood +self+) 'sulking)))

(primitive
 (index (update-hunger))
 (update (100 ms) 
	 (incf (hunger +self+) (random 3))
	 (cond 
	  ((< (hunger +self+) 30)
	   nil)
	  (t
	   (inform `(hungry) :router *walter-router*)
	   (setf (mood +self+) 'hungry)))))

(procedure
 (index (get-food))
 (profile doggy-attention)
 (step s1 (bark))
 (step s2 (eat-food-from ?dish) 
       (waitfor (food-in-dish ?dish)))
 (step sh (happy-dog) 
       (waitfor ?s2))
 (step test (test) 
       (waitfor ?sh))
 (step s3 (terminate) 
       (waitfor ?sh)))

(primitive
 (index (test))
 (duration (100 ms)) 
 (on-start 
  (inform `(this should happen afer happy dog from eating) 
          :router *walter-router*)))

(primitive
 (index (eat-food-from ?dish))
 (duration (100 ms)) 
 (on-start
  (inform  `(in eat food with dish ?dish)))
 (on-completion
  (setf (food ?dish) nil)
  (setf (hunger +self+) 0)))

(primitive
 (index (bark))
 (duration (100 ms)) 
 (on-start 
  (inform `(barking ,+self+) 
          :router *walter-router*)))

(procedure
 (index (wake-up))
 (profile doggy-attention)
 (step s1 (yawn))
 (step s2 (stretch))
 (step s3 (get-up) 
       (waitfor ?s2 ?s1))
 (step s4 (terminate) 
       (waitfor ?s3)))

(primitive
 (index (yawn))
 (duration (100 ms)))

(primitive
 (index (stretch))
 (duration (100 ms)))

(primitive
 (index (get-up))
 (duration (100 ms)) 
 (on-start
  (setf (sleeping +self+) nil)
  (setf (mood +self+) (compute-doggy-mood +self+))
  (inform `(dog-awake) 
          :router *walter-router*)))

(defun compute-doggy-mood (dog) 
  ;;available for more elaboration. just a backbone now
  (if (sleeping dog)
      'asleep
    'happy))



