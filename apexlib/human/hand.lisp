;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Hand resource
;;; apex/apexlib/human/hand.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: hand.lisp,v 1.8 2006/01/15 03:42:50 dalal Exp $


(in-package :common-lisp-user)
(require-apex-library "geometry")

;;; -----------------------------------------------------------------------
;;; ----- Hands
;;; -----------------------------------------------------------------------

(defclass hand (resource physob)
  ((grasped-object :accessor grasped-object :initarg :grasped-object
		   :initform nil)))

(defclass left-hand (hand) ())
  
(defclass right-hand (hand) ())

;;; Activity and effect definitions

;;; Hands engage in resource-activities in response to commands from the ASA.
;;; The effect of completing such as activity often depends on factors that are
;;; meaningful within a particular application.  For example, the effect of a
;;; pulling-apart activity on an eggshell is specific to eggs (e.g.  falling out
;;; yolk).  Methods for handling such completion events (and similar for
;;; starting and updating an activity) are specified in a simworld definition
;;; file.  General (default) methods are possible for some activities (e.g.
;;; see GRASPING below)


;;; -- Moving and moved
;;;
;;; Used to move a hand and possibly some other object to a destination.
;;; Currently the object must be grasped to be moved, though eventually
;;; this could support pushing and the like.  If the destination is a 
;;; physob, the moved hand is considered to be ON it.

(primitive
 (index (move ?object ?preposition ?destination with ?hand taking ?duration))
 (duration ?duration)
 (on-start
    (when (and ?object (not (equal (grasped-object ?hand) ?object)))
      (warn (concatenate 'string
              "Ignoring attempt to move object ~a, because "
              "hand ~a is holding ~a")
            ?object ?hand (grasped-object ?hand))
    (complete)))
 (on-completion
  (if (typep ?destination 'physob)  
      ;; the place to be moved is another object
      (signal-event (moved-to-object ?object ?destination))
    (signal-event (moved-to-position ?object ?destination)))))

;;; -- Pull-appart

;;; Two hand version

(primitive
 (index (pull-apart ?object taking ?duration))
 (profile (left-hand 8 10) (right-hand 8 10))
 (duration ?duration)
 (on-completion
  (signal-event (pulled-apart ?object))))

;;; One hand

(primitive
 (index (pull-apart ?object with ?hand taking ?duration))
 (duration ?duration)
 (on-completion
  (signal-event (pulled-apart ?object))))

;;; -- Striking and struck

(primitive
 (index (strike ?obj1 against ?obj2 with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (struck ?obj1 ?obj2))))

(defmethod struck ((obj1 physob) (obj2 physob)))

;;; -- Grasping and grasped

(primitive
 (index (grasp ?object with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion (signal-event (grasped ?object ?hand))))
 
;;;default method for effect of grasping
(defmethod grasped ((obj visobfile) (h hand))
  (grasped (visobfile-visob obj) h))
  
(defmethod grasped ((obj visob) (h hand))
  (grasped2 h obj)
  (assert-physob-relation `(in ,obj ,h)))

(defun grasped2 (a b)
  (setx (grasped-object a) b))

;;; --- Releasing and released

;(defclass releasing (resource-activity)
;  ((object :accessor object :initarg :object)))

(primitive
 (index (release ?obj from ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-start
  (when (not (equal (grasped-object ?hand) ?obj))
    (format t "Attempt by ~a to release ~a, which is not currently held"
          ?hand ?obj)
    (complete)))
 (on-completion
  (signal-event (released ?obj ?hand))))

(defmethod released ((obj physob) (h hand))
  (retract-physob-relation `(in ,(grasped-object h) ,h))
  (retract-physob-relation `(on ,(grasped-object h) ,h))
  (setx (grasped-object h) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FITTS LAW
; T = a log2(D/S + b)

(defvar *fitts-a* 100)
(defvar *fitts-b* 0.5)

(defun fitts (d s)
  (max (floor (* *fitts-a* (log (+ (/ d s) *fitts-b*) 2)))
       0))


(defmethod fitts-time ((pointer hand) (v visobfile))
  ;; pointer is hand before movement
  ;; v is object to move to
  (let ((obj (visobfile-visob v)))
    (fitts (distance (pos pointer) (pos obj))
           (min (first (dimensions obj))
                (second (dimensions obj))))))

(defmethod fitts-time ((pointer hand) obj)
  ;; pointer is hand before movement
  ;; obj is object to move to
  (fitts (distance (pos pointer) (pos obj))
         (min (first (dimensions obj))
              (second (dimensions obj)))))

;;; --- Grasp object

;;; Causes an object to be grasped by the selected hand.  Requires a
;;; <necessity> parameter, T or nil, for whether the object is already
;;; grasped.

(procedure
 (index (grasp ?obj with ?hand))
 (step s1 (grasp ?obj with ?hand if-needed ?needed)
       (select ?needed (not (equal ?obj (grasped-object ?hand)))))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 ;; t means ?obj not already grasped
 (index (grasp ?obj with ?hand if-needed t))  
 (profile (?hand 8 10))
 (step s1 (grasp ?obj with ?hand taking (1 sec)))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))

(procedure
 (index (grasp ?obj with ?hand if-needed nil))  ;; no grasp action needed
 (step s1 (terminate)))

;;; --- Strike object

(procedure
 (index (strike ?obj1 against ?obj2 using ?hand))
 (profile (?hand 8 10))
 (step s1 (strike ?obj1 against ?obj2 with ?hand taking (2 secs)))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))

;;; --- Release object

(procedure
 (index (release ?obj from ?hand))
 (profile (?hand 8 10))
 (step s1 (release ?obj from ?hand taking (800 ms)))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))


;;; -------------------------------------------------------------------------
;;; General-skills
;;; -------------------------------------------------------------------------

;;; --- Move object

;;; Puts an object at the location of another object.  If the hand
;;; being used to move the object is already holding something, the
;;; held object will be dropped.  If the object is already at the
;;; target location, this procedure will terminate without taking
;;; action.

(procedure
 (index (move ?object1 ?preposition ?object2 using ?hand))
 (profile (?hand 8 10))
 (step s1 (grasp ?object1 with ?hand))
 (step s2 (move ?object1 ?preposition ?object2 with ?hand taking (1200 ms))
       (waitfor ?s1))
 (step s3 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s5 (terminate) (waitfor ?s2)))
