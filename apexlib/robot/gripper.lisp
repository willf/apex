;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Robot gripper
;;; apex/apexlib/robot/gripper.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: gripper.lisp,v 1.5 2006/01/15 03:42:51 dalal Exp $


(in-package :common-lisp-user)
(require-apex-library "geometry")

;;; -----------------------------------------------------------------------
;;; ----- Grippers
;;; -----------------------------------------------------------------------

(defclass gripper (resource physob)
  ((grasped-object :accessor grasped-object :initarg :grasped-object
		   :initform nil)))

;;; Activity and effect definitions

;;; Grippers engage in resource-activities in response to commands from the ASA.
;;; The effect of completing such as activity often depends on factors that are
;;; meaningful within a particular application.  For example, the effect of a
;;; pulling-apart activity on an eggshell is specific to eggs (e.g.  falling out
;;; yolk).  Methods for gripperling such completion events (and similar for
;;; starting and updating an activity) are specified in a simworld definition
;;; file.  General (default) methods are possible for some activities (e.g.
;;; see GRASPING below)

;;; -- Turning and turned

(defclass turning (resource-activity)
  ((object :accessor object :initarg :object)
   ;; the thing that is being turned
   (setting :accessor setting :initarg :setting)
   ));;the direction or setting to which it is being turned

(defmethod complete-activity ((act turning) (h gripper))
  (signal-event (turned (object act) (setting act))))

;;; -- Moving and moved
;;;
;;; Used to move a gripper and possibly some other object to a destination.
;;; Currently the object must be grasped to be moved, though eventually
;;; this could support pushing and the like.  If the destination is a 
;;; physob, the moved gripper is considered to be ON it.

(defclass moving (resource-activity)
  ((object :accessor object :initarg :object)
   (destination :accessor destination :initarg :destination)
   (preposition :accessor preposition :initarg :preposition :initform nil)))

(defmethod initialize-activity ((act moving) (h gripper))
  (when (and (object act) (not (equal (grasped-object h) (object act))))
    (stop-activity act)))

(defmethod complete-activity ((act moving) (h gripper))
  ;;(when (preposition act)
  ;;  (assert-physob-relation `(,(preposition act) ,(primary-object act) ,h)))
  (cond ((typep (destination act) 'physob)  
	 ;; if the place to be moved is another object
         ;;(assert-physob-relation `(in ,(primary-object act) ,h))
         (signal-event 
          (moved-to-object (primary-object act) (destination act))))
	(t 
       	 (signal-event 
	  (moved-to-position (primary-object act) (destination act) 
			     :cause act)))))

;;; -- Pulling-apart and pulled-apart

(defclass pulling-apart (resource-activity)
  ((object :accessor object :initarg :object)
   ;; the object that is being pulled apart
   (pulling-agent :accessor pulling-agent :initarg :pulling-agent)
   (holding-agent :accessor holding-agent :initarg :holding-agent)))

(defmethod complete-activity ((act pulling-apart) (h gripper))
  (signal-event (pulled-apart (object act))))

;;; -- Striking and struck

(defclass striking (resource-activity)
  ((object :accessor object :initarg :object)
  ;;object being struck
   (against :accessor against :initarg :against))
  );;what it is struck against
   
(defmethod complete-activity ((act striking) (h gripper))
  (signal-event (struck (object act) (against act))))

;;; -- Grasping and grasped

(defclass grasping (resource-activity)
  ((object :accessor object :initarg :object)))  ;;what is being grasped

(defmethod complete-activity ((act grasping) (h gripper))
  (signal-event (grasped (object act) h)))

;;;default method for effect of grasping
;;; the hand analog doesn't have the cause keyword
(defmethod grasped ((obj physob) (h gripper)
;;;		    &key cause
			 )
  (grasped2 h obj 
;;	    cause
	    )
  (assert-physob-relation `(in ,obj ,h)))

(defun grasped2 (a b 
;;		 c
		 )
  (setx (grasped-object a) b 
;;	:cause c
	))

;;; --- Releasing and released

(defclass releasing (resource-activity)
  ((object :accessor object :initarg :object)))

(defmethod initialize-activity ((act releasing) (h gripper))
  (when (not (equal (grasped-object h) (object act)))
    (stop-activity act)))

(defmethod complete-activity ((act releasing) (h gripper))
  (signal-event (released (object act) h)))

(defmethod released ((obj physob) (h gripper))
  (retract-physob-relation `(in ,(grasped-object h) ,h))
  (retract-physob-relation `(on ,(grasped-object h) ,h))
  (setx (grasped-object h) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FITTS LAW
; T = a log2(D/S + b)

(defvar *fitts-a* 100)
(defvar *fitts-b* 0.5)

(defun fitts (d s)
  (floor (* *fitts-a* (log (+ (/ d s) *fitts-b*) 2))))

(defmethod fitts-time ((pointer gripper) (v visobfile))
  ;; pointer is gripper before movement
  ;; v is object to move to
  (let ((obj (visobfile-visob v)))
    (fitts (distance (pos pointer) (pos obj))
           (min (first (dimensions obj))
                (second (dimensions obj))))))

(defmethod fitts-time ((pointer gripper) obj)
  ;; pointer is gripper before movement
  ;; obj is object to move to
  (fitts (distance (pos pointer) (pos obj))
         (min (first (dimensions obj))
              (second (dimensions obj)))))

;;; --- Grasp object

;;; Causes an object to be grasped by the selected gripper.  Requires a
;;; <necessity> parameter, T or nil, for whether the object is already
;;; grasped.

(procedure
 (index (grasp ?obj with ?gripper))
 (step s1 (grasp ?obj with ?gripper if-needed ?needed)
       (select ?needed (not (equal ?obj (grasped-object ?gripper)))))
 (step s2 (terminate) (waitfor ?s1)))

(procedure
 (index (grasp ?obj with ?gripper if-needed t))  ;; t means ?obj not already grasped
 (profile (?gripper 8 10))
 (step s1 (start-activity ?gripper grasping :object ?obj :duration 1000 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?hack1)))

(procedure
 (index (grasp ?obj with ?gripper if-needed nil))  ;; no grasp action needed
 (step s1 (terminate)))

;;; --- Strike object

(procedure
 (index (strike ?obj1 against ?obj2 using ?gripper))
 (profile (?gripper 8 10))
 (step s1 (start-activity ?gripper striking :object ?obj1 :against ?obj2 
			  :duration 2000 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?hack1)))

;;; --- Release object

(procedure
 (index (release ?obj from ?gripper))
 (profile (?gripper 8 10))
 (step s1 (start-activity ?gripper releasing :object ?obj :duration 800 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?hack1)))


;;; -------------------------------------------------------------------------
;;; General-skills
;;; -------------------------------------------------------------------------

;;; --- Type message

;;; Assumes list of arguments following specification of gripper.  Arguments
;;; preceding the optional keyword :meaning are considered individual
;;; words/keystrokes.  If a meaning argument is supplied, it is gripperled
;;; separately by the activity completion mechanism, presumably making it
;;; available to whatever object processes the typed message.

(procedure
 (index (type ?gripper (?* ?message))) 
 (profile (?gripper 9 10))
 (step s1 (parse-message ?message => (?keystrokes ?meaning)))
 (step s2 (start-activity ?gripper typing :keystrokes ?keystrokes :meaning
			  ?meaning :duration 1000 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s3 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s4 (terminate) 
       (waitfor ?s2 ?hack1))) 

;;; used for both type and say actions
;;;(procedure :special
;;; (index (parse-message ?u))
;;; (let ((flag nil) (words nil) (msg nil))
;;;   (dolist (item ?u)
;;;     (if flag (setf msg item)
;;;       (if (equal item :meaning)
;;;	   (setf flag t)
;;;	 (setf words (append words (list item))))))
;;;   (list words msg)))

(primitive
 (index (parse-message ?u))
 (return 
   (let ((flag nil) (words nil) (msg nil))
     (dolist (item ?u)
       (if flag (setf msg item)
	   (if (equal item :meaning)
	     (setf flag t)
	     (setf words (append words (list item))))))
     (list words msg))))

;;; --- Press Button

(procedure
 (index (press-button ?gripper ?b))
 (profile (?gripper 10 10))
 (step s1 (start-activity ?gripper pressing :object ?b :duration 1000 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s2 (terminate) (waitfor ?hack1)))

;;; --- Turn dial

(procedure 
 (index (turn-dial ?dial ?setting using ?gripper))
 (profile (?gripper 10 10))
 (step s1 (start-activity ?gripper turning :object ?dial :setting ?setting 
			  :duration 1000 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s2 (terminate) (waitfor ?hack1)))


;;; --- Move object

;;; Puts an object at the location of another object.  If the gripper
;;; being used to move the object is already holding something, the
;;; held object will be dropped.  If the object is already at the
;;; target location, this procedure will terminate without taking
;;; action.

(procedure
 (index (move ?object1 ?preposition ?object2 using ?gripper))
 (profile (?gripper 8 10))
 (step s1 (grasp ?object1 with ?gripper))
 (step s2 (start-activity ?gripper moving :object ?object1 
			  :destination ?object2 
                          :preposition ?preposition
                          :duration 1200 => ?act)
       (waitfor ?s1))
 (step s3 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s2))
 (step s5 (terminate) (waitfor ?hack1)))

