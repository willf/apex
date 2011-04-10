;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Robot class
;;; apex/apexlib/robot/robot.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: robot.lisp,v 1.12 2006/01/15 03:42:51 dalal Exp $

(in-package :common-lisp-user)

(require-apex-library "physob")

;;; (Derived from apexlib/human/human.lisp)
;;; Robot agents couple the generic agent architecture for action
;;; selection with the robot-specific resource-architecture.  This
;;; file includes functions for creating robot-agents.

;;; -------------------------------------------------------------------------
;;; === Robot

;;; A standard robot uses all available resource components including
;;; two hand components.  As additional resource types are
;;; constructed, these will also be incorporated.  If variants of some
;;; resource types are available, the standard robot will incorporate
;;; the one viewed as most complete (though perhaps less efficient
;;; than others)

;;; A standard robot is kind of agent.  Every agent has certain
;;; properties; a robot agent adds properties particular to the robot
;;; resource architecture.

(defclass robot (agent physob)
  (
   (velocity :accessor velocity :initarg :velocity :initform 10)
   (max-velocity :accessor max-velocity :initarg :max-velocity :initform 10)
   (min-velocity :accessor min-velocity :initarg :min-velocity :initform 10)
   (rotation-speed :accessor rotation-speed :initarg :rotation-speed :initform 10) ;; deg/sec
   (max-rotation-speed :accessor max-rotation-speed :initarg :max-rotation-speed :initform 10) ;; deg/sec
   (min-rotation-speed :accessor min-rotation-speed :initarg :min-rotation-speed :initform 10) ;; deg/sec
   (external-event :accessor external-event :initarg :external-event)
   (goal-location :accessor goal-location :initarg :goal-location)
 
;;; Note that direction refers to the direction of translation, whereas heading
;;; refers to the orientation of the robot body.
;;; Both are measured clockwise from north (robot-centric x and world-centric y).

   (goal-heading :accessor goal-heading :initarg :goal-heading) ;; orientation
   (direction :accessor direction :initarg :direction) ;; of travel
   (moving? :accessor moving? :initarg :moving? :initform nil)
   (turning? :accessor turning? :initarg :turning? :initform nil)))

;;; ! Repeat of hack from human.lisp (documented there)  - KMD
(defmethod pos ((x robot))
  (slot-value x 'pos))

(defmethod containing-object ((x robot)) ; -> Locale + Application
  (or (locale x) *application*))

(defmethod update-vertices ((x robot))
  (setf (vertices x) 
	(rotate-vertices (heading x) 
			 (ref->vertices (dimensions x) (pos x)))))

;; This rotates faces about the origin
;(defmethod update-faces ((x robot))
;  (setf (faces x) 
;	(rotate-faces (heading x)
;		      (ref->faces (dimensions x) (pos x)))))

;; This rotates them about the location of the robot
(defmethod update-faces ((x robot))
  (let ((rotated-faces 
	 (rotate-faces (heading x)
		       (ref->faces (dimensions x) '(0 0 0))));; about origin
	(location (location x)))
    (setf (faces x) 
	(loop for face in rotated-faces collect
	      (loop for (x y z) in face collect 
		    `(,(+ x (first location)) ,(+ y (second location)) ,z))))))

(defclass mother-ship (robot)
  ((heading :accessor heading :initarg :heading :initform 0)
   (external-event :accessor external-event :initarg :external-event)))

(defmethod assemble ((mother-ship-1 mother-ship) &key component-of &allow-other-keys)
  (setx (vertices mother-ship-1)
	(ref->vertices 
	 (dimensions mother-ship-1) (pos mother-ship-1)))
  (setx (faces  mother-ship-1) (ref->faces (dimensions  mother-ship-1) (pos  mother-ship-1)))
  (add-resource mother-ship-1 (make-instance 'radios :name 'radios))
  (add-resource mother-ship-1 (make-instance 'thrusters :name 'thrusters :cross-axis 2))
  (add-resource mother-ship-1 (make-instance 'external-event))
  (asamain mother-ship-1) ;; do initial expansion of tasks
  mother-ship-1 
  )

(defclass aur (robot)
  ((heading :accessor heading :initarg :heading :initform 0)
   (external-event :accessor external-event :initarg :external-event)
;;; Used in the movement activity
   (on-surface? :accessor on-surface? :initarg :on-surface? :initform nil)))

(defmethod assemble ((aur-1 aur) &key component-of &allow-other-keys)
  (setx (vertices aur-1) (ref->vertices (dimensions aur-1) (pos aur-1)))
  (setx (faces aur-1) (ref->faces (dimensions aur-1) (pos aur-1))) 
  (add-resource aur-1 (make-instance 'vision :name 'camera))
  (add-resource aur-1 (make-instance 'gaze :name 'gaze))
  (add-resource aur-1 (make-instance 'gripper :name 'left-gripper))
  (add-resource aur-1 (make-instance 'gripper :name 'right-gripper))
  (add-resource aur-1 (make-instance 'memory :name 'memory))
  (add-resource aur-1 (make-instance 'radios :name 'radios))
  (add-resource aur-1 (make-instance 'microphone :name 'microphone))
  (add-resource aur-1 (make-instance 'external-event))
  (add-resource aur-1 (make-instance 'thrusters :name 'thrusters :vertical 2 :cross-axis 2))
  (asamain aur-1) ;; do initial expansion of tasks
  aur-1 
  )

