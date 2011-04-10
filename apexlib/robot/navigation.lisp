;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Robot navigation
;;; apex/apexlib/robot/navigation.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: navigation.lisp,v 1.11 2006/01/15 03:42:51 dalal Exp $

;;;
;;; 3D Nav with thrusters
;;;

(in-package :user)

(require-apex-library "visob")


;;;--------------------------------------------------------------------------
;;; The Thruster Resource
;;;--------------------------------------------------------------------------


(defclass thrusters (physob resource)
  ((horizontal :accessor horizontal :initarg :horizontal :initform 2)
   (vertical :accessor vertical :initarg :vertical :initform 0)
   (cross-axis :accessor cross-axis :initarg :cross-axis :initform 0)))

;;;
;;; Movement -- basically moves in 3D coordinate space
;;;

(defparameter *places*
    '((quiescent-place1 '(10 10 100))
      (galapagos-rift (120 40 70))))

(defun find-place (goal-loc)
  (loop for (place loc) in *places*
      if (equal loc goal-loc)
      return place))

(defun find-coords (goal-place)
  (second (assoc goal-place *places*)))

(defparameter *heading-epsilon* 1.5) ;; degrees
(defparameter *position-epsilon* 1.0) ;; meters

(defun within-hdg (c-hdg g-hdg)
  (and (<= (abs (- g-hdg c-hdg)) *heading-epsilon*)))

(defun distance-3d (loc1 loc2)
  (sqrt (+ (expt (- (first loc1)(first loc2)) 2)
	   (expt (- (second loc1)(second loc2)) 2)
	   (expt (- (third loc1)(third loc2)) 2))))

(defun within-3d (loc1 loc2 epsilon)
  (<= (distance-3d loc1 loc2) epsilon))

(defun within (p1 p2 epsilon)
  (<= (sqrt (+ (expt (- (first p1) (first p2)) 2) 
	       (expt (- (second p1) (second p2)) 2)))
	    epsilon))

(defun compute-target-direction (from-loc to-loc)
     (let* ((diff-vector `(,(- (first to-loc)(first from-loc))
 			  ,(- (second to-loc)(second from-loc))))
 	   (dir-rads (if (not (= (first diff-vector) 0))
 			 (atan (/ (second diff-vector) (first diff-vector)))))
 	   (dir-degrees (if dir-rads
 			    (abs (rad->deg dir-rads)))))
       (cond ((= (first diff-vector) 0) ;; straight up
 	     0.0)
 	    ((= (second diff-vector) 0) ;; right
 	     90.0)
 	    ((and (> (first diff-vector) 0) (> (second diff-vector) 0)) ;; up right
 	     (- 90.0 dir-degrees))
 	    ((and (> (first diff-vector) 0) (< (second diff-vector) 0)) ;; down right
 	     (+ 90.0 dir-degrees))
 	    ((and (< (first diff-vector) 0) (< (second diff-vector) 0)) ;; down left
 	     (- 270.0 dir-degrees))
 	    ((and (< (first diff-vector) 0) (> (second diff-vector) 0)) ;; up left
 	     (+ 270.0 dir-degrees)))))

;;Heading increment is determined by the rotation speed and the update-interval.
(defun compute-heading (cur-hdg goal-hdg ang-v update-interval)
  (let* ((hdg-diff (- goal-hdg cur-hdg))
	 (ahdg-diff (abs hdg-diff))
	 (delta-a (* ang-v (/ update-interval 1000.0))))
    ;; make the smallest turn
    (if (<= ahdg-diff 180.0)
	(+ cur-hdg (* (signum hdg-diff) delta-a)) 
      (+ cur-hdg (* -1 (signum hdg-diff) delta-a)) ;; turn the other way
      )))

;; z-angle is 0 to -+ 90 degrees from the world-y axis

(defun compute-z-angle (from-loc to-loc)
  (let* ((x-y-resultant (distance from-loc to-loc))
	 (diff-vector `(,x-y-resultant
			,(- (third to-loc)(third from-loc))))
	 (dir-rads (if (not (= (first diff-vector) 0))
		       (atan (/ (second diff-vector) (first diff-vector)))))
	 (dir-degrees (if dir-rads (rad->deg dir-rads))))
    (cond ((and (= (first diff-vector) 0)(> (second diff-vector) 0)) ;; straight down
	   90.0)
	  ((and (= (first diff-vector) 0)(<= (second diff-vector) 0))
	   -90.0)
	  (t dir-degrees))))

;; Location increment is determined by the velocity and the update-interval.
;; Velocity is in meters per sec, interval is in milliseconds.
;; No-x-y allows for z-only travel, with a check to ensure
;; the vehicle doesn't go above the surface.

(defun compute-next-location (cur-loc no-x-y heading z-angle velocity interval)
  (let* ((step (* velocity (/ interval 1000.0))) ;; length of the direction vector
	 (x-step (* step (sin (deg->rad heading))))
	 (y-step (* step (cos (deg->rad heading))))
	 (z-step (* step (sin (deg->rad z-angle))))
	 (z-loc (+ (third cur-loc) z-step))
	 )
    (if (not no-x-y)
	`(,(+ (first cur-loc) x-step) ,(+ (second cur-loc) y-step) 
					  ,(if (< z-loc 0) 0.0 z-loc))
      `(,(first cur-loc) ,(second cur-loc)
					  ,(if (< z-loc 0) 0.0 z-loc)))))

;;; The movement activity handles both translation and
;;; rotation.  It's purpose is to bring the robot's location
;;; and heading within the goal location and heading. Note that
;;; direction refers to the direction of translation, whereas heading
;;; refers to the orientation of the robot body.

(defclass movement (activity) 
  ((locomotion-resource :accessor locomotion-resource :initarg :locomotion-resource)
   ))

;; Copied from another lib, this method just checks to see
;; if another like activity is in progress.

(defmethod initialize-activity :before ((act movement) (robot aur))
  (let* ((resource (locomotion-resource act))
	 (old-activity 
	  (find-if 
	   #'(lambda (a) (and (typep a ' movement)
			      (not (equal a act))))
	   (activities robot))))
    (when (and old-activity (equal (locomotion-resource old-activity) resource))
      (stop-activity old-activity)
      (cogevent `(clobbered ,old-activity :by ,act) 
		robot))
    (if (goal-location robot)
	(setf (direction robot)
	(compute-target-direction (location robot) (goal-location robot))))))

;; Whenever the robot's current location or heading is significantly different
;; from its goal location or heading, movement will occur.

;; Turning: we use the robot's current and goal headings and its rotation-speed
;; and the update-interval for the movement activity to compute the
;; new heading.

;; Translating: Using the robot's current and goal locations we compute a angles
;; (upward movement) and a direction.  Using these, the robot's current speed and the activity's
;; update-interval, we compute the next location, checking to see when we're
;; there. 

;; Set speeds ramps the robot to its max speed until it
;; Gets close, then ramps down until it's with the epsilons.
;; The achievable accuracy is probably unrealistic in the face
;; of turbulence.

(defun set-speeds (robot update-interval)
  (let* ((cur-hdg (heading robot)) (goal-hdg (goal-heading robot))
	 (cur-loc (location robot)) (goal-loc (goal-location robot))
	 (max-speed (max-velocity robot))(min-speed (min-velocity robot))
	 (max-ang-v (max-rotation-speed robot)) (min-ang-v (min-rotation-speed robot))
	 (max-delta-a (* max-ang-v (/ update-interval 1000.0)))
	 (min-delta-a (* min-ang-v (/ update-interval 1000.0)))
	 (hdg-diff (- goal-hdg cur-hdg))
	 (ahdg-diff (abs hdg-diff))
	 (max-step (* max-speed (/ update-interval 1000.0)))
	 (min-step (* min-speed (/ update-interval 1000.0)))
	 (one-step-to-target (distance-3d cur-loc goal-loc))
	 )
    (setf (velocity robot) 
      (cond ((<= max-step one-step-to-target) max-speed)
	    ((<= min-step one-step-to-target) min-speed)
	    (t (/ update-interval 1000.0))))
    (setf (rotation-speed robot) 
      (cond ((<= max-delta-a ahdg-diff) max-ang-v)
	    ((<= min-delta-a ahdg-diff) min-ang-v)
	    (t (/ update-interval 1000.0))))))
    
(defun update-heading (robot interval)
  (let ((ang-v (rotation-speed robot))
	(cur-hdg (heading robot))
	(goal-hdg (goal-heading robot)))
    (cond 
     ;; Turning and got there
     ((and (within-hdg cur-hdg goal-hdg)
	   (turning? robot))
      (print (list "At heading " (heading robot) (goal-heading robot)))
      ;; Always say that we're 
      (cogevent `(at-heading ,robot ,goal-hdg) robot :trigger-asa t)      
      (setf (turning? robot) nil))
     ;; Still turning
     (t	     
      (setf (turning? robot) t)
      (setf (heading robot) (compute-heading cur-hdg goal-hdg ang-v interval))      
      (update-vertices robot)(update-faces robot)
      ))
    ))

(defun update-location (robot interval)
  (let ((velocity (velocity robot))
	(cur-loc (location robot))
	(goal-loc (goal-location robot))
	place)
    (cond
     ;; Moving and got there
     ((and (within-3d cur-loc goal-loc *position-epsilon*)
	   (moving? robot))
      (print "Arrived.")
      ;; Always say that we're near the coords
      (cogevent `(near ,robot ,goal-loc) robot :trigger-asa t)
      (if (setf place (find-place goal-loc))
	  (cogevent `(at ,robot ,place) robot :trigger-asa t)
	(if (<= (abs (- (third (location robot)) 0)) *position-epsilon*)
	    (cogevent `(at ,robot surface) robot :trigger-asa t)
	  (cogevent `(at ,robot unknown) robot :trigger-asa t)))
      (setf (moving? robot) nil))
     ;; Still moving 
     (t	     
      (setf (moving? robot) t)
      (setf (direction robot) 
	(compute-target-direction cur-loc goal-loc)
	(location robot) 	    
	(compute-next-location
	 cur-loc
	 (within cur-loc goal-loc *position-epsilon*) ;; just an x-y check
	 (direction robot) 
	 (compute-z-angle cur-loc goal-loc)
	 velocity interval)
	(pos robot) (location robot))
      ))))

(defmethod update-activity ((act movement) (robot aur))
  (let ((cur-loc (location robot))
	(goal-loc (goal-location robot))
	(cur-hdg (heading robot))
	(goal-hdg (goal-heading robot)))
    (set-speeds robot (update-interval act))
          ;; not on surface
    (cond ((and (on-surface? robot)
		(not (= (third (location robot)) 0)))
	   (setf (on-surface? robot) nil)
	   (print "Not on Surface.")
	   )
	  ;; On-surface
	  ((and (not (on-surface? robot))
		(= (third (location robot)) 0))
	   (setf (on-surface? robot) t)
	   (cogevent `(at ,robot surface) robot :trigger-asa t)
	   (print "On surface.")
	   )
	  ;; Do nothing (station-keep)
	  ((and (within-3d cur-loc goal-loc *position-epsilon*)
		(not (moving? robot))
		(within-hdg cur-hdg goal-hdg)
		(not (turning? robot)))
	   (print (list "Station-keeping.")))
	  ;; Rotating
	  ((and (within-3d cur-loc goal-loc *position-epsilon*)
		(not (moving? robot))
		(or (not (within-hdg cur-hdg goal-hdg))
		    (turning? robot)))
	   (update-heading robot (update-interval act)))
	  ;; Translating
	  ((and (within-hdg cur-hdg goal-hdg)
		(not (turning? robot))
		(or (not (within-3d cur-loc goal-loc *position-epsilon*))
		    (moving? robot)))
	   (update-location robot (update-interval act)))
	  ;; Both
	  (t
	   (update-heading robot (update-interval act))
	   (update-location robot (update-interval act))))
;;; Used for the diagram views
    (update-vertices robot)(update-faces robot)
;;; Output progress data
    (format t "~%               Location             Heading Vel(m/s) Angular Vel(d/s)")
    (format t "~%Current: ~6,2F , ~6,2F , ~6,2F , ~6,2F, ~6,2F, ~6,2F"
	    (first (location robot)) (second (location robot)) (third (location robot))
	    (heading robot)(velocity robot)(rotation-speed robot))
    (format t "~% Target: ~6,2F , ~6,2F , ~6,2F , ~6,2F~%~%"
	    (float (first (goal-location robot))) (float (second (goal-location robot))) 
	    (float (third (goal-location robot)))
	    (float (goal-heading robot)))
    ))

