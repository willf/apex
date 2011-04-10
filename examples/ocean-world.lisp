;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/ocean-world.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: ocean-world.lisp,v 1.22 2006/01/15 03:42:52 dalal Exp $

;;;
;;; AUR (Autonomous Underwater Robot) World
;;;

;;; This simworld will eventually illustrate an aur with two arms and a
;;; variety of sensors.

;;; 02/08/05 When you run this file, the mother-ship, on the surface,
;;; will initialize and listen for communications from the aur. The aur
;;; will initialize then travel to a home position and wait for the
;;; eruption of a thermal vent.  After sensing a vent, the aur will
;;; travel to the vent site, take data, then up to the surface to report
;;; it's findings to the mother ship.  Finally it will return home.

(in-package :user)

;;;
;;; ---- Application definition
;;;

(defapplication "Ocean World" :init (initialize-ocean))
(require-apex-library "robot") ;; loads libs from apexlib and apexlib/robot

;;; Modified from visob-apexlib.lisp to handle vents.

(defconstant given-vis-attribute-types    
    '(location pos color orientation contrast texture shape blink 
      contains elements))

;; Currently only using vis and comm fields
(defparameter *vis-field* (make-instance 'ps-router :name 'vis-field))
(defparameter *aud-field* (make-instance 'ps-router :name 'aud-field))
(defparameter *comm-field* (make-instance 'ps-router :name 'comm-field))

(defparameter *debug-ob* nil "Cache for objects to inspect.")

;;;
;;; An object to track
;;;

(defclass thermal-vent (physob)
  ((location :accessor location 
             :initarg :location)
   (erupting? :accessor erupting?
             :initarg :erupting? :initform nil)))

(defmethod containing-object ((x thermal-vent)) ; -> Locale + Application
  (or (locale x) *application*))

(defun initialize-ocean ()

  (initialize-physob-database)

  (let*
      ((ocean (make-instance 'locale :name 'ocean))
       ;;; location is inherited from agent
       ;;; pos is from visob-apexlib
       ;;; mother-ship and aur are subclasses of robot [apexlib/robot/robot.lisp]
       ;;; mother-ship is on the surface [z=0]
       (mother-ship (make-instance 'mother-ship 
		      :name 'poseidon
		      :location '(100 100 0) :pos '(100 100 0) :locale ocean
		      :routers (list *comm-field* *aud-field*)
		      :initial-task  '(initialize-and-listen)))
       ;;; autonomous underwater vehicle (aur) is below the surface [z=100]
       (aur (make-instance 'aur :name 'neptune :location '(50 50 100) 
			   :pos '(50 50 100) :heading 90.0
	;;		   :rotation-speed 20 :velocity 20.0
			   :locale ocean
			   :initial-task '(initialize-and-look)
			   :routers (list *comm-field* *aud-field* *vis-field*)
			   :use-bundles '(:aur)))
       ;; Want to eventually create these dynamically
       (vents (list
	       (make-instance 'thermal-vent :name 'galapagos1 :location '(125 50 70)
			      :pos '(125 50 70) :locale ocean
			      :shape '(tall-rectangle)
			      :color 'black
			      :orientation '(0 0)
			      :dimensions '(5 5 20)
			      ))))
    
    (setf *debug-ob* (list ocean aur mother-ship vents))

    ;; *places* is defined in apexlib/robot/navigation
    (setf *places* '((quiescent-place1 (10 10 100))
		     (galapagos-rift (120 40 70))))

    ;; among other things, assemble puts the objects and their components in the visual field
    ;; of the locale
    (assemble mother-ship)
    (assemble aur)

    ;; graphics for the diagram views
    (create-wireframe-graphical-object mother-ship :auto-update t)
    (create-wireframe-graphical-object aur :auto-update t)
    (setf (graphical-object ocean)
          (make-instance 'rect
                         :x 0 :y 0
                         :width 500 :height 500
                         :stroke "black" :fill "blue"))
    (loop for vent in vents
	do (assemble vent)
	   (setf (vertices vent)
	     (ref->vertices 
	      (dimensions vent) (pos vent)))
	   (setf (faces vent)
	     (ref->faces
	      (dimensions vent) (pos vent)))
	   (create-wireframe-graphical-object vent :auto-update t))
    
    ;; For the ramifications of these settings, see  apexlib/robot/navigation.lisp
    (setf (max-velocity aur) 30.0) ;; 30 meters per sec
    (setf (max-rotation-speed aur) 50.0) ;; 50 degrees per sec

    ;; Just a touch of some spatial logic
    (loop for (place coords) in *places*
	do
	  (assert-physob-relation `(place ,place)))

    ;; Always need a default goal location and heading
    ;; for station-keeping
    
    (setf (goal-location aur) (location aur)
	  (goal-heading aur) (heading aur)
	  (goal-location mother-ship) (location mother-ship)
	  (goal-heading mother-ship) (heading mother-ship))

    ;; activate vents 
    (loop for vent in vents
	do
	  (start-activity vent 'erupt 
			  :update-interval 100)
	  )
    
    ;; relations we want traced
    (show at)
    (show near)
    (show at-heading)))

(defun find-resource (agent resource-name)
  (loop for r in (resources agent)
      if (eq (name r) resource-name)
      return r))

;;; Communications
;;; Limiting factors for receivers: frequency and surface location.

(defun comms-subscriber? (subscriber attributes)
  (let ((freq (nth (1+ (position 'frequency attributes)) attributes))
	(subscriber-z (if (slot-exists-p subscriber 'location)
			  (third (location subscriber))))
	(subscriber-freq (frequency (find-resource subscriber 'radios)))
	ans)
    (setf ans
      (and (= freq subscriber-freq) ;; have to be on the same frequency
	   subscriber-z ;; subscriber even has a location
	   (<= (abs (- (third (location subscriber)) 0)) *position-epsilon*)))    ;; have to be on the surface
    (if (not ans) 
	(format t "~%Subscriber ~a is not capable of listening (z ~a,freq ~a)."
		subscriber subscriber-z subscriber-freq)
      (format t "~%Subscriber ~a is capable of listening (z ~a,freq ~a).~%"
		subscriber subscriber-z subscriber-freq))
      ans))

;; patterned after the inform function in system/foundation/pubsub.lisp
(defun comms-inform (eventform
               &key (router *default-ps-router*)
                    author suppress-log trigger-asa attributes)
  (type-check inform
    (eventform (list any))
    (router ps-router)
    (trigger-asa bool)
    (author (opt ps-mixin))
    (suppress-log bool)
    (attributes (list any)))		; ! improve this check
  (if (and (slot-exists-p author 'location)
	   (<= (abs (- (third (location author)) 0)) *position-epsilon*)) ;; sender is on the surface
      (dolist (subscriber (subscribers router))
	(when (and 
	       (or (null author)
		   (not (equal author subscriber)))
	       (comms-subscriber? subscriber attributes))
	  (deliver subscriber eventform
		   :suppress-log suppress-log
		   :trigger-asa trigger-asa
		   :attributes attributes)
	  ))
    (format t "~%Author ~a not on surface (z= ~a)." author (third (location author)))))

(defclass erupt (activity) 
  ((last-update-time :accessor last-update-time
             :initarg :location :initform 0)))

(defmethod initialize-activity :before ((act erupt)(vent thermal-vent))
  (let* ()
	(assemble vent)
	(setf (vertices vent)
		 (ref->vertices 
		  (dimensions vent) (pos vent)))))

;; All vents erupt at same time for now.

(defmethod update-activity ((act erupt)(vent thermal-vent))
  (when (> (- (current-time) (last-update-time act))
	     500)
    (if (not (erupting? vent))
	(setf (erupting? vent) t)
      (setf (erupting? vent) nil))
    (if (erupting? vent)
	(format t "~%++++++ Vent (~a) erupting." vent)
      (format t "~%------ Vent (~a) quiescent." vent))
    (setf (last-update-time act) (current-time))))
    
;;; ---------------------------------------------
;;; PROCEDURES
;;; ---------------------------------------------

;;; Procedures for Both Ships (*apex-bundle* = nil)

(procedure
 (index (do-domain)))

;;; Initialize starts the activities for the robot.

(procedure :sequential
 (index (initialize))
 (start-engines)
 (start-cameras)
 )

;;; The movement activity is defined in apexlib/robot/navigation.lisp.
;;; If the robot's location and heading are its goal-location and goal-heading, 
;;; it station-keeps, else it'll try to turn and move the robot to the position
;;; and orientation. Once there (within *position-epsilon* and *heading-epsilon*)
;;; it'll generate cogevents of (at +self+ ?place), (near +self+ "place),
;;; and (at-heading +self+ ?heading).

(primitive 
 (index (start-engines))
 (profile thrusters)
 (duration (100 ms))
 (on-completion
  (start-activity +self+ 'movement         
		  :locomotion-resource (find-resource +self+ 'thrusters)
		  :goal-location (location +self+) :update-interval 100)
  ))

;;; The seeing activity is defined in apexlib/robot/vision.lisp and is
;;; essentially the same as apexlib/human/vision.lisp

(primitive 
 (index (start-cameras))
 (profile cameras)
 (duration (100 ms))
 (on-completion
  (let ((camera-resource (find-resource +self+ 'camera)))
    (if camera-resource
	(start-activity +self+ 'seeing :vision-resource camera-resource
			:update-interval 50 :fullcycle 20)
      (format t "~%!!!! At start-cameras -- no camera-resource for ~a." +self+)))))

;;; We want the mother ship to listen for possibly multiple
;;; transmissions.  The responding clause allows that to happen,
;;; rebinding the sender and the body each time.

(procedure
 (index (listen))
 (step s1 (notify (i got ?body from ?from))
       (waitfor (transmission-from ?from ?body))
       (responding :for-new ?from ?body)))

(procedure :sequential
 (index (initialize-and-listen))
 (initialize)
 (listen))

;;; Since the mother-ship is still listening,
;;; here's a hack to free up apex after the aur returns to its nest.

(defun stopsim ()
  (loop for agent in *all-agents*
      do
	(loop for act in (activities agent)
	    do
	      (stop-activity act)))
  (loop for vent in (first (last *debug-ob*))
      do
	(loop for act in (activities vent)
	    do
	      (stop-activity act))))

(primitive 
 (index (stop-sim))
 (duration (2 ms))
 (on-completion
  (stopsim)
  ))

(primitive
 (index (send ?message))
 (profile radio)
 (duration (10 ms))
 (on-completion
  (comms-inform `(transmission-from ,+self+ ,?message)
		:router *comm-field* :author +self+ :trigger-asa t :attributes '(frequency 100.0))
  ))

(primitive
  (index (notify ?message))
  (duration (10 ms))
  (on-completion
   (format t "~%~%From ~a ~a.~%~%" +self+ ?message)))
 
;;;
;;; Procedures for the aur
;;;

(in-apex-bundle :aur)

(procedure :sequential
 (index (initialize-and-explore ?site))
 (initialize)
 (explore ?site))

;; In case we want to just test traveling to the surface
;; and reporting.

(procedure :sequential
 (index (initialize-and-uplink))
 (initialize)
 (uplink))


(procedure :sequential
	   (index (uplink ?site ?info))
	   (travel-to +self+ surface)
	   (send (I explored ?site)) 
	   (send (report --  ?info))
	   (send (signing off)))

;;; Drive around and uplink
(procedure :sequential
	   (index (explore ?site))
	   (travel-to +self+ quiescent-place1)
	   (travel-to +self+ ?site)
	   (uplink ?site (no-info))
	   (travel-to +self+ quiescent-place1)
	   (stop-sim))

;;; Cogevents for these procedures come from
;;; the seeing activity (see start-cameras primitive).

(procedure
 (profile cameras)
 (index (look-for-object ?color))
 (step s1 (notify (i saw ?color ?object1))
       (waitfor (color ?object1 ?color)))
 (step s2 (notify (i saw ?object1 at ?loc))
       (waitfor ?s1 (location ?object1 ?loc)))
 (step s3 (terminate >> ?loc)
       (waitfor ?s2)))

;;; This procedure waits for there to be
;;; an object at ?location before getting its shape.

(procedure
 (profile cameras)
 (index (get-object-shape ?location))
 (step s1 (notify (reacquired ?object1 at ?location))
       (waitfor (location ?object1 ?location)))
 (step s2 (notify (?object1 shape is ?shape))
       (waitfor ?s1 (shape ?object1 ?shape)))
 (step s3 (terminate >> (?shape ?object1))
       (waitfor ?s2)))

;;; Basically these primitives post a goal location,
;;; and a goal-heading, which, if the movement activity is ongoing
;;; (see the start-engines primitive), causes the robot to achieve
;;; the given location/heading. This could apply to any moving 'bot
;;; but the mother ship is just stationary on the surface right now.

(primitive 
;;; ?place can be a place or coords
 (index (head-for +self+ ?place))
 (duration (2 ms))
 (on-completion
  (print (list "At head for ..." ?place))
  (cond ((assoc ?place *places*)
         (setf (goal-location +self+) (second (assoc ?place *places*))))
	((eq ?place 'surface)
	 (setf (goal-location +self+)
	   `(,(first (goal-location +self+)) 
	     ,(second (goal-location +self+)) 
	     0))) ;; head straight up
	((= (length ?place) 3) ;; got coords
	 (setf (goal-location +self+) ?place))
	(t (format t "~%At head-for, bad place: ~a." ?place)))
  (setf (moving? +self+) t)
  ))

(primitive 
;;; ?place can be a place or coords or a direction angle
 (index (turn-to +self+ ?place))
 (duration (2 ms))
 (on-completion
  (let ((coords (cond ((assoc ?place *places*)
		       (second (assoc ?place *places*)))
		      ((eq ?place 'surface)
		       `(,(first (goal-location +self+)) 
			 ,(second (goal-location +self+)) 
			 0)) ;; head straight up
		      ((not (consp ?place))
		       nil)
		      ((= (length ?place) 3) ;; got coords
		       ?place)
		      (t (format t "~%At turn-to, bad place: ~a." ?place)
			 (location +self+)))))
    (if coords
	(setf (goal-heading +self+) 
	      (compute-target-direction (location +self+) coords))
      (setf (goal-heading +self+) ?place))
    (setf (turning? +self+) t)
    (format t "~%Turning to ~a (~a)." (goal-heading +self+) coords))))

;; A simple turn testor

(procedure 
 (index (turn-testor))
 (step (initialize))
 (step first (turn-to +self+ galapagos-rift))
 (step second (turn-to +self+ (100 100 0))
       (waitfor 
	(:in-order ?first (at-heading +self+ ?hdg1))))
 (step third (turn-to +self+ 100)
       (waitfor 
	(:in-order ?second (at-heading +self+ ?hdg2))))
 (step (stop-sim)
       (waitfor 
	(:in-order ?third (at-heading +self+ ?hdg3)))))

;;; ?Place can be a place or coords or a direction
;;; the "near" cogevent is generated from the movement activity
;;; (see the start-engines primitive). It will someday
;;; imply the at relation.

(procedure
 (index (travel-to +self+ ?place))
 (profile thrusters)
 (step s1 (turn-to +self+ ?place))
 (step s2 (head-for +self+ ?place))
 (step s3 (terminate) 
       (waitfor (:and (at +self+ ?place)(at-heading +self+ ?hdg))))
 (step s4 (terminate) 
       (waitfor (:and (near +self+ ?place)(at-heading +self+ ?hdg)))))

;;; Can use this to do specific navigations.

(procedure :sequential
	   (index (travel-test))
	   (initialize)
;	   (travel-to +self+ (125 50 70))				;
;	   (travel-to +self+ surface)
	   (travel-to +self+ quiescent-place1)
; 	   (travel-to +self+ ?location)
; 	   (travel-to +self+ quiescent-place1)
 	   (stop-sim)
 	   )

;; The main work for the aur.
;; For now, the aur is looking for blackness
;; in the water column.  When it finds it,
;; it moves to that place and records data on the vent.
;; Then it travels to the surface to report the info,
;; then returns home. Eventually, this will be a recurring task.


(procedure :sequential
	   (index (initialize-and-look))
	   (initialize)
	   (travel-to +self+ quiescent-place1)
 	   (look-for-object black => ?location)
 	   (notify (heading for ?location))
 	   (travel-to +self+ ?location)
 	   (get-object-shape ?location => (?shape ?object))
 	   (uplink ?location (object ?object color black shape ?shape))
 	   (travel-to +self+ quiescent-place1)
 	   (stop-sim)
 	   )

;;; helper functions for debugging

(defun rep (reps) (loop for i from 1 to reps do (stepapp)))
(defun rdata ()
  (print (list "loc, goal loc" 
	       (location (second *debug-ob*)) (goal-location (second *debug-ob*))
	       (distance-3d (goal-location (second *debug-ob*)) (location (second *debug-ob*)))))
  (print (list "hdg, goal hdg"
	       (heading  (second *debug-ob*))  (goal-heading  (second *debug-ob*))
	       (abs (- (goal-heading  (second *debug-ob*))
		       (heading  (second *debug-ob*)))))))

