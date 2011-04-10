;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/kitchen-definitions.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: kitchen-definitions.lisp,v 1.8 2006/01/15 03:42:57 dalal Exp $

(in-package :user)

;;; ---------------------------------------------------------------------
;;; WALTERS WORLD - Kitchen object definitions

;;; -- Pan --------------------------------------------------------

;;; Pans are distinct from other physobs in their
;;; appearance and mass.  They have no distinctive state attributes 
;;; and do not need any specific assemble method.

(defclass pan (physob)
  ((mass :initform 5) 
   ;;subclasses can overwrite defaults; so can make-instance 
   (temp :initform 0)
   (shape :initform '(pan)) 
   ;;object can have a multiple shape descriptions
   (color :initform 'grey)))

;;; -- Counter ------------------------------------------------------

;;; Counters are distinct from other physobs only
;;; in their appearance.  They have no distinctive state attributes. 
;;; However, they do need some "assembly" to specify vertices based 
;;; on input dimensions; the assemble method automatically uses the 
;;; (given) refpos value and the (derived) vertices to compute a value 
;;; for the pos (centroid) attribute.  The assembly method also makes 
;;; the kitchen-counter visible within its locale by adding it to that
;;; locales vfield (visual-field).

(defclass kitchen-counter (physob)
  ((shape  
    :initform '(rectangle counter))
   (dimensions  
    :accessor dimensions  
    :initarg 
    :dimensions)))

(defmethod assemble ((kitchen-counter-1 kitchen-counter) 
                     &key component-of)
  (setx (vertices kitchen-counter-1)
	(generate-rectangular-vertices (dimensions kitchen-counter-1)))
  (setx (pos kitchen-counter-1) (refpos->pos kitchen-counter-1))
  kitchen-counter-1)  
;; assemble method must always return object

;;; --- Stovetop --------------------------------------------------------

;;; A stovetop is more complicated than either of the previous items, 
;;; mainly because it has many components (burners and dials).  The 
;;; assembly method for a stovetop is therefore more elaborate.

(defclass stovetop (physob)
  ((shape 
    :initform '(rectangle stovetop))
   (dimensions 
    :accessor dimensions 
    :initarg 
    :dimensions)
   ;; the size of the stovetop is used to init its appearance   
   (widgetspecs 
    :accessor widgetspecs 
    :initarg 
    :widgetspecs)
   ;; burner/dial specifications incl. name, size, pos   
   (burners 
    :accessor burners 
    :initarg 
    :burners)
   ;; list of the burner physobs mounted on the stovetop
   (dials 
    :accessor dials 
    :initarg 
    :dials)))
   ;; list of the dials (burner controls) on the stovetop

;;; A stovetop is a surface containing a set of burners and dials. 
;;; Number, size, and relative positions must be specified so that an 
;;; assemble method can create these "widgets."  The Jennair20 
;;; configuration below specifies 4 dials/burner pairs: 
;;;   (<burnername> <diameter> <pos> <dialname> <pos> <numsettings>).
;;; Positions are with respect to the refpos (lower left corner) of 
;;; the stovetop surface.  Measurements are in centimeters.

;;; Note on cooking.  Temperatures of all objects range from 0 (room 
;;; temp) to 5 (very hot).  The stovetop dials have 6 positions (0-5) 
;;; causing a burner to reach a max temperature equal to its dial 
;;; setting. 

(defconstant jennair20
    '((front-left-burner 20 (17 20) front-left-dial (5 4) 6)  
      ;; first burner/dial
      (back-left-burner 15 (17 42) back-left-dial (12 4) 6)  
      ;;  second...
      (front-right-burner 15 (60 20) front-right-dial (62 4) 6) 
      (back-right-burner 20 (60 42) back-right-dial (72 4) 6)))

;;; Note that the method for assembling an object with components 
;;; resembles the initialize method for the whole scenario.  

(defmethod assemble ((stove-1 stovetop) &key component-of)
  (let ((loc (locale stove-1)))
    (setx (vertices stove-1)
	  (generate-rectangular-vertices (dimensions stove-1)))
    (setx (pos stove-1) (refpos->pos stove-1))
    (dolist (spec (widgetspecs stove-1))  
      ;;go through each line of jennair specs
      (let* ((dial (make-instance 'dial 
                     :name (fourth spec)
                     :component-of (component-set stove-1)
                     :numsettings (sixth spec) 
                     :locale loc
                     :pos (add-coordinates (fifth spec) (pos stove-1))))
	     (burner (make-instance 'burner 
                       :name (first spec)
                       :component-of (component-set stove-1)
                       :control dial 
                       :locale loc
                       :pos (add-coordinates (third spec) 
                                             (pos stove-1)))))
	(setx (device dial) burner)  
        ;; the dial controls the burner device
	(push burner (components (component-set stove-1)))
	(push dial (components (component-set stove-1)))
	(assemble dial :component-of (component-set stove-1))
	(assemble burner :component-of (component-set stove-1))))))

;;; ---- Burner ------------------------------------------------------

;;; TEMPerature is another important attribute of a burner, but this is
;;; inherited (with default value 0 as room temp) from physob.  When its
;;; associated dial is turned to a non-0 setting, a burner becomes a
;;; heatsource.  However, the dial change only affects the burner state
;;; indirectly via the adjust-temperature activity that affects all 
;;; physobs. 

(defclass burner (physob)
  ((control 
    :accessor control 
    :initarg 
    :control)
    ;control dial
   (mass 
    :initform 2)
   (temp 
    :initform 0)
   (shape 
    :initform '(circle burner))
   (color 
    :initform 'black)))

;;; heat source maxtemp equals the dial setting
(defmethod heatsource-maxtemp ((obj burner))
  (setting (control obj)))

;;; determines whether a burner is currently acting as a heat source
(defmethod heatsource-p ((obj burner))
  (> (setting (control obj)) 0))

;;; -- Dial --------------------------------------------------------------

;;; Dials are distinguished from other physobs by their appearance, by 
;;; the kind of action (turning) used to change their setting, and by the
;;;  state parameter SETTING.

(defclass dial (physob)
  ((setting 
    :accessor setting 
    :initarg 
    :setting 
    :initform 0) 
   ;; represents the value to which the dial is currently pointing
   (numsettings 
    :accessor numsettings 
    :initarg 
    :numsettings)
   (device 
    :accessor device 
    :initarg 
    :device 
    :initform nil)
   ;; the device that the dial controls
   (aspect 
    :accessor aspect 
    :initarg 
    :aspect 
    :initform nil)
   ;; aspect of the device function (e.g volume) affected by dial setting
   ;; if nil, then dial setting effect is passive and usually delayed
   (shape 
    :initform '(circle dial))
   (color 
    :initform 'black)))

;;; When the dial is created, its appearance needs to be brought into
;;; correspondence with its internal state.

(defmethod assemble ((dial-1 dial) &key component-of)
  (set-dial-orientation dial-1) 
  dial-1)

;;; sets the orientation of a dial based on on its current setting and
;;; the number of settings available.  Assumes the first setting is 0 and
;;; that all are equally spaced.

(defun set-dial-orientation (dial)
  (setx (orientation dial) (* (+ 1 (setting dial)) 
			      (/ 360 (numsettings dial)))))

;;; --- Turn dial ---------------------------------------------------

;;; Causes change to dial setting (internal state), orientation
;;; (appearance) and possibly initiates a change in a device connected
;;; to the dial.  This is more general than it needs to be for
;;; controlling a stove burner since the dial's effect on a burner is
;;; passive -- there is no immediate effect of turning the dial.

(defmethod turned ((dial-1 dial) new-setting)
  (setx (setting dial-1) new-setting)
  (set-dial-orientation dial-1)
  ;;relation between orientation and dial setting is specific to
  ;;particular dials.. need to generalize
  (if (aspect dial-1)
      (signal-event 
       (changed-control-setting (device dial-1) (aspect dial-1) 
				(setting dial-1)))))

;;;  ------ Measuring Cup -----------------------------------------------

(defclass measuring-cup (physob)
  ((fill-level 
    :accessor fill-level 
    :initarg 
    :fill-level 
    :initform 0)
   (full 
    :accessor full 
    :initarg 
    :full 
    :initform 100)
   (shape 
    :accessor shape 
    :initform `(empty-cup))))

(defmethod fill-one-scoop ((c measuring-cup))
  (progn
    (setf (fill-level c) (+ (fill-level c) (normRand 15 5)))
    (if (>= (fill-level c) (full c))
	(setx (shape c) `(filled-cup)))))

;;; -------- Microwave -----------------------------------------

(defclass microwave (physob)
  ((plugged-in 
    :accessor plugged-in 
    :initarg 
    :plugged-in)
   (power-setting 
    :accessor power-setting 
    :initform 10)
   (food-to-cook 
    :type food 
    :accessor food-to-cook  
    :initform nil)
   (time-left 
    :accessor time-left 
    :initform -1)
   (needs-hit 
    :accessor needs-hit 
    :initarg :needs-hit)
   (shape 
    :initform '(microwave))))

(defmethod assemble ((m microwave) &key component-of)
  (progn
    (setf (plugged-in m) (random 2))
    (if (= (plugged-in m) 0)
	(setf (plugged-in m) nil))
    (setf (needs-hit m) (random 2))
    (if (= (needs-hit m) 0)
	(setf (needs-hit m) nil))
    (start-activity m `microwave-cooking :update-interval 100)))

(defmethod put-in ((m microwave) (f food))
  (progn 
    (setf (food-to-cook m) f)
    (inform `(put-in ,f ,m) 
            :router *walter-router*)))

(defmethod hit-microwave ((m microwave))
  (setf (needs-hit m) nil))

(defmethod take-out ((m microwave) (f food))
  (progn
    (setf (food-to-cook m) nil)
    (inform `(taken-out ,f ,m) 
            :router *walter-router*)))

(defmethod plug-in ((m microwave))
  (setf (plugged-in m) 1))

(defmethod change-power-setting ((m microwave) setting)
  (if (and (plugged-in m) (not (needs-hit m)))
      (progn
	(setx (power-setting m) (min setting 10)) ;;10 is highest setting
	(inform `(power-setting ,m ,setting) 
                :router *walter-router*))
    (inform '(microwave-unresponsive) 
            :router *walter-router*)))

(defmethod set-time ((m microwave) time-to-set)
  (if (and (plugged-in m) (not (needs-hit m)))
      (progn (setf (time-left m) time-to-set)
	     (inform `(set-time ,m ,time-to-set) 
                     :router *walter-router*))
    (inform '(microwave-unresponsive) 
            :router *walter-router*)))

;;; ---- Microwave activities ------------

(defclass microwave-cooking (activity) ())

(defmethod update-activity ((act microwave-cooking) (ob appob))
  (let* ((m (primary-object act))
         (interval (update-interval act))
         (f (food-to-cook m)))
    (if (and (plugged-in m) (/= (time-left m) -1) (not (null f)) 
             (not (needs-hit m)))
	(let* ((cookspeed (* .1 (power-setting m))) 
	       (cookstate (cookstate f))
	       (max-increment   
                ;; how much more cooked it will get this update
		(* cookspeed (/ interval (* 3000 (cookrate f)))))
	       (new-cookstate (min 9.0 (+ max-increment cookstate )))
	       ) ;; can't get more cooked than 9.0 (burnt badly) 
	  (when (> new-cookstate cookstate)
	    (setx (cookstate f) new-cookstate)
	    (signal-event (cookstate-changed f)))
          ;; ! hack for faster run: the (min 1000 ...) wrapper below
	  (setx (time-left m) (min 1000 (max 0 (- (time-left m) interval))))
	  (when (= (time-left m) 0)
	    (inform '(microwave-beep) :router *walter-router*)
	    (setf (time-left m) -1))))))

;;;  -------  Dish -------------------------------------------

(defclass dish (physob)
  ((state 
    :accessor state 
    :initarg 
    :state 
    :initform '(dirty dry))
   (shape 
    :accessor shape 
    :initform '(dish) 
    :initarg 
    :shape)))

;;;  -------  Trashcan -----------------------------------------

(defclass trashcan (physob)
  ((state 
    :accessor state 
    :initarg 
    :state :
    initform '(no-bag)) ; can be no-bag or has-bag
   (bag 
    :accessor bag 
    :initarg 
    :bag 
    :initform nil)
   (shape 
    :accessor shape 
    :initform '(trashcan) 
    :initarg 
    :shape)))

(defclass trashbag (physob)
  ((shape 
    :accessor shape 
    :initarg 
    :shape 
    :initform '(in-box)) ; can be in-box, in-can or thrown-out
   (contents 
    :accessor contents 
    :initarg 
    :contents 
    :initform nil)
   (full 
    :accessor full 
    :initform nil 
    :initarg full)))

;;;  -------  Drawer -------------------------------------------
;;; NOTE:  All contents of drawers must have an isa slot for the
;;; show-contents function to work.

(defclass drawer (physob)
  ((contents 
    :accessor contents 
    :initarg 
    :contents 
    :initform nil)
   (shape 
    :accessor shape 
    :initarg 
    :shape 
    :initform '(closed-drawer))))

;;; -------  Vase And Other stuff ------------------------------

(defclass vase (physob)
  ((flower 
    :type flower 
    :accessor flower 
    :initform nil)
   (isa 
    :accessor isa 
    :initform 'vase 
    :initarg 
    :isa)))

(defclass bottle-opener (physob)
  ((isa 
    :accessor isa 
    :initform 'bottle-opener 
    :initarg 
    :isa)))

(defclass rubber-band (physob)
  ((isa 
    :accessor isa 
    :initform 'rubber-band 
    :initarg 
    :isa)))

(defclass mustard-spoon (physob)
  ((isa 
    :accessor isa 
    :initform 'mustard-spoon 
    :initarg 
    :isa)))

;; --- big soup pot ---------------------------------------------

(defclass pot (physob)
  ((temp 
    :initform 0)
   (mass 
    :initform 8)
   (shape 
    :initform '(pot))
   (contents 
    :accessor contents 
    :initarg 
    :contents 
    :initform nil)
   (isa 
    :accessor isa 
    :initform 'pot 
    :initarg 
    :isa)))

;;;  -------- Cook Book-----------------------------------------
;;; A cookbook is a collection of recipes, which are
;;; in turn a collection of instructions.  

(defclass instruction (appob)
  ((directive 
    :accessor directive 
    :initarg 
    :directive)))

(defclass recipe (appob)
  ((name 
    :accessor name 
    :initarg 
    :name)
   (instructions 
    :type list 
    :accessor instructions 
    :initarg 
    :instructions)
   (current-instruction 
    :accessor current-instruction 
    :initform 0)
   (ingredients 
    :type list 
    :accessor ingredients 
    :initarg 
    :ingredients)))

(defclass cookbook (physob)
  ((recipes 
    :accessor recipes 
    :initarg 
    :recipes 
    :type list)
   (shape 
    :initform '(cookbook))
   (title 
    :accessor title 
    :initarg 
    :title)
   (isa 
    :accessor isa 
    :initform 'cookbook 
    :initarg 
    :isa)))

;;;  -------  Watch ----------------------------------------------
;;; The watch is a remnant from the class project, which we used
;;; to signal to the cook that he should check on the egg, but 
;;; this can probably be done better.

(defclass watch (physob)
  ((currentTime 
    :accessor currentTime 
    :initarg 
    :currentTime)
   (alarmTime 
    :accessor alarmTime 
    :initform 0)
   (shape 
    :initform '(watch))))

(defmethod assemble ((w watch) &key component-of)
  (start-activity w 'adjusting-time :update-interval 1000))

(defclass adjusting-time (activity) ())

(defmethod update-activity ((act adjusting-time) (ob appob))
  (let ((watch (primary-object act)))
    (setf (currentTime watch) (round (/ (current-time) 1000)))
    (setf (alarmTime watch) *globalAlarmTime*)
    (if (equal (currentTime watch) (alarmTime watch))
        (inform `(alarm-rings) 
                :router *walter-router*)
      nil)))

;; ----------------------------------------------------

(defvar *temp*)                         ; for debugging


