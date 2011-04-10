;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/kitchen-world.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: kitchen-world.lisp,v 1.12 2006/01/15 03:42:52 dalal Exp $


(in-package :common-lisp-user)

(defapplication "Kitchen World"
    :init (initialize-kitchen))

(require-apex-library "human")

;;; -----------------------------------------------------------------------------
;;; KITCHENWORLD
;;;

;;; An APEX simworld is the collection of specifications needed to simulate a
;;; particular simulation domain.  This file contains specs for the physical
;;; environment of a simulated kitchen, plus functions for determining the
;;; effects of action by simulation entities (both human and non-human).  The
;;; file pdl.lisp specifies the "how-to" knowledge needed by these simulated
;;; human agents to perform kitchen tasks.  The file initialize.lisp specifies
;;; how these definitions are used to create the entitites for a specific
;;; simulation trial.

;;; -----------------------------------------------------------------------------
;;; ----- Kitchenworld object definitions

;;; == Pan

;;; In kitchenworld, pans are distinct from other physobs in their appearance
;;; and mass.  They have no distinctive state attributes and do not need any
;;; specific assemble method.

(defclass pan (physob)
  ((mass :initform 5) ;;subclasses can overwrite defaults; so can make-instance 
   (temp :initform 0)
   (shape :initform '(pan)) ;;object can have a multiple shape descriptions
   (color :initform 'grey)))

;;; == Counter

;;; In kitchenworld, counters are distinct from other physobs only in their
;;; appearance.  They have no distinctive state attributes.  However, they
;;; do need some "assembly" to specify vertices based on input dimensions; the
;;; assemble method automatically uses the (given) refpos value and the
;;; (derived) vertices to compute a value for the pos (centroid) attribute.  The
;;; assembly method also makes the kitchen-counter visible within its locale by
;;; adding it to that locales vfield (visual-field).

(defclass kitchen-counter (physob)
  ((shape :initform '(rectangle counter))
   (dimensions :accessor dimensions :initarg :dimensions)))

(defmethod assemble ((kitchen-counter-1 kitchen-counter) &key component-of)
  (setx (vertices kitchen-counter-1)
	(generate-rectangular-vertices (dimensions kitchen-counter-1)))
  (setx (pos kitchen-counter-1) (refpos->pos kitchen-counter-1))
  kitchen-counter-1)  ;; assemble method must always return object

;;; == Stovetop

;;; A stovetop is more complicated than either of the previous items, mainly
;;; because it has many components (burners and dials).  The assembly method for
;;; a stovetop is therefore more elaborate.

(defclass stovetop (physob)
  ((shape :initform '(rectangle stovetop))
   ;; the size of the stovetop is used to init its appearance   
   (dimensions :accessor dimensions :initarg :dimensions)
   ;; burner/dial specifications incl. name, size, pos      
   (widgetspecs :accessor widgetspecs :initarg :widgetspecs)))


;;; A stovetop is a surface contianing a set of burners and dials.  Number,
;;; size, and relative positions must be specified so that an assemble method
;;; can create these "widgets."  The Jennair20 configuration below specifies 4
;;; dials/burner pairs: 
;;;   (<burnername> <diameter> <pos> <dialname> <pos> <numsettings>).
;;; Positions are with respect to the refpos (lower left corner) of the stovetop
;;; surface.  Measurements are in centimeters.

;;; Note on cooking.  Temperatures of all objects range from 0 (room temp) to 5
;;; (very hot).  The stovetop dials have 6 positions (0-5) causing a burner to
;;; reach a max temperature equal to its dial setting. 
    
(defconstant jennair20
    '((front-left-burner 20 (17 20) front-left-dial (5 4) 6)  ;; first burner/dial
      (back-left-burner 15 (17 42) back-left-dial (12 4) 6)  ;;  second...
      (front-right-burner 15 (60 20) front-right-dial (62 4) 6) 
      (back-right-burner 20 (60 42) back-right-dial (72 4) 6)))

;;; Note that the method for assembling an object with components resembles the
;;; initialize method for the whole scenario.  

(defmethod assemble ((stove-1 stovetop) &key component-of)
  (let ((loc (locale stove-1)))
    (setx (vertices stove-1)
	  (generate-rectangular-vertices (dimensions stove-1)))
    (setx (pos stove-1) (refpos->pos stove-1))
    (dolist (spec (widgetspecs stove-1))  ;;go through each line of jennair specs
      (let* ((dial (make-instance 'dial :name (fourth spec)
				  :numsettings (sixth spec) :locale loc
				  :pos (add-coordinates (fifth spec) (pos stove-1))))
	     (burner (make-instance 'burner :name (first spec)
				    :control dial :locale loc
				    :pos (add-coordinates (third spec) (pos stove-1)))))
	(setx (device dial) burner)  ;; the dial controls the burner device
	(assemble dial :component-of stove-1)
	(assemble burner :component-of stove-1)
      ))))

;;; == Burner

;;; TEMPerature is another important attribute of a burner, but this is
;;; inherited (with default value 0 as room temp) from physob.  When its
;;; associated dial is turned to a non-0 setting, a burner becomes a
;;; heatsource.  However, the dial change only affects the burner state
;;; indirectly via the adjust-temperature activity that affects all physobs. 

(defclass burner (physob)
  ((control :accessor control :initarg :control) ;control dial
   (mass :initform 2)
   (temp :initform 0)
   (shape :initform '(circle burner))
   (color :initform 'black)))

;;; heat source maxtemp equals the dial setting
(defmethod heatsource-maxtemp ((obj burner))
  (setting (control obj)))

;;; determines whether a burner is currently acting as a heat source
(defmethod heatsource-p ((obj burner))
  (> (setting (control obj)) 0))

;;; == Dial

;;; Dials are distinguished from other physobs by their appearance, by the kind
;;; of action (turning) used to change their setting, and by the state parameter
;;; SETTING.

(defclass dial (physob)
  ((setting :accessor setting :initarg :setting :initform 0) 
   ;; represents the value to which the dial is currently pointing
   (numsettings :accessor numsettings :initarg :numsettings)
   (device :accessor device :initarg :device :initform nil)
   ;; the device that the dial controls
   (aspect :accessor aspect :initarg :aspect :initform nil)
   ;; aspect of the device's function (e.g volume) affected by dial setting;
   ;; if nil, then dial setting effect is passive and usually delayed
   (shape :initform '(circle dial))
   (color :initform 'black)))

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

;;; -- Turning and turned activities

;;; --- Turn dial

(procedure :sequential
 (index (turn-dial ?dial ?setting using ?hand))
 (turn ?dial to ?setting with ?hand taking (1 sec)))


(primitive
 (index (turn ?object to ?setting with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion
  (signal-event (turned ?object ?setting))))


;;; --- Turn dial

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

;;;  --- Eggs

;;; Eggs are tricky to represent.  For example, when one cracks and separates an
;;; egg, causing the yolk/white (eggmass) to come out, one is left with two
;;; objects (shell and eggmass).  One could represent this as the destruction of
;;; the old (egg) object and the creation of two new ones (shell and eggmass).
;;; But then, how is this mundane act different from a "magical" transformation
;;; -- e.g. an egg into a pair of birds?  Another idea is to have one of the
;;; new objects be the continuation of the old one, so that only one new object
;;; is created.  Which should be the continuation?  The eggmass seems sensible
;;; since it's the important part of the egg.  But this produces the awkward
;;; situation in which a previously held object drops out of the hand while a
;;; newly created one (the shell) "appears" in the hand from nowhere.  The
;;; alternative used here is to treat the shell as the original object.  

;;; == food

;;; This data structure defines a food as a kind of physical object that has a
;;; cookstate and a cookrate.  Cookstate is number ranging from 0 (raw) to 9
;;; (badly burnt).  The most desirable cookstate will vary for different foods.
;;; When sufficiently heated, a food object's cookstate increases.  The cookrate
;;; specifies how many (simulated) seconds are required for the cookstate to
;;; advance by 1 if the food's temp = 5.  In kitchenworld, the temp slot value
;;; ranges from 0 (room temperature) to 5 (very hot).

(defclass food (physob)
  ((cookstate :accessor cookstate :initarg :cookstate)    
   ;; integer from 0 to 9 where 0 is raw and 9 is burnt to a crisp
   (cookrate :accessor cookrate :initarg :cookrate))
  );; seconds of cooking needed to advance cookstate at std temp

;;; == egg

;;; The egg is represented as a combination of two objects -- shell and eggmass.
;;; Initially, only the shell can be seen or manipulated.  By creating the
;;; eggmass along with the shell, it is possible, e.g., to model what happens to
;;; the eggmass if it is heated while still in the shell.  An egg is used here
;;; only as a holder for its components.  Agent actions such as grasping are
;;; performed on the shell (if the shell is moved, the insides move with it).
;;; Cooking effects take place on the eggmass.

(defclass egg (physob)
  ((eggshell :accessor eggshell :initarg :eggshell)
   (eggmass :accessor eggmass :initarg :eggmass)))

(defmethod assemble ((egg-1 egg) &key component-of)
  (let* ((eggmass (make-instance 'eggmass :temp 0 :mass 1 :cookstate 0 :cookrate 30 
				 :yolk 'intact :shape '(eggmass) :texture 'liquid 
				 :component-of (component-set egg-1)))
	 (shell (make-instance 'eggshell :soundness 'intact :color 'white
		    :shape '(egg eggshell ovoid) :texture 'smooth 
		    :component-of (component-set egg-1))))
    (setx (eggshell egg-1) shell)
    (setx (eggmass egg-1) eggmass)
    (assert-physob-relation `(in ,eggmass ,shell))
    (assemble eggmass :component-of egg-1)
    (assemble shell :component-of egg-1)
    egg-1))

;;; == eggshell

(defclass eggshell (physob)
  ((soundness :accessor soundness :initarg :soundness)
   (temp :initform 0))
  );; possible values: intact, cracked, open

;;; models the effect of striking an eggshell against another object.  This
;;; assumes that the object <against> will be massive and hard enough to crack
;;; the shell.

(defmethod struck ((shell eggshell) against)
  (declare (ignore against)) 
  (setx (texture shell) 'cracked)
  (setx (soundness shell) 'cracked))

;;; This function defines what happens to a cracked egg/shell when it is opened
;;; -- i.e. split apart by hand.  This function assumes that when the eggmass
;;; drops out of the shell, it doesn't change x,y position, but merely becomes
;;; decoupled from the shell -- i.e. it will no longer move with the shell.

(defmethod pulled-apart ((shell eggshell))
  (let* ((contents (find-all-matches `(in ?obj ,shell)))
	 (eggmass (if contents (first contents))))
    (when eggmass
      ;; eggmass becomes visible and no longer inside shell
      (make-visible eggmass)     
      (retract-physob-relation `(in ,eggmass ,shell)) 
      ;; This is a temporary change to simplify this part of the 
      ;; physob database logic.
      (dolist (obj (find-all-matches `(pan ?object)))
        (assert-physob-relation `(in ,obj ,eggmass)))
      ;; change shell soundness state and appearance
      (setx (texture shell) 'open)
      (setx (soundness shell) 'open))))

;;; == eggmass

;;; the "egg mass" is the combined yolk and white stuff (albumen)

(defclass eggmass (food)
  ((yolk :accessor yolk :initarg :yolk)
   (mass :initform 1))
  );; values: intact, broken, partly-mixed, mixed

;;; Causes eggmass to begin/continue cooking.  In some cases, this results in a
;;; change to the visual texture of the eggmass.

(defmethod cookstate-changed ((em eggmass))
  (let ((start-texture (texture em))
	(end-texture (compute-eggmass-texture em)))
    (when (not (equal start-texture end-texture))
      (setx (texture em) end-texture))))

(defun compute-eggmass-texture (eggmass)
  (let ((ec (cookstate eggmass)))
    (cond ((< ec 2.0) 'liquid)
	  ((< ec 4.0) 'runny)
	  ((< ec 6.0) 'spongy)
	  ((< ec 7.0) 'crisp)
	  ((< ec 8.0) 'very-crisp)
	  (t 'ashen))))

;;; -----------------------------------------------------------------------------
;;; ----- "Physics" support functions
;;;
;;; The functions below are used to model the physical processes of temperature
;;; change and cooking.  Physics models of general use as these, though crude,
;;; are meant to be, would normally be stored as library functions in the
;;; directory /apex/lib; see /apex/lib/geometry for an example.  However, it is
;;; often useful to experiment with and iteratively refine new models before
;;; making them part of the general library.  


;;; --- Temperature change

;;; Temperature change is handled uniformly for all physobs in a locale that
;;; have a temperature value defined.
;;;
;;; The model of temperature change used here makes the following assumptions:
;;; (1) the temperature of a heatsource object is determined only by its
;;;     setpoint (maximum temp) and heating rate, not by other objects.  Rate
;;;     of temp change for these objects is fixed.
;;; (2) the more massive an object is, the slower its temp changes
;;; (3) more massive objects have a greater influence on the temperature of the
;;;     objects they touch
;;; (4) ambient air is a low-mass influence on the temperature of all objects
;;;
;;; This model is crude in many respects -- e.g. it doesn't account for
;;; differences in object surface area, amount of surface area in contact with
;;; other specific objects, ablative (evaporative) release of heat (especially
;;; relevant for cooking), differences in heat conductivity of different
;;; materials, or the effect of temperature differential on heat/cool rate.

(defclass adjusting-temperature (activity) ())

(defmethod update-activity ((act adjusting-temperature) (ob appob))
  (let ((locale (primary-object act)))
    (update-all-physob-temps (contents locale) (update-interval act))))

(defun update-all-physob-temps (objects update-interval)
  (when objects ;; if no more objects then done
    (dolist (obj objects)
      ;;      (princ obj)
      ;;(format *log* "Thermal evaluation[~S]:~S ~S ~S~%" obj (typep obj 'physob) (temp obj) (mass obj)) 
      (when (and (typep obj 'physob) (temp obj) (mass obj))
        ;;(format *log* "Updating Temp for ~S~%" obj)
	(update-all-physob-temps (components obj) update-interval)
	(if (heatsource-p obj)
	    (adjust-heatsource-temp obj update-interval)
	  (adjust-normal-temp obj update-interval))))))

;;; by default, a given physob is not considered a heatsource
(defmethod heatsource-p ((obj physob))
  nil)

(defun adjust-heatsource-temp (physob interval)
  (let* ((heatup-rate 10000) ;10 seconds to change one temperature level
	 (max-change (/ interval heatup-rate))
	 (new-temp (min (heatsource-maxtemp physob)
			(+ max-change (temp physob)))))
    ;;(format *log* "dT ~S ~S ~S~%" max-change (temp physob) new-temp)
    (setx (temp physob) new-temp)))

;;; cooking activity starts as a side-effect of temperature change
(defun adjust-normal-temp (physob update-interval)
  (let* ((touch-list (find-all-touching-objects physob))
	 (Teq (compute-equilibrium-temp touch-list))
	 (Tdiff (- Teq (temp physob)))
	 (base-interval 7500) ;; 7.5 seconds to change one temp level
	 (adj-interval (* base-interval (mass physob)))
	 (abs-Tchange-max (/ update-interval adj-interval))
	 (temp-change (* (sign Tdiff) (min (abs Tdiff) abs-Tchange-max)))
	 (new-temp (+ (temp physob) temp-change)))
    
   ;; (format *log* "~&   touch-list      ~S~%" touch-list)
    ;;(format *log* "~&   Teq             ~S~%" Teq)
    ;;(format *log* "~&   Tdiff           ~S~%" Tdiff)
    ;;(format *log* "~&   base-interval   ~S~%" base-interval)
    ;;(format *log* "~&   adj-interval    ~S~%" adj-interval)
    ;;(format *log* "~&   abs-Tchange-max ~S~%" abs-Tchange-max)
    ;;(format *log* "~&   temp-change     ~S~%" temp-change)
    ;;(format *log* "~&   new-temp        ~S~%" new-temp)

    (if (and (typep physob 'food) (<= (temp physob) 1.0) (> new-temp 1.0))
	(start-activity physob 'cooking :update-interval 2000))
    ;;(format *log* "Temp change[~S]: From ~S to ~S~%" physob (temp physob) new-temp)
    (setx (temp physob) new-temp)))
;;;    (format t "Temp of ~a is ~a~%" physob new-temp)))

(defun sign (num)
  (if (minusp num) -1.0 (if (zerop num) 0.0 1.0)))

(defun find-all-touching-objects (physob)
  (append (find-all-matches `(in ?obj ,physob))
	  (find-all-matches `(in ,physob ?obj))
	  (find-all-matches `(on ?obj ,physob))
	  (find-all-matches `(on ,physob ?obj))))

;;; The equilibrium temperature (Teq) for an object is the temperature it would
;;; eventually converge to if all the objects it is touching remained at their
;;; same temperature.

(defun compute-equilibrium-temp (obj-list)
  (let ((total-heat 0.0)
	(total-mass 0.2))  ;; effect of ambient air temperature
    (dolist (obj obj-list)
      (when (and (temp obj) (mass obj))
;;;	(format "~a ~a ~a~%" obj (mass obj) (temp obj))
	(setx total-heat (+ total-heat (* (temp obj) (mass obj))))
	(setx total-mass (+ total-mass (mass obj)))))
    (/ total-heat total-mass)))

;;; --- Cook a food item

;;; The current model of cooking makes the following assumptions: (1) food must
;;; be at or above some minimum temperature (temp=1.0) to cook at all; (2) the
;;; rate of cooking is directly proportional to temperature; (3) different food
;;; types cook at different rates as defined by the attribute food-cookrate.
;;; this is crude in several respects -- e.g. there is no account of the effect
;;; of fast heating versus slow heating.

(defclass cooking (activity) ())

(defmethod update-activity ((act cooking) (ob appob))
  (when (> (temp (primary-object act)) 1.0)
    ;; food only cooks if temp is above 1.0 (warm)
    (let* ((interval (update-interval act)) ;; how often cookstate is updated
	   (food (primary-object act))
	   (cookspeed (* .25 (- (temp food) 1.0))) ;; percent of max cook speed
	   (cookstate (cookstate food))
	   (max-increment   ;; how much more cooked it will get this update
	    (* cookspeed (/ interval (* 1000 (cookrate food)))))
	   (new-cookstate (min 9.0 (+ max-increment cookstate )))
	   ) ;; can't get more cooked than 9.0 (burnt badly) 
      (when (> new-cookstate cookstate)
	(setx (cookstate food) new-cookstate)
	(signal-event (cookstate-changed food))
	))));;triggers cooking results specific to food type


;;; ------------------------------------------------------------------

(defvar *temp*)				; for debugging


;;; ----------------------------------------------------------------------------
;;; Kitchenworld procedures
;;;
;;; The agent's task is to make a fried egg.  
;;; ----------------------------------------------------------------------------

;;; Note: The right and left hands are referred to by the symbols RIGHT
;;; and LEFT.  This will occasionally produce confusion.  For instance,
;;; the following step (step s2 (turn-dial right lower-left-dial 4)
;;; (waitfor ?s1)) means turn-the specified dial using the right hand,
;;; not turn the dial to the right (clockwise).

;;; --- Do Domain

(procedure
 (index (do-domain))
 (step s1 (grasp ?pan with right-hand)
       (waitfor (shape ?pan pan)))
 (step s2 (make-fried-egg) (waitfor ?s1))
 (step s3 (end-trial) (waitfor ?s2)))


;;; --- Make Fried Egg

(procedure 
 (index (make-fried-egg))
 (step s1 (assemble ingredients fried-egg))
 (step s2 (turn-dial front-left-dial 4 using right-hand) 
       (waitfor ?s1 (shape front-left-dial dial)))
 (step s3 (move ?pan onto front-left-burner using right-hand) 
       (waitfor ?s2 (shape ?pan pan)))
 (step s4 (release ?pan from right-hand) (waitfor ?s3))
 (step s5 (crack-open ?eggshell over ?pan) 
       (waitfor ?s4 (shape ?eggshell eggshell)))
 (step s6 (turn-dial front-left-dial 0 using right-hand) 
       (waitfor (texture ?eggmass spongy)))
 (step s7 (move ?pan onto front-right-burner using right-hand)
       (waitfor ?s6))
 (step s8 (release ?pan from right-hand) (waitfor ?s7))
 (step s9 (terminate) (waitfor ?s8)))

;;; --- Assemble ingredients

;;; Procedures should normally contain appropriate preparatory actions -- e.g. 
;;; assembling (acquiring and putting in easy reach) ingredients for a cooking
;;; task.  Currently, the kitchenworld assembles the needed ingredients to fry
;;; an egg at the start of the simulation trial.  Thus, the following is a null
;;; procedure. 

(procedure
 (index (assemble ingredients ?food-item))
 (step s1 (terminate)))

;;; --- Crack open eggshell

(procedure
 (index (crack-open ?shell over ?object))
 (profile (left 8 10))
 (step s1 (move ?shell over ?object using left-hand))
 (step s2 (strike ?shell against ?object using left-hand) (waitfor ?s1))
 (step s3 (pull-apart ?shell)  
       (waitfor ?s2 (texture ?shell cracked)))
 (step s4 (move ?shell onto counter using left-hand) 
       (waitfor ?s3 (shape ?em eggmass)))
 (step s5 (release ?shell from left-hand) (waitfor ?s4))
 (step s6 (terminate) (waitfor ?s5)))

;;; --- Pull apart

;;; The person needs both hands to pull something apart

(procedure
 (index (pull-apart ?object))
 (profile (left-hand 8 10) (right-hand 8 10))
 (step s1 (pull-apart ?object taking (1500 ms)))
 (step s2 (reset) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?s1)))

;;; ----- (i) Initializing a Kitchenworld simulation trial

;;; INITIALIZE is called at the beginning of a simulation trial to create the
;;; objects and activities to be simulated.  In kitchenworld, these include
;;; objects such as a kitchen, cook and egg, and activities these objects are
;;; performing such as seeing (the cook) and temp-equilibriation (all objects).
;;; The kitchen object is of a special type called a locale that provides a
;;; 2-dimensional frame of reference for the objects "in" it.  Most objects
;;; are intiialized with a pos(ition) value that indicates where in the locale
;;; it lies.  Position units are in centimeters.

(defun initialize-kitchen ()
  (initialize-physob-database)
  (let* 
      ((kitchen (make-instance 'locale :name 'kitchen))
       (cook (make-instance 'human :name 'cook :location '(0 0 22) :pos '(0 0) 
			    :locale kitchen))
       (counter 
	(make-instance 'kitchen-counter :dimensions '(60 62) 
		       :refpos '(-70 20) :name 'counter :locale kitchen))
       (stovetop 
	(make-instance 'stovetop :dimensions '(78 62)
		       :refpos '(-10 20) :name 'stovetop :locale kitchen
		       :widgetspecs jennair20))
       (egg (make-instance 'egg :pos '(-20 40) :locale kitchen))
       (pan (make-instance 'pan :pos '(-45 40) :locale kitchen)))
    (assemble cook)  ;; ! maybe ok to couple with make-instance
    (assemble counter)
    (assemble stovetop)
    (assemble egg)
    (assemble pan)    
    (assert-physob-relation `(pan ,pan))
    (assert-physob-relation `(on ,pan ,counter))
    (assert-physob-relation `(on ,(eggshell egg) ,counter))
    (start-activity kitchen 'adjusting-temperature :update-interval 1000)
    ;;(format *log* "Relations ~S~%" *physob-db*)
    ))

;    (set-pause-cycle 500)))



;;; MORE DISCUSSION to come later.

;;; --- Make-instance

;;; --- Locales

;;; --- Assembly

;;; Calling the assemble method on a newly created object "fills out" a
;;; partially specified object.  For example, one might choose to specify
;;; the REFPOS of an object (the position of the first specified vertex) rather
;;; than the POS (its centroid).  Calling assemble automatically computes the
;;; POs value in this case.  In many cases, specific object types will require
;;; specific assembly actions such as  (1) creating component objects (a stove
;;; includes burners, dials, etc..; a human includes hands, eyes, etc...); (2)
;;; starting activities (e.g. a human is initialized with a SEEING activity).

;;; --- Humans
;;;
;;; treated uniformly with non-human objs; initial-activites


;;; --- Physob relations

;;; --- Names

;;; --- Starting Activities

;;; --- Simpause

