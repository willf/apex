;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/cooking.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: cooking.lisp,v 1.2 2006/01/15 03:42:56 dalal Exp $

(in-package :user)

;;; ---------------------------------------------------------------------
;;; ----- "Physics" support functions
;;;
;;; The functions below are used to model the physical processes of 
;;; temperature change and cooking.  Physics models of general use as 
;;; these, though crude, are meant to be, would normally be stored as 
;;; library functions in the directory /apex/lib; see /apex/lib/geometry 
;;; for an example.  However, it is often useful to experiment with and 
;;; iteratively refine new models before making them part of the general
;;; library.  


;;; --- Temperature change

;;; Temperature change is handled uniformly for all physobs in a locale 
;;; that have a temperature value defined.
;;;
;;; The model of temperature change used here makes the following 
;;; assumptions:
;;;    (1) the temperature of a heatsource object is determined only by 
;;;        its setpoint (maximum temp) and heating rate, not by other 
;;;        objects.  Rate of temp change for these objects is fixed.
;;;    (2) the more massive an object is, the slower its temp changes
;;;    (3) more massive objects have a greater influence on the 
;;;        temperature of the objects they touch
;;;    (4) ambient air is a low-mass influence on the temperature of all
;;;        objects
;;;
;;; This model is crude in many respects -- e.g. it doesn't account for
;;; differences in object surface area, amount of surface area in contact
;;; with other specific objects, ablative (evaporative) release of heat 
;;; (especially relevant for cooking), differences in heat conductivity 
;;; of different materials, or the effect of temperature differential on 
;;; heat/cool rate.


(defclass adjusting-temperature (activity) ())

(defmethod update-activity ((act adjusting-temperature) (ob appob))
  (let ((locale (primary-object act)))
    (update-all-physob-temps (contents locale) (update-interval act))))

(defun update-all-physob-temps (objects update-interval)
  (when objects ;; if no more objects then done
    (dolist (obj objects)
      ;;      (princ obj)
      ;;(format *log* "Thermal evaluation[~S]:~S ~S ~S~%" obj 
      ;;(typep obj 'physob) (temp obj) (mass obj)) 
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
 ;  (let* ((heatup-rate 10000) ;10 seconds to change one temperature level
    (let* ((heatup-rate 100) 
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
        (base-interval 75) ;; now 75 ms 7.5 seconds to change one temp level
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

    (if (and (typep physob 'food) (<= (temp physob) 1.0) 
             (> new-temp 1.0))
	(start-activity physob 'cooking :update-interval 200)) ;used to be 2000
    ;;(format *log* "Temp change[~S]: From ~S to ~S~%" physob 
    ;;(temp physob) new-temp)
    (setx (temp physob) new-temp)))

(defun sign (num)
  (if (minusp num) -1.0 (if (zerop num) 0.0 1.0)))

(defun find-all-touching-objects (physob)
  (append (find-all-matches `(in ?obj ,physob))
	  (find-all-matches `(in ,physob ?obj))
	  (find-all-matches `(on ?obj ,physob))
	  (find-all-matches `(on ,physob ?obj))))

;;; The equilibrium temperature (Teq) for an object is the temperature it
;;;  would eventually converge to if all the objects it is touching 
;;; remained at their  same temperature.

(defun compute-equilibrium-temp (obj-list)
  (let ((total-heat 0.0)
	(total-mass 0.2))  ;; effect of ambient air temperature
    (dolist (obj obj-list)
      (when (and (temp obj) (mass obj))
	(setx total-heat (+ total-heat (* (temp obj) (mass obj))))
	(setx total-mass (+ total-mass (mass obj)))))
    (/ total-heat total-mass)))

;;; --- Cook a food item

;;; The current model of cooking makes the following assumptions: 
;;;    (1) food must be at or above some minimum temperature (temp=1.0)
;;;        to cook at all                          
;;;    (2) the rate of cooking is directly proportional to temperature 
;;;    (3) different food types cook at different rates as defined by 
;;;        the attribute food-cookrate.
;;; this is crude in several respects -- e.g. there is no account of the 
;;; effect of fast heating versus slow heating.

(defclass cooking (activity) ())

(defmethod update-activity ((act cooking) (ob appob))
  (when (> (temp (primary-object act)) 1.0)
    ;; food only cooks if temp is above 1.0 (warm)
    (let* ((interval (update-interval act)) 
           ;; how often cookstate is updated
	   (food (primary-object act))
	   (cookspeed (* .25 (- (temp food) 1.0))) 
           ;; percent of max cook speed
	   (cookstate (cookstate food))
	   (max-increment   
            ;; how much more cooked it will get this update
	    (* cookspeed (/ interval (* 100 (cookrate food))))) ; used to be 1000
	   (new-cookstate (min 9.0 (+ max-increment cookstate )))
	   ) ;; can't get more cooked than 9.0 (burnt badly) 
      (when (> new-cookstate cookstate)
	(setx (cookstate food) new-cookstate)
	(signal-event (cookstate-changed food))
	))));;triggers cooking results specific to food type
