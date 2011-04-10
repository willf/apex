;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/simulation/test/test3.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: test3.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $

;;; Test Simulation: A dimmer switch and light.
;;;
;;; The dimmer switch has a setting adjustable from 0.0 to 100.0
;;; The light has corresponding brightness.


;;; ------ Appobs

;;; Light

(defclass light (appob)
  ((brightness :initform 0 :accessor brightness)))

(defmethod adjust ((light light) amount)
  ;; Light * number -> ()
  (setx (brightness light) amount)
  (format t "Light changed to ~a.~%" amount))

(defmethod print-object ((l light) s)  ; Light * stream -> ()
  (format s "<Light-~a, brighness=~a>" (id l) (brightness l)))

(defun make-light ()  ; () -> Light
  (make-instance 'light :simulation *application*))

;;; Dimmer Switch

(defclass dimmer-switch (appob)
  ((level :initform 0 :accessor level)
   (maxlevel :initform 100 :initarg :maxlevel :reader maxlevel)
   (minlevel :initform   0 :initarg :minlevel :reader minlevel)
   ;; Light controlled by this switch
   (light :initarg :light :reader light)))
   
(defmethod adjust ((switch dimmer-switch) amount)
  ;; DimmerSwitch * number -> ()
  ;;
  ;; Set the new level for the switch
  (format t "Adjusting ~a by ~a.~%" switch amount)
  (setx (level switch) 
        (max (min (+ (level switch) amount)
                  (maxlevel switch))
             (minlevel switch)))
  ;; Now affect the light.
  ;; Note: Could check to see if level changed.
  ;;
  (adjust (light switch) (level switch)))

(defmethod print-object ((d dimmer-switch) s)  
  ;; DimmerSwitch * stream -> ()
  (format s "<DimmerSwitch-~a, level=~a>" 
          (id d) (level d)))
           
(defmethod make-dimmer-switch ((light light))
  ;; Light -> DimmerSwitch
  (make-instance 'dimmer-switch 
    :simulation *application*
    :light light))

;;; ---- Activities

;;; Incrementing and decrementing the level of the switch 
;;; is an instantaneous activity (takes zero time).

(defclass increment (activity) ())
(defclass decrement (activity) ())

(defmethod update-activity ((inc increment)
                            (switch dimmer-switch))
  ;; Increment * DimmerSwitch -> ()
  (adjust switch 1.0)
  (format t "Incremented: ~a.~%" switch)
  (complete-activity inc switch))

(defmethod update-activity ((dec decrement)
                            (switch dimmer-switch))
  ;; Decrement * DimmerSwitch -> ()
  (adjust switch -1.0)
  (format t "Decremented: ~a.~%" switch)
  (complete-activity dec switch))

;;; Increasing and decreasing the level of the switch is done
;;; "smoothly" at the specified rate (level units per time unit)

(defclass variable-rate-activity (activity)
  ((rate :initform 1 :initarg :rate  ; amount of change for
          :reader rate)))            ; each time unit

(defclass increasing (variable-rate-activity) ())
(defclass decreasing (variable-rate-activity) ())

(defmethod start-decreasing ((switch dimmer-switch)
                             (light light)
                             amount rate)
  ;; DimmerSwitch * Light * number * number -> ()
  (start-activity switch 'decreasing :primary-object light
                  :rate rate
                  :update-interval 1
                  :duration (/ amount rate)))

(defmethod start-increasing ((switch dimmer-switch)
                             (light light)
                             amount rate)
  ;; DimmerSwitch * Light * number * number -> ()
  (start-activity switch 'increasing :primary-object light
                  :rate rate
                  :update-interval 1
                  :duration (/ amount rate)))

(defmethod update-activity ((act increasing)
                            (ob dimmer-switch))
  ;; Increasing * DimmerSwitch -> ()
  (adjust ob (rate act)))

(defmethod update-activity ((act decreasing)
                            (ob dimmer-switch))
  ;; Decreasing * DimmerSwitch -> ()
  (adjust ob (- (rate act))))

;;; A completion method that simply reports completion.

(defmethod complete-activity ((act increasing) 
                              (ob dimmer-switch))
  (declare (ignore act))
  ;; Increasing * DimmerSwitch -> ()
  (format t "Increasing of ~a is finished.~%" ob))

(defmethod complete-activity ((act decreasing) 
                              (ob dimmer-switch))
  (declare (ignore act))
  ;; Decreasing * DimmerSwitch -> ()
  (format t "Decreasing of ~a is finished.~%" ob))

;;; ---- Simulation Initialization

(initialize-simulation
  (let* ((light (make-light))
         (switch (make-dimmer-switch light)))
    ;;
    ;; Turn the light up, then turn it down more slowly.
    ;; NOTE: this does not have the intended effect because
    ;; the activities are interleaved rather than sequenced.
    ;; A sequencing construct is needed, because the sim engine has no
    ;; such facility.
    ;;
    (start-increasing switch light 4 2) 
    (start-decreasing switch light 4 1)))
