;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/xplane/xplane-api.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: xplane-api.lisp,v 1.18 2006/04/19 20:39:56 rharris Exp $

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; Apex X-Plane API ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; some handy procedures ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; request the value of a singel variable once
;;

(procedure :seq
(index (xplane-query-value ?variable))
(step (xplane-enable-measurement ?variable))
(step (xplane-disable-measurement ?variable)
      (waitfor (<?variable> ac1 = ?value)))
(step (terminate >> ?value)))

;; (step (inform (?variable ?value))) 


;;
;; compute magnetic heading from true + declination
;;

(procedure :seq
(index (compute-runway-heading-magenetic))
(step (ask +current-runway+ for heading-true => ?heading-true))
(step (xplane-query-value mavar-deg => ?declination))
(step (compute-delta-heading ?heading-true ?declination => ?heading-mag))
(step (terminate >> ?heading-mag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; basic xplane / pdl interface ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; enable measurment of xplane variable
;;

(primitive
 (index (xplane-enable-measurement ?variable))
 (on-start (xplane-subscribe-variable ?variable)))

;;
;; enable multiple measurments of xplane variables
;;

(primitive
 (index (xplane-enable-measurements ?variables))
 (on-start (loop for var in ?variables do
                 (xplane-subscribe-variable var))))

;;
;; disable measurment of xplane variable
;;

(primitive
 (index (xplane-disable-measurement ?variable))
 (on-start (xplane-unsubscribe-variable ?variable)))

;;
;; disable multiple measurments of xplane variables
;;

(primitive
 (index (xplane-disable-measurements ?variables))
 (on-start (loop for var in ?variables do
                 (xplane-unsubscribe-variable var))))
;;
;; set the value of any defined xplane variable
;;

(primitive
 (index (xplane-set ?variable ?value))
 (profile network)
 (on-start (xplane-set-variable ?variable ?value)))

;;
;; set & verify the value of any defined xplane variable
;;

(primitive
 (index (xplane-set-verified ?variable ?value))
 (profile network)
 (on-start (xplane-set-variable ?variable ?value)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; takeoff and landing ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; takeoff from current ruwnay
;;

(procedure
 (index (takeoff))
 
 ;; view from inside the plane
 
 (step (xplane-camera-view cockpit))

 ;; measure knots indicated airspeed, and altitude in feet above ground level
 
 (step (xplane-enable-measurements (vind-kias alt-ftagl)))

 ;; disable all residual autopilot functions
 
 (step (autopilot-disable-all))
 
 ;; start take off sequence
 
 (step (start-takeoff-sequence)
       (waitfor ?autopilot-disable-all))

 ;; view from inside the plane once we are rolling
 
 (step (xplane-camera-view circle)
       (waitfor ?start-takeoff-sequence))

 ;; get vfe, and put up the flaps before we get to that value
 
 (step (xplane-query-value vfe-ktas => ?vfe-ktas))
 (step (set-flaps 0.0)
       (waitfor (:measurement (vind-kias ac1 = ?kias1) :value (> (* 0.90 <?vfe-ktas>)))))
 
 
 ;; when we are going fast enough, start the climb
 
 (step (ask +current-aircraft+ for ias-rotate => ?ias-rotate))
 (step (ask +current-aircraft+ for climb-rate => ?climb-rate))
 (step (autopilot-hold vertical ?climb-rate)
       (waitfor (:measurement (vind-kias ac1 = ?kias2) :value (> <?ias-rotate>))))
 (step (xplane-print "start climb")
       (waitfor ?autopilot-hold))
 (step (xplane-camera-view spot)
       (waitfor ?autopilot-hold))

 ;; are we high enough to pick up the gear

 (step (ask +current-aircraft+ for agl-gear-up => ?agl-gear-up))
 (step (raise-gear)
       (waitfor (:measurement (alt-ftagl ac1 = ?ftagl) :value (> ?agl-gear-up))))
 (step (xplane-print "raise gear")
       (waitfor ?raise-gear))
 (step (xplane-camera-view circle)
       (waitfor ?raise-gear))

 ;; we don't need these measurements anymore
 
 (step (xplane-disable-measurements (vind-kias alt-ftagl))
       (waitfor  (:and ?set-flaps
                       ?raise-gear)))
   ;; all done

 (step (terminate)
       (waitfor ?xplane-disable-measurements)))

;;
;; start takeoff sequence
;;

(procedure 
 (index (start-takeoff-sequence))
 (step (xplane-print "start takeoff roll")) 

  ;; configure the aircraft for takeoff roll
 
 (step (compute-runway-heading-magenetic => ?heading-mag))
 (step (autopilot-hold heading ?heading-mag)
       (waitfor ?compute-runway-heading-magenetic))
 (step (xplane-set ruddr-yoke 0.0))
 (step (autopilot-hold vertical 0.0))
 
 ;; set takeoff flaps
 
 (step (ask +current-aircraft+ for flaps-takeoff))
 (step (set-flaps ?flaps-takeoff)
       (waitfor (flaps-takeoff ?flaps-takeoff)))

 ;; punch up the throttle, when the autopilot is set
 
 (step (set-throttle 1.0)
       (waitfor ?autopilot-hold-1 
                ?autopilot-hold-2
                ?set-flaps))

 ;; get max horse power and engine count, and measure current horse power
 
 (step (ask +current-aircraft+ for horse-power => ?hp))
 (step (ask +current-aircraft+ for engine-count))
 (step (xplane-enable-measurements (flap-postn power1-hp power2-hp)))

 ;; release the brakes when the engine(s) is(are) at about 70%

 (step (release-brakes) 
       (waitfor (:or
                 
                 ;; one engine case
                 
                 (:and 
                  (engine-count 1)
                  (:measurement (power1-hp ac1 = ?hp1) :value (> (* <?hp> 0.70))))
                 
                 ;; two engine case
                 
                 (:and 
                  (engine-count 2)
                  (:measurement (power1-hp ac1 = ?hp1) :value (> (* <?hp> 0.70)))
                  (:measurement (power2-hp ac1 = ?hp2) :value (> (* <?hp> 0.70)))))))

 ;; disable measurements
 
 (step (xplane-disable-measurements (flap-postn power1-hp power2-hp))
       (waitfor ?release-brakes))

 ;; all done when measurments disabled
 
 (step (terminate)
       (waitfor ?xplane-disable-measurements)))

;;
;; land
;;

(procedure
 (index (land))

 ;; start land sequence

 (step (start-land-sequence)) 

 ;; measure knots indicated airspeed, and altitude in feet above ground level
 
 (step (xplane-enable-measurements (vind-kias alt-ftagl flap-postn))
       (waitfor ?start-land-sequence))

  ;; when we are below vfe drop the flaps
 
 (step (xplane-query-value vfe-ktas => ?vfe))
 (step (ask +current-aircraft+ for flaps-land => ?flaps-land))
 (step (set-flaps ?flaps-land)
       (waitfor ?ask-1 
                (:measurement (vind-kias ac1 = ?vind-kias) :value (< ?vfe))))
 
 ;; slow down a bit
 
 (step (ask +current-aircraft+ for ias-final)
       (waitfor ?set-flaps-1))
 (step lowered-throttle (autopilot-hold airspeed ?ias-final)
       (waitfor (ias-final ?ias-final)))
 
 ;; flare when we are close to the ground

 (step (ask +current-aircraft+ for agl-flare => ?agl-flare))
 (step (flare)
       (waitfor (:in-order
                 (agl-flare ?)
                 (:measurement (alt-ftagl ac1 = ?agl2) :value (< ?agl-flare)))))

 ;; touched down,  disable autopilots
 
 (step (autopilot-disable-all)
       (waitfor (:measurement (alt-ftagl ac1 = ?agl3) :value (< 1))))

 ;; zero out the throttle
 
 (step (set-throttle 0.0)
       (waitfor ?autopilot-disable-all))
 
  ;; and inform the world
 
 (step (xplane-print "touch down")
      (waitfor ?set-throttle))

 ;; are we going slow enough, to brake hard?
 
 (step (ask +current-aircraft+ for ias-brake => ?ias-brake))
 (step (set-brakes 1.0)
       (waitfor (:in-order
                 (ias-brake ?)
                 (:measurement (vind-kias ac1 = ?kias) :value (< ?ias-brake)))))
 
 ;; we've stopped, inform the world
 
 (step (xplane-print "full stop")
       (waitfor (:measurement (vind-kias ac1 = 0 +/- 3))))

 ;; cleanup the flaps 
 
 (step (set-flaps 0.0) 
       (waitfor (:measurement (vind-kias ac1 = 0 +/- 3))))
 
 ;; when the flaps are all the way up we're done with the measurements
 
 (step (xplane-disable-measurements (vind-kias alt-ftagl flap-postn)) 
       (waitfor (:in-order
                 ?set-flaps-2
                 (:measurement (flap-postn ac1 = 0.0)))))
 ;; all done

 (step (terminate)
       (waitfor ?xplane-disable-measurements)))
;;
;; start landing sequence
;;

(procedure :seq
 (index (start-land-sequence))

 ;; slow to approach speed
 
 (step (ask +current-aircraft+ for ias-appraoch => ?ias-aproach))
 (step (autopilot-set airspeed ?ias-aproach))
 
 ;; drop the gear to help slow down
 
 (step (lower-gear))
 (step (xplane-print "drop gear"))
 
 ;; set nav1 to ils frequencty (should ensure nav1 selected!)
 
 (step (ask +current-runway+ for ils-freq => ?ils-freq))
 (step (set-nav1 ?ils-freq))
 
 ;; enable vertical navigation when where inside the vor deflection
 
 (step (autopilot-enable arm-vnav))
 
 ;; enable horizontal navigation when we're on the base leg
 
 (step (autopilot-enable arm-hnav))
 
 ;; update the world

 (step (xplane-print "start approach")))

;;
;; flare
;;

(procedure :seq
 (index (flare))
 (step (ask +current-aircraft+ for vvi-flare => ?vvi-flare))
 (step (autopilot-hold vertical ?vvi-flare))
 (step (set-brakes 0.20))
 (step (ask +current-aircraft+ for ias-flare => ?ias-flare))
 (step (autopilot-hold airspeed ?ias-flare))
 (step (xplane-print "flare")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; autopilot pdl interface ;;
;;                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; set and enable autopilot function hold in one step
;;

(primitive
 (index (autopilot-hold ?function ?value))
 (on-start
  
  ;; establish function details
  
  (destructuring-bind (mode mode-var val-var order) (autopilot-lookup ?function)
    
    ;; if this function requires the value to be set first,  do that
    
    (when (eq order *ap-value-first*)
      (xplane-set-variable val-var ?value)
      (sleep 0.1)
      (xplane-set-variable mode-var mode))

    ;; if this function requires the mode to be set first do that
    
    (when (eq order *ap-mode-first*)
      (xplane-set-variable mode-var mode)
      (sleep 0.1)
      (xplane-set-variable val-var ?value))
    
    ;; shock autopilot channels
    
    (xplane-shock-channel 'ap-mode)
    (xplane-shock-channel 'ap-set))))

;;
;; enable autopilot function
;;

(primitive
 (index (autopilot-enable ?function))
 (on-start 
  (destructuring-bind (mode mode-var val-var order) (autopilot-lookup ?function)
    (xplane-set-variable mode-var mode))))

;;
;; disable autopilot function
;;

(primitive
 (index (autopilot-disable ?function))
 (on-start
  (destructuring-bind (mode mode-var val-var order) (autopilot-lookup ?function)
    (xplane-set-variable mode-var *ap-disable*))))

;;
;; disable all autopilot functions
;;

(primitive
 (index (autopilot-disable-all))
 (on-start
  (loop for (func mode mode-var val-var order) in *autopilot-function-lut* do
        (xplane-set-variable mode-var *ap-disable*))))
;;
;; set autopilot function value
;;

(primitive
 (index (autopilot-set ?function ?value))
 (on-start
  (destructuring-bind (mode mode-var val-var order) (autopilot-lookup ?function)
    (xplane-set-variable val-var ?value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; traffic pattern code ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; fly left or right traffic pattern until we turn to given leg
;;

(procedure
 (index (fly ?direction pattern to ?leg))

 ;; configure the aircraft for flight in the traffice pattern
 
 (step (configure-for-pattern-flight ?direction => ?rw-heading))

 ;; when we've entered the crosswind region fly that leg
 
 (step (turn cross ?rw-heading ?direction)
       (waitfor (pattern-status cross)))
 
 ;; when we've entered the downwind region fly that leg
 
 (step (turn down ?rw-heading ?direction)
       (waitfor (pattern-status down)))
 
 ;; when we've entered the base region fly that leg
 
 (step (turn base ?rw-heading ?direction)
       (waitfor (pattern-status base)))
 
 ;; when we've entered the upwind region fly that leg
 
 (step (turn up ?rw-heading ?direction)
       (waitfor (pattern-status up)))

 ;; cleanup when we've turned to the final leg
 
 (step (unconfigure-pattern-flight)
       (waitfor (turned <?leg>)))
 
 ;; terminate once we've cleaned up
 
 (step (terminate)
       (waitfor ?unconfigure-pattern-flight)))

;;
;; fly left or right traffic pattern extended downwind
;; until we turn to given leg
;;

(procedure
 (index (fly extended ?direction pattern to ?leg))

 ;; configure the aircraft for flight in the traffice pattern
 
 (step (configure-for-pattern-flight ?direction => ?rw-heading))

 ;; when we've entered the crosswind region fly that leg
 
 (step (turn cross ?rw-heading ?direction)
       (waitfor (pattern-status cross)))
 
 ;; when we've entered the downwind region fly that leg
 
 (step (turn down ?rw-heading ?direction)
       (waitfor (pattern-status down)))
 
 ;; when we've entered the extended region fly that leg
 
 (step (turn extend ?rw-heading ?direction)
       (waitfor (pattern-status extend)))
 
 ;; when we've entered the upwind region fly that leg
 
 (step (turn up ?rw-heading ?direction)
       (waitfor (pattern-status up)))

 ;; cleanup when we've turned to the final leg
 
 (step (unconfigure-pattern-flight)
       (waitfor (turned <?leg>)))
 
 ;; terminate once we've cleaned up
 
 (step (terminate)
       (waitfor ?unconfigure-pattern-flight)))
 
;;
;; configure for traffic pattern flight
;; 

(procedure :seq
 (index (configure-for-pattern-flight ?direction))

 ;; set auto pilot to traffic pattern speed

 (step (ask +current-aircraft+ for ias-pattern => ?ias-pattern))
 (step (autopilot-hold airspeed ?ias-pattern))
  
 ;; set auto pilot to traffic pattern altitude
 
 (step (ask +current-airport+ for tpa => ?tpa))
 (step (autopilot-hold altitude ?tpa))
 
 ;; get current runway magnetic heading
 
 (step (compute-runway-heading-magenetic => ?rw-heading))
 
 ;; select traffice pattern, this & lat/long trigger pattern measurments
 
 (step (select-traffic-pattern 
        +current-runway+ +current-aircraft+ ?direction ?rw-heading))
 
 ;; measure the vehicle location, lat/long 
 
 (step (xplane-enable-measurements (lat-deg lon-deg)))
 
 ;; return the runway heading
 
 (step (terminate >> ?rw-heading)))

;;
;; cleanup after traffice pattern flight
;;

(procedure :seq
 (index (unconfigure-pattern-flight))

  ;; disable lat long measurements

 (step (xplane-disable-measurements (lat-deg lon-deg))))

 ;; and deselect the traffice pattern

;; (step (deselect-traffic-pattern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;; misc xplane primitives ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; move aircraft to a given airport given an airport code and runway index
;;

(primitive
 (index (xplane-place-aircraft ?airport-code ?runway-number))
 (on-start
  
  ;; select airport and runway in question
  
  (select-runway ?airport-code ?runway-number)
  
  ;; place aircraft on the runway
  
  (xplane-place-aircraft-at-airport ?airport-code (index *current-runway*) (backward *current-runway*))))


;;
;; load a particular aircraft
;;

(primitive
 (index (xplane-load-aircraft ?path))
 (on-start (xplane-load-aircraft 0 ?path)))
 
;;
;; select the current aircraft
;;

(primitive 
 (index (select-aircraft ?name))
 (return (select-aircraft ?name)))

;;
;; select the current runway
;;

(primitive 
 (index (select-runway ?airport-code ?runway-number))
 (return (select-runway ?airport-code ?runway-number)))


;  ;; get current runway magnetic heading
 
;  (step (compute-runway-heading-magenetic-test1 => ?rw-heading))
 
;  ;; select traffice pattern, this & lat/long trigger pattern measurments
 
;  (step (select-traffic-pattern 
;         +current-runway+ +current-aircraft+ ?direction ?rw-heading))


;;
;; select the current traffic pattern
;;

(primitive 
 (index (select-traffic-pattern ?runway ?aircraft ?direction ?heading-mag))
 (return (select-traffic-pattern ?runway ?aircraft ?direction ?heading-mag)))

;;
;; deselect the current traffic pattern
;;

(primitive 
 (index (deselect-traffic-pattern))
 (on-start (deselect-traffic-pattern)))

;;
;; set master engine throttle posisiont (0.00 - 1.00)
;;

(primitive
 (index (set-throttle ?setting))
 (on-start (let ((newset (eval ?setting)))
	     (loop for eng from 1 to (engine-count (current-aircraft nil))
               for var in '(thro1 thro2 thro3 thro4 thro5 thro6 thro7 thro8)
               do (xplane-set-variable-verified var newset)))))

;;
;; set flap position (0.00 - 1.00)
;;

(primitive
 (index (set-flaps ?setting))
 (on-start (xplane-set-variable 'flap-reqst ?setting)))

;;
;; release brakes
;;

(primitive
 (index (release-brakes))
 (on-start (xplane-set-variable 'wbrak 0.0)))

;;
;; set brakes a given amount
;;

(primitive
 (index (set-brakes ?value))
 (on-start (xplane-set-variable 'wbrak ?value)))

;;
;; raise the gear
;;

(primitive
 (index (raise-gear))
 (on-start (xplane-set-variable 'gear 0.0)))

;;
;; lower the gear
;;

(primitive
 (index (lower-gear))
 (on-start (xplane-set-variable 'gear 1.0)))

;;
;; set nav1 frequency
;;

(primitive
 (index (set-nav1 ?freq))
 (on-start (xplane-set-variable 'nav1-freq ?freq)))

;;
;; set nav2 frequency
;;

(primitive
 (index (set-nav2 ?freq))
 (on-start (xplane-set-variable 'nav2-freq ?freq)))

;;
;; set obs1 heading
;;

(primitive
 (index (set-obs1 ?heading))
 (on-start (xplane-set-variable 'nav1-obs ?heading)))

;;
;; set obs2 heading
;;

(primitive
 (index (set-obs2 ?heading))
 (on-start (xplane-set-variable 'nav2-obs ?heading)))


;;
;; turn to the correct heading based on leg and traffice patern direction
;;

(procedure :seq
 (index (turn ?leg ?runway-heading ?direction))
 (step (compute-leg-heading ?runway-heading ?leg ?direction => ?leg-heading))
 (step (autopilot-hold heading ?leg-heading))
 (step (inform (turned ?leg))))

;;
;; compute heading of traffic pattern leg
;;

(primitive
 (index (compute-leg-heading ?runway-heading ?leg ?direction))
 (return (compute-leg-heading ?runway-heading ?leg ?direction)))

;;
;; compute heading of traffic pattern leg
;;

(defun compute-leg-heading (runway-heading leg pattern-direction)
  
  ;; establish the delta heading for this direction type
  
   (let ((delta (cond ((eq leg 'down  )  180)
                      ((eq leg 'up    )    0)
                      ((eq leg 'cross )  (if (eq pattern-direction 'left) 270 90))
                      ((eq leg 'base  )  (if (eq pattern-direction 'left) 90 270))
                      ((eq leg 'extend)  (if (eq pattern-direction 'left) 90 270))
                      (t (error "Unknow leg: ~a~%" leg)))))
     
     ;; compute actual heading from runway-heading and delta
     
     (compute-delta-heading runway-heading delta)))

;;
;; compute a change in heading
;;

(primitive 
 (index (compute-delta-heading ?heading ?delta))
 (return (compute-delta-heading ?heading ?delta)))
 
;;
;; compute a change in heading
;;

(defun compute-delta-heading (heading delta)
  (mod (+ (float heading) (+ 360 (float delta))) 360))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;; camera view operations ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *view-key-lookup-table*
    '((cockpit     #\w)
      (circle      #\|)
      (spot        #\@)
      (moving-spot #\c)
      (from-tower  #\t)
      (from-runway #\#)       
      ))

;;
;; selecte xplane camera view
;;

(primitive 
 (index (xplane-camera-view ?view)) 
 (on-start 
  (let ((key (lookup ?view *view-key-lookup-table*)))
    (if key (xplane-keypress (car key))
      (error "unknow view: ~a" ?view)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; misc other primitives ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; send cogevent to agent
;;

(primitive
 (index (inform ?event))
 (on-start (inform ?event)))

;; print a message on on the screen

(primitive
  (index (xplane-print ?message))
;;  (on-start (format t  "XPLANE MESSAGE: ~a~%" ?message))
  )

;;
;; send key to xplane
;;

(primitive
 (index (xplane-keypress ?key))
 (on-start (xplane-keypress ?key)))

;;
;; generate cogevent containing field value from a given object
;; also return the same value
;;

(primitive 
 (index (ask ?object for ?field))
 (return (let ((value (eval (list ?field ?object))))
           (inform (list ?field value))
           value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        ;;;;
;;;; x-plane lisp interface ;;;;
;;;;                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; global variables ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

;; xplane communications data strucutre

(defvar *xplane-comm-port* nil)

;; this variable removes the last chunk of data from a received 
;; udp packet to repair a bug which appends extra data

(defparameter *remove-last-channel-data* nil)

;; active xplane variable measurements

(defparameter *active-variables* nil)

;; aircraft object designation

(defparameter *aircraft-object* 'ac1)

;; list of available aircraft types

(defparameter *xplane-aircraft-db* nil)

;; list of available airports

(defparameter *xplane-airports-db* nil)

;; values used to indicate that this variable should be ignored
;; when setting some other variable in a particular channel

(defparameter +ignore-int+ -999)
(defparameter +ignore-float+ -999.0)

;; xplane application state

(defparameter *xplane-app-inited*  nil)
(defparameter *xplane-app-running* nil)

;; varible names, data types and numbers for xplane channels

(defparameter *channel-signatures*
    '((time         1 ((elaps-time :float) (timer-time :float) (local-time :float) (zulu-time  :float) (                 ) (                  ) (                 ) (pause       :int  )))
      (velocity     2 ((vtrue-ktas :float) (vind-kias  :float) (vtrue-mph  :float) (vind-mph   :float) (vvi-fpm    :float) (                  ) (vtotl-ktas :float) (vtotl-mph   :float)))
      (joystick     7 ((elev-yoke  :float) (ailrn-yoke :float) (ruddr-yoke :float) (sweep-rqst :float) (vect-rqst  :float) (                  ) (                 ) (                  ))) 
      (flaps       11 ((trim-elev  :int  ) (trim-ailrn :float) (trim-ruddr :float) (trim-rotor :float) (flap-reqst :float) (flap-postn  :float) (slat-ratio :float) (sbrak-ratio :float)))
      (gear	   12 ((gear       :int  ) (wbrak      :float) (lbrak      :float) (rbrak      :float) (                 ) (                  ) (                 ) (                  )))
      (orient      16 ((pitch-deg  :float) (roll-deg   :float) (hding-true :float) (hding-mag  :float) (mavar-deg  :float) (bug-mag     :float) (                 ) (                  )))
      (latlongalt  18 ((lat-deg    :float) (lon-deg    :float) (alt-ftmsl  :float) (alt-ftagl  :float) (                 ) (alt-ind     :float) (lat-south  :float) (lon-west    :float)))
      (xyz-dist    19 ((x-m        :float) (y-m        :float) (z-m        :float) (                 ) (                 ) (                  ) (dist-ft    :float) (dist-nm     :float)))
      (throttle	   23 ((thro1      :float) (thro2      :float) (thro3      :float) (thro4      :float) (thro5      :float) (thro6       :float) (thro7      :float) (thro8       :float)))
      (eng-power   31 ((power1-hp  :float) (power2-hp  :float) (                 ) (                 ) (                 ) (                  ) (                 ) (                  )))
      (ailerons1   65 ((lailn1     :float) (lailn2     :float) (lailn3     :float) (lailn4     :float) (lailn5     :float) (lailn6      :float) (lailn7     :float) (lailn8      :float)))
      (nav-freq    92 ((nav1-freq  :int  ) (nav1-stby  :int  ) (nav2-freq  :int  ) (nav2-stby  :int  ) (nav1-type  :int  ) (nav2-type   :int  ) (nav1-scrs  :float) (nav2-scrs   :float)))
      (nav-obs     93 ((nav1-obs   :float) (nav1-flag  :int  ) (nav2-obs   :float) (nav2-flag  :int  ) (                 ) (                  ) (                 ) (                  )))
      (nav-deflect 94 ((vor1-hdef  :float) (vor1-vdef  :float) (vor1-brg   :float) (vor1-dist  :float) (vor2-hdef  :float) (vor2-vdef   :float) (vor2-brg   :float) (vor2-dist   :float)))
      (switches2  101 ((ecam-mode  :int  ) (efis-sel1  :int  ) (efis-sel2  :int  ) (hsi-sel1   :int  ) (hsi-sel2   :int  ) (hsi-arc     :int  ) (rm-sel     :int  ) (map-range   :int  )))
      (switches3  102 ((ap-src     :int  ) (fdir-mode  :int  ) (fdir-pitch :int  ) (fdir-roll  :int  ) (                 ) (hud-power   :int  ) (hud-brite  :int  ) (                  )))
      (ap-mode    109 ((mode-speed :int  ) (mode-hding :int  ) (mode-alt   :int  ) (mode-hnav  :int  ) (mode-vnav  :int  ) (mode-backc  :int  ) (                 ) (                  )))
      (ap-set     110 ((set-speed  :float) (set-hding  :float) (set-vvi    :float) (dial-alt   :float) (set-alt    :float) (fms-alt     :float) (set-pitch  :float) (                  )))
      (failures   118 ((fail-grp1  :float) (fail-grp2  :float) (fail-grp3  :float) (fail-grp4  :float) (fail-grp5  :float) (fail-grp6   :float) (fail-grp7  :float) (fail-grp8   :float)))
      (vspeeds    120 ((vso-ktas   :float) (vs-ktas    :float) (vfe-ktas   :float) (vle-ktas   :float) (vno-ktas   :float) (vne-ktas    :float) (mmo-ktas   :float) (                  )))
      ))

;; packet signature look up table

(defparameter *packet-signatures*
    '(("PAPT" 4 :int :int :int :int)
      ("VEHN" :int 150)))


;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; udp packet class ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass packet ()
  (
   ;; packet size
   
   (size :accessor size :initarg :size :initform 200)
   
   ;; packet payload
   
   (payload :accessor payload :initarg :payload :initform 
            (make-array 200 :element-type '(unsigned-byte 8) :initial-element 0))
     
   ;; insertion/extraction point for data in packet
     
   (at :accessor at :initarg :at :initform 0)))

;;
;; print method for packet
;;

(defmethod print-object ((p packet) stream)
  (format stream "{~a}" (packet-decode-by-signature p)))

;;
;; decode packet based on packet signatures
;;

(defmethod packet-decode-by-signature ((p packet))
  
  ;; record the at position
  
  (let ((tmp-at (at p))
        (decoded-data nil))
    
    ;; set at positon to 0
    
    (setf (at p) 0)
    
    ;; get packet type and index
    
    (let* ((type (extract-string p 4))
           (index  (extract-byte p))
           (sig    (lookup-packet-signature type)))
           
      ;; if signature found, decode using that
      
      (setf decoded-data 
        (if sig
            (loop for item in (cdr sig)
                collect (cond
                         
                         ;; a string is indicated by it's length
                         
                         ((numberp item) (extract-string p item))

                         ;; an intiger
                         
                         ((eq item :int) (extract-int p))
                         
                         ;; a float
                         
                         ((eq item :float) (extract-float p))
                         
                         ;; a double
                         
                         ((eq item :double) (extract-double p))
                         
                         ;; a char
                         
                         ((eq item :char) (extract-char p))
                         
                         ;; a byte
                         
                         ((eq item :byte) (extract-byte p))
                         
                         ;; an alligned byte
                         
                         ((eq item :a-byte) (extract-alligned-byte p))
                         
                         ;; unknow type generate error 
                         
                         (t (error "unkown packet element: ~a" item))))

          ;; else use default decoding
          
          (loop for i from (at p) to (1- (size p))
              collect (aref (payload p) i))))
    
      ;; restore origonal at positon
      
      (setf (at p) tmp-at)
    
      ;; return collected data
    
      (append (list type index) decoded-data))))
    

;;
;; lookup packet signature 
;;

(defun lookup-packet-signature (type)
  (loop for signature in *packet-signatures* 
      do (if (equal type (car signature))
             (return-from lookup-packet-signature signature)))
  nil)

;;
;; send a packet
;;

(defmethod packet-send ((p packet) port)
;;  (format t "sending:" (payload p))  
;;  (loop for i from 0 to (1- (at p)) do
;;        (format t " ~a" (aref (payload p) i)))
;;  (format t "~%")
;;  (format t "sent: ~a~%" (at p))
  (socket:send-to port (payload p) (at p))

  ;; pause breifly to let the packet go on it's way

  (sleepless-delay 0.008))

;;
;; delay a small amount of time without using sleep
;; which can't sleep for periods smaller then
;; 0.076 seconds
;;

(defun sleepless-delay (seconds)
  (let ((ms (floor (* seconds 1000)))
        (start (current-milliseconds)))
    (while (< (- (current-milliseconds) start) ms))))


;;
;; receive packet
;;

(defun packet-receive (port)
  (multiple-value-bind (data length)
      (socket:receive-from port 1024 :extract t)
    (make-instance 'packet :size length :payload data :at 0)))

;;
;; receive and decode packet
;;

(defun packet-receive-decode (port)
  (packet-decode (packet-receive port)))

;;
;; decode arriving xplane data
;;

(defmethod packet-decode ((p packet))
  
  ;; get the command string pluse the 1 extra byte (we would dispatch
  ;; on the command string to cope with multipe packet types)
  
  (extract-array p 5)
  
  ;; while the packet contains data, collect the channel
  ;; id number and the 8 values for that channel
    
  (let ((data (loop while (not (packet-empty? p))
                    collect (list (extract-int p)
                                  (loop for i from 1 to 8
                                      collect (extract-float p))))))
    
    ;; if we need to, remove the last item (because we get an extra one)
    
    (if *remove-last-channel-data*
        (remove (car (last data)) data)
      data)))
    
;;
;; is the packet empty of data
;;

(defmethod packet-empty? ((p packet))
  (>= (at p) (size p)))

;;
;; append an intiger to a packet
;;

(defmethod append-int ((p packet) int)
  (let  ((array (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (ff:fslot-value-typed :int :lisp array) (floor int))
    (append-array p (endian-swap4 array))))

;;
;; append a float to a packet
;;

(defmethod append-float ((p packet) float)
  (let  ((array (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (ff:fslot-value-typed :float :lisp array) (float float))
    (append-array p (endian-swap4 array))))

;;
;; append a double float to a packet
;;

(defmethod append-double ((p packet) double)
  (let  ((array (make-array 8 :element-type '(unsigned-byte 8))))
    (setf (ff:fslot-value-typed :double :lisp array) (to-double-float double))
    (append-array p array)))
;;
;; append a string to a packet
;;

(defmethod append-string ((p packet) string &optional (pad nil))
  (let ((start-at (at p)))
    (loop for i from 0 to (1- (length string))
        do (append-char p (char string i)))
    (append-byte p 0)
    (when pad 
      (while (> pad (- (at p) start-at))
        (append-byte p 0))
      (while (< pad (- (at p) start-at))
        (decf (at p))))))
      
;;
;; append a string (without a null) to a packet
;;

(defmethod append-string-no-null ((p packet) string &optional (pad nil))
  (loop for i from 0 to (1- (length string))
      do (append-char p (char string i)))
  (when pad 
    (loop for i from 0 to (- pad (length string))
        do (append-byte p 0))))

;;
;; append an array of bytes to a packet
;;

(defmethod append-array ((p packet) array)
  (loop for i from 0 to (1- (length array))
      do (append-byte p (aref array i))))
;;
;; append a 4 byte alligned byte
;;

(defmethod append-alligned-byte ((p packet) byte)
  (let  ((array (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref array 0) byte)
    (setf (aref array 1) 0   )
    (setf (aref array 2) 0   )
    (setf (aref array 3) 0   )
    (append-array p (endian-swap4 array))))

;;
;; append a character to a packet
;;

(defmethod append-char ((p packet) char)
  (setf (aref (payload p) (at p)) (char-code char))
  (incf (at p)))

;;
;; append a byte to a packet
;;

(defmethod append-byte ((p packet) byte)
  (setf (aref (payload p) (at p)) byte)
  (incf (at p)))

;;
;; extract an intiger from a packet
;;

(defmethod extract-int ((p packet))
  (ff:fslot-value-typed :int :lisp (endian-swap4 (extract-array p 4))))

;;
;; extract a float from a packet
;;

(defmethod extract-float ((p packet))
  (ff:fslot-value-typed :float :lisp (endian-swap4 (extract-array p 4))))

;;
;; extract a double float from a packet
;;

(defmethod extract-double ((p packet))
  (ff:fslot-value-typed :double :lisp (extract-array p 8)))

;;
;; extract a string from a packet
;;

(defmethod extract-string ((p packet) &optional (pad nil))
  
  ;; exstract characters until null or pad length reached
  
  (let* ((chars (loop for length from 0
                    while (or (not pad) (< length pad))
                    as char = (extract-char p)
                    while (not (= (char-code char) (char-code #\null)))
                    collect char))
         (string (make-string (length chars))))
    
    ;; map chars into string
    
    (loop for char in chars
        for i from 0 to (1- (length chars))
        do (setf (char string i) char))
    
    ;; return string
    
    string))

;;
;; extract an array of bytes from a packet
;;

(defmethod extract-array ((p packet) size)
  (let  ((array (make-array size :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (1- size)
        do (setf (aref array i) (extract-byte p)))
    array))

;;
;; extract a 4 byte alligned byte
;;

(defmethod extract-alligned-byte ((p packet))
  (let ((byte (extract-byte p)))
    (extract-byte p)
    (extract-byte p)
    (extract-byte p) 
    byte))

;;
;; extract a character from a packet
;;

(defmethod extract-char ((p packet))
  (code-char (extract-byte p)))

;;
;; extract a byte from a packet
;;

(defmethod extract-byte ((p packet))
  (aref (payload p) (1- (incf (at p)))))

;;
;; packet sniffer
;;

(defun packet-sniffer (ip &key include exclude)
  
  ;; close any existing ports
  
  (when *xplane-comm-port*
    (close *xplane-comm-port*))
  
  ;; reopen the port to desired ipaddress
  
  (setf *xplane-comm-port*
    (socket:make-socket 
     :address-family :internet 
     :type :datagram
     :local-port 49001
     :remote-host "143.232.72.162"
     :remote-port 49000
     :reuse-address t))

  ;; run forever
  
  (while t
    
    ;; get at decode the data
    
    (let* ((p  (packet-receive *xplane-comm-port*))
           (pd (packet-decode-by-signature p)))
      
      ;; if there is a filter, print only desired packets
      
      (if (and (or (not include)
                   (xplane-member (car pd) include))
               (or (not exclude)
                   (not (xplane-member (car pd) exclude))))
          (format t "~a~%[~a]: ~a~%" pd (size p) (payload p))))))


(defun xplane-member (item list)
  (loop for element in list 
      if (equal element item)
      do (return-from xplane-member t))
  nil)
                    
      

;;
;; change endian of 4 byte value only if nessecery on this os
;;

#+big-endian    (defun endian-swap4 (chunk) chunk)
#+little-endian (defun endian-swap4 (chunk)
                  (rotatef (aref chunk 0) (aref chunk 3))
                  (rotatef (aref chunk 1) (aref chunk 2))
                  chunk)

;;;;;;;;;;;;;
;;         ;;
;; airport ;;
;;         ;;
;;;;;;;;;;;;;

(defclass airport ()
  
  ;; elevation of aiprot in feet above msl
   
  ((elevation    :accessor elevation    :initarg :elevation    :initform nil)

   ;; airport identifiers
   
   (code         :accessor code         :initarg :code         :initform nil)
   (name         :accessor name         :initarg :name         :initform nil)
   
   ;; autolandable runways
   
   (runways      :accessor runways      :initarg :runways      :initform nil)
  
   ;; traffic pattern altitude 

   (tpa          :accessor tpa          :initarg :tpa          :initform nil)))

;;
;; airport print method
;;

(defmethod print-object ((ob airport) s)
  (format s "#{AIRPORT: ~a ~a}" (code ob) (name ob)))
   
;;;;;;;;;;;;
;;        ;;
;; runway ;;
;;        ;;
;;;;;;;;;;;;

(defclass runway ()
  
  ;; locatoin of center of aircraft
  
  ((latitude     :accessor latitude     :initarg :latitude     :initform nil)
   (longitude    :accessor longitude    :initarg :longitude    :initform nil)
   
   ;; index of this runway and weather or not this is a reverse
   
   (index        :accessor index        :initarg :index        :initarg nil)
   (backward     :accessor backward     :initarg :backward     :initarg nil)
   
   ;; airport associated with this runway

   (airport      :accessor airport      :initarg :airport      :initform nil)

   ;; runway number (16x or 32L)
   
   (runway-no    :accessor runway-no    :initarg :runway-no    :initform nil)

   ;; heading degrees true
   
   (heading-true :accessor heading-true :initarg :heading-true :initform nil)

   ;; length in feet
   
   (rw-length    :accessor rw-length    :initarg :rw-length    :initform nil)   

   ;; width in feet 
   
   (rw-width     :accessor rw-width     :initarg :rw-width     :initform nil)

   ;; ils frequency
   
   (ils-freq     :accessor ils-freq     :initarg :ils-freq     :initform nil)))

;;
;; runway print method
;;

(defmethod print-object ((ob runway) s)
  (format s "#{RUNWAY: ~a ~a}" (code (airport ob)) (runway-no ob)))

;;;;;;;;;;;;;;;;;;;;;
;;                 ;;
;; traffic pattern ;;
;;                 ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass traffic-pattern ()
  
  ;; runway associated with this traffic pattern
  
  ((runway      :accessor runway      :initarg :runway      :initform nil)
   
   ;; magnetic heading of runway
   
   (runway-heading :accessor runway-heading :initarg :runway-heading :initform nil)
   
   ;; magnetic heading of traffic pattern legs
   
   (up-heading     :accessor up-heading     :initarg :up-heading     :initform nil)
   (cross-heading  :accessor cross-heading  :initarg :cross-heading  :initform nil)
   (down-heading   :accessor down-heading   :initarg :down-heading   :initform nil)
   (base-heading   :accessor base-heading   :initarg :base-heading   :initform nil)
   (extend-heading :accessor extend-heading :initarg :extend-heading :initform nil)
   
   ;; center line of runway
   
   (center-line :accessor center-line :initarg :center-line :initform nil)

   ;; type (left or right traffic pattern)
   
   (direction   :accessor direction   :initarg :direction    :initform nil)
   
   ;; traffic pattern region lines
   
   (regions     :accessor regions    :initarg :regions       :initform nil)))

;;
;; traffic-pattern print method
;;

(defmethod print-object ((ob traffic-pattern) s)
  (format s "#{PATTERN: ~a ~a}" (code (airport (runway ob))) (runway-no (runway ob))))

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; aircraft type  ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

(defclass aircraft-type ()
  
  ;; basic aircraft type information
  
  ((make          :accessor make          :initarg :make          :initform nil)
   (model         :accessor model         :initarg :model         :initform nil)
   (name          :accessor name          :initarg :name          :initform nil)
   (path          :accessor path          :initarg :path          :initform nil)

   ;; powerplant specifications
   
   (engine-count  :accessor engine-count  :initarg :engine-count  :initform nil)
   (horse-power   :accessor horse-power   :initarg :horse-power   :initform nil)

   ;; takeoff related values

   (flaps-takeoff :accessor flaps-takeoff :initarg :flaps-takeoff :initform nil)
   (ias-rotate    :accessor ias-rotate    :initarg :ias-rotate    :initform nil)
   (agl-gear-up   :accessor agl-gear-up   :initarg :agl-gear-up   :initform nil)
   (climb-rate    :accessor climb-rate    :initarg :climb-rate    :initform nil)
   
   ;; cruise related values
   
   (ias-cruise    :accessor ias-cruise    :initarg :ias-cruise    :initform nil)
   (ias-pattern   :accessor ias-pattern   :initarg :ias-pattern   :initform nil)

   ;; landing related valuese
   
   (flaps-land    :accessor flaps-land    :initarg :flaps-land    :initform nil)   
   (ias-appraoch  :accessor ias-appraoch  :initarg :ias-appraoch  :initform nil)
   (ias-final     :accessor ias-final     :initarg :ias-final     :initform nil)
   (agl-flare     :accessor agl-flare     :initarg :agl-flare     :initform nil)
   (vvi-flare     :accessor vvi-flare     :initarg :vvi-flare     :initform nil)
   (ias-flare     :accessor ias-flare     :initarg :ias-flare     :initform nil)
   (ias-brake     :accessor ias-brake     :initarg :ias-brake     :initform nil)
   (tp-radius     :accessor tp-radius     :initarg :tp-radius     :initform nil)
   ))

;;
;; aircraft type print method
;;

(defmethod print-object ((ob aircraft-type) s)
  (format s "#{AIRCRAFT-TYPE: ~a ~a}" (model ob) (name ob)))

;; init aircraft and airports

(defun init-db ()
  
  ;; KLAX
  
  (let* ((klax-07l (make-instance 'runway
                :latitude     33.937849
                :longitude    -118.399554
                :index        0
                :backward     nil
                :runway-no    "07L"
                :heading-true 82.97
                :rw-length    12057
                :rw-width     150
                :ils-freq     11110))

         (klax-07r (make-instance 'runway
                :latitude     33.935653
                :longitude    -118.400885
                :index        1
                :backward     nil
                :runway-no    "07R"
                :heading-true 82.98
                :rw-length    11065
                :rw-width     200
                :ils-freq     10990))
         
         (klax-06r (make-instance 'runway
                :latitude     33.948466
                :longitude    -118.418493
                :index        2
                :backward     nil
                :runway-no    "06R"
                :heading-true 82.96
                :rw-length    10256
                :rw-width     150
                :ils-freq     11170))
         
         (klax-06l (make-instance 'runway
                :latitude     33.950604
                :longitude    -118.416550
                :index        3
                :backward     nil
                :runway-no    "06L"
                :heading-true 82.96
                :rw-length    8898
                :rw-width     150
                :ils-freq     10850))

         (klax (make-instance 'airport
                 :elevation    126
                 :code         "KLAX"
                 :name         "Los Angeles Intl"
                 :runways      (list klax-06l klax-06r klax-07l klax-07r)
                 :tpa          2000))

         ;; KONT

         (kont-08l (make-instance 'runway
                :latitude     34.056885
                :longitude    -117.602691
                :index        0
                :backward     nil
                :runway-no    "08L"
                :heading-true 89.98
                :rw-length    12165
                :rw-width     150
                :ils-freq     10970))
         
         (kont-26l (make-instance 'runway
                :latitude     34.054963
                :longitude    -117.599391
                :index        1
                :backward     t
                :runway-no    "26L"
                :heading-true 269.98
                :rw-length    10173
                :rw-width     150
                :ils-freq     111.35))
         
         (kont (make-instance 'airport
                 :elevation    944
                 :code         "KONT"
                 :name         "Ontario Intl"
                 :runways      (list kont-08l kont-26l)
                 :tpa          2000))
    
         ;; KPOC

         (kpoc-26l (make-instance 'runway
                :latitude     34.091340
                :longitude    -117.782522
                :index        0
                :backward     t
                :runway-no    "26L"
                :heading-true 273.21
                :rw-length    4825
                :rw-width     75
                :ils-freq     11050))
         
         (kpoc (make-instance 'airport
                 :elevation    1011
                 :code         "KPOC"
                 :name         "Brackett Fld"
                 :runways      (list kpoc-26l)
                 :tpa          2000))
    
         ;; KSBD

         (ksbd-06 (make-instance 'runway
                :latitude     34.095353
                :longitude    -117.234874
                :index        0
                :backward     nil
                :runway-no    "06"
                :heading-true 70.36
                :rw-length    9977
                :rw-width     180
                :ils-freq     10930))
         
         (ksbd (make-instance 'airport
                 :elevation    1157
                 :code         "KSBD"
                 :name         "San Bernardino Intl"
                 :runways      (list ksbd-06)
                 :tpa          2000))
    
         ;; KRAL
         
         (kral-09 (make-instance 'runway
                :latitude     33.952213
                :longitude    -117.443299
                :runway-no    "09"
                :heading-true 103.12
                :rw-length    5386
                :rw-width     100
                :ils-freq     11090))
         (kral (make-instance 'airport
                 :elevation    818
                 :code         "KRAL"
                 :name         "Riverside Muni"
                 :runways      (list kral-09)
                 :tpa          1800)))
    
         ;; set runway airports
         
    (setf (airport klax-07l) klax)
    (setf (airport klax-07r) klax)
    (setf (airport klax-06r) klax)
    (setf (airport klax-06l) klax)
    (setf (airport kont-26l) kont)
    (setf (airport kont-08l) kont)
    (setf (airport kpoc-26l) kpoc)
    (setf (airport ksbd-06) ksbd)
    (setf (airport kral-09) kral)
    
    ;; config airports
    
    (setf *xplane-airports-db* (list ksbd kpoc kont kral klax))
    
    ;; confige aircraft pool
    
    (setf *xplane-aircraft-db* 
         (list
          (make-instance 'aircraft-type
            :make          "Raytheon"
            :model         "B200"
            :name          "King Air"
            :path          "Aircraft/General Aviation/King Air B200/King Air B200.acf"
            
            :engine-count  2
            :horse-power   850
            
            :flaps-takeoff 0
            :ias-rotate    80
            :climb-rate    500
            :agl-gear-up   50
            
            :ias-cruise    240
            :ias-pattern   130
            
            :flaps-land    0.5
            :ias-appraoch  100
            :ias-final     80
            :agl-flare     10
            :vvi-flare     100
            :ias-flare     50
            :ias-brake     20
            :tp-radius     2500
            )
          
          ;; altair b-er
          
          (make-instance 'aircraft-type
            :make          "General Atomics"
            :model         "B-ER"
            :name          "Altair"
       
            :engine-count  1
            :horse-power   750
            
            :flaps-takeoff 0.6667
            :ias-rotate    50
            :climb-rate    1000
            :agl-gear-up   30
            
            :ias-cruise    240
            :ias-pattern   150
            
            :flaps-land    1
            :ias-appraoch  100
            :ias-final     90
            :agl-flare     20
            :vvi-flare     450
            :ias-flare     25
            :ias-brake     20
            :tp-radius     5000
            )))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; indexical functions ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; indexical for current aircraft
;;

(defun current-aircraft (task) *current-aircraft*)
(defparameter *current-aircraft* nil)

;;
;; indexical for current airport
;;

(defun current-airport (task) *current-airport*)
(defparameter *current-airport* nil)

;;
;; indexical for current runway
;;

(defun current-runway (task) *current-runway*)
(defparameter *current-runway* nil)

;;
;; indexical for current traffic pattern
;;

(defun current-pattern (task) *current-pattern*)
(defparameter *current-pattern* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; xplane time functions ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *elapsed-time* 0)
(defvar *elapsed-time-sample-time* 0)

;;
;; get the current simulation time
;;

;(defun current-time ()
;  (+ *elapsed-time* (- (current-milliseconds) *elapsed-time-sample-time*)))

;;
;; record an elapsed time sample
;;

(defun record-elapsed-time-sample (et)
  (setf *elapsed-time-sample-time* (current-milliseconds))
  (setf *elapsed-time* (floor (* 1000 et))))

;;
;; init simulation time
;;
  
(defun xplane-init-sim-time ()
  (record-elapsed-time-sample (xplane-get-variable-value 'elaps-time))
  (xplane-subscribe-variable 'elaps-time))

;;
;; pause xplane
;;

(defun xplane-pause-sim-time ()
  (xplane-set-variable 'pause 1))

;;
;; unpause xplane
;;

(defun xplane-unpause-sim-time ()
  (xplane-set-variable 'pause 0))

(trace xplane-pause-sim-time)
(trace xplane-unpause-sim-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;; channel and variable lookup ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; lookup channel number and variable type
;;

(defun xplane-variable-lookup  (channel variable)
  (let* ((chinfo (lookup channel *channel-signatures*))
         (vtype (lookup variable (nth 1 chinfo))))
    (list (car chinfo) (car vtype))))

;;
;; given a variable name lookup channel information
;;

(defun xplane-channel-lookup (variable)
  (loop for channel in *channel-signatures* do
        (loop for (var type) in (nth 2 channel) do
              (if (eq var variable) 
                  (return-from xplane-channel-lookup channel))))
  (error "unknown xplane variable: ~a~%" variable))

;;
;; given a channel id lookup channel information
;;

(defun xplane-channel-lookup-by-id (id &key (supress-error nil))
  (loop for channel in *channel-signatures* do
        (if (eq id (nth 1 channel))
            (return-from xplane-channel-lookup-by-id channel)))
  (if supress-error 
      nil
    (error "unknown xplane channel: ~a~%" id)))
;;
;; given a channel name lookup channel information
;;

(defun xplane-channel-lookup-by-name (name &key (supress-error nil))
  (loop for channel in *channel-signatures* do
        (if (eq name (nth 0 channel))
            (return-from xplane-channel-lookup-by-name channel)))
  (if supress-error 
      nil
    (error "unknown xplane channel: ~a~%" name)))
;;
;; lookup channel signature and convert it into a list of types
;;

(defun xplane-channel-types (channel-id)
  (mapcar (lambda (x) (if (nth 1 x) (nth 1 x) :float)) 
          (nth 2 (xplane-channel-lookup-by-id channel-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;; low level commands to xplane  ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; send a key press to x-plane
;;

(defun xplane-keypress (key)

  ;; create the packet
  
  (let* ((packet (make-instance 'packet)))
         
    ;; append the correct command
    
    (append-string packet "CHAR")
    
    ;; append the channel number
    
    (append-char packet key)
        
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))

;;
;; set network configuration
;;

(defun xplane-configure-network-io (index ip-address port enable)

  ;; create the packet
  
  (let* ((packet (make-instance 'packet)))
         
    ;; append the correct command
    
    (append-string packet "ISET")
    
    ;; append index
    
    (append-int packet index)
    
    ;; append ip-address
    
    (append-string packet ip-address 16)
    
    ;; append ip-address
    
    (append-string packet port 8)
    
    ;; append enable disable
    
    (append-int packet (if enable 1 0))
    
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))

;;
;; set airport
;;

(defun xplane-place-aircraft-at-airport (airport-code taxi-or-runway-no backward)

  ;; create the packet
  
  (let* ((packet (make-instance 'packet)))
         
    ;; append the correct command
    
    (append-string packet "PAPT")
    
    ;; append airport code
    
    (append-string packet airport-code 8)
    
    ;; append placement type
    
    (append-int packet 601)
    
    ;; append runway or ramp number
    
    (append-int packet taxi-or-runway-no)
    
    ;; append direction
    
    (append-int packet (if backward 1 0))
    
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))


;;
;; load a particular aircraft
;; (xplane-load-aircraft "Aircraft/General Aviation/King Air B200/King Air B200.acf"))
;; (xplane-load-aircraft "Aircraft/General Aviation/Cessna 172SP/Cessna 172SP.acf"))
;;

(defun xplane-load-aircraft (aircraft-id aircraft-path)

  ;; create the packet
  
  (let* ((packet (make-instance 'packet)))
         
    ;; append the correct command
    
    (append-string packet "VEHN" 4)
    (append-byte packet 21)
    
    ;; assume my plane
    
    (append-int packet aircraft-id)
    
    ;; append path to aircraft
    
    (append-string packet aircraft-path 152)
    
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))

;;
;; fail a system, equipment starts at #326
;;

(defun xplane-fail-system (number)

  ;; create the packet
  
  (let* ((packet (make-instance 'packet)))
         
    ;; append the correct command
    
    (append-string packet "FAIL")
    
    ;; append the channel number
    
    (append-string packet (format nil "~a" number))
        
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))

;;
;; recover system
;;

(defun xplane-recover-system (number)

  ;; create the packet
  
  (let* ((packet (make-instance 'packet)))
         
    ;; append the correct command
    
    (append-string packet "RECO")

    ;; append the channel number
    
    (append-string packet (format nil "~a" number))
        
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))


;;
;; set the channel state in x-plane either to send or not send data
;;

(defun xplane-set-channel (id command)

  ;; create the packet
  
  (let* ((packet (make-instance 'packet))
         
         ;; lookup the channel number if not provided
         
         (channel (if (numberp id) id (xplane-channel-lookup id))))
    
    ;; append the correct command
    
    (append-string packet command)
    
    ;; append the channel number
    
    (append-alligned-byte packet channel)
        
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))
;;
;; subscribe to a channel
;;

(defun xplane-subscribe (channel-id)
  (xplane-set-channel channel-id "DSEL"))

;;
;; unsubscribe from a channel
;;

(defun xplane-unsubscribe (channel-id)
  (xplane-set-channel channel-id "USEL"))

;;
;; activate an xplane variable for measurement
;;

(defun xplane-subscribe-variable (variable)
  
  ;; look up the channel information and subscribe the to the channel
  
  (xplane-subscribe (nth 1 (xplane-channel-lookup variable)))

  ;; add variable name to the active list
  
  (push variable *active-variables*))

;;
;; deactivate an xplane variable for measurement
;;

(defun xplane-unsubscribe-variable (variable)
  (setf *active-variables* (remove-once variable *active-variables*)))

;;
;; remove  only the first instance of an item from a list
;;

(defun remove-once (item list)
  (cond ((not list) nil)
        ((eql item (car list)) (cdr list))
        (t (cons (car list) (remove-once item (cdr list))))))

;;
;; test to see if a variable is subscribed to
;;

(defun xplane-variable-subcribed? (variable)
  (find variable *active-variables*))

;;
;; set and verify variable value in xplane, this only works 
;; for variables which are not modifed when set, when the 
;; autopilot altitude is set it's moded by 100, and this
;; function will never terminate in that case
;;

(defun xplane-set-variable-verified (variable value)
  
  ;; init verified false
  
  (let ((verified nil)
        
        ;; get the channel signature by name
        
        (ch-sig (xplane-channel-lookup variable)))
    
    ;; subscribe to the variable (what happens if this fails?)
    
    (xplane-subscribe-variable variable)
    
    ;; while we have not verified the value
  
    (while (not verified)
      
      ;; set the value
      
      (xplane-set-variable variable value)
      
      ;; wait for the next packet to come back from xplane

      (loop while (not verified)
          for i from 1 to 5 do
            
            ;; decode the data into channel information
            
            (let ((cdata (car (lookup (nth 1 ch-sig)
                                      (packet-receive-decode *xplane-comm-port*)))))
              
              ;; loop through the variables in the channel
              
              (loop while (not verified)
                  for (var type) in (nth 2 ch-sig)
                  for val in cdata do
                    (setf verified (and (eq var variable) 
                                        (xplane-eql val value type))))))))

    ;; unsubscribe from the variable
    
    (xplane-unsubscribe-variable variable))

;;
;; test that two xplane variables are equal based on type
;;

(defun xplane-eql (v1 v2 type)
  (cond ((eq type :float) (eql (float v1) (float v2)))
        ((eq type :int)   (eql (floor (round v1)) (floor (round v2))))
        (t (error "Unknown type: ~a~%" type))))
    
;;
;; set variable value in xplane
;;

(defun xplane-set-variable (variable value)
  
  ;; look up the channel information
  
  (let ((ch-info (xplane-channel-lookup variable))
        
        ;; create the packet
        
        (packet (make-instance 'packet)))
    
    ;; add command type to packet
    
    (append-string packet "DATA")
    
    ;; append the channel number
    
    (append-int packet (nth 1 ch-info))
    
    ;; loop through the variables in the channel
    
    (loop for (var type) in (nth 2 ch-info) do

          ;; if this is a float variable
          
          (cond ((eq type :float) 
                 (append-float packet
                               
                               ;; if it's the target set value
                               ;; otherwise ignore it
                               
                               (if (eq var variable) 
                                    value
                                 +ignore-float+)))
                
                ;; if this is an int
                
                ((eq type :int) 
                 (append-int packet
                               
                             ;; if it's the target set value
                             ;; otherwise ignore it
                               
                             (if (eq var variable) 
                                 value
                               +ignore-int+)))
                
                ;; else wise just sick int info in
                
                (t (append-int packet +ignore-int+))))
    
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)
    
    ;; sleep for just a moment so we don't work the net too hard
    
    (sleep 0.01)))

;;
;; get the value of a variable now
;;

(defun xplane-get-variable-value (variable)
  
  ;; get the channel signature by name
  
  (let ((ch-sig (xplane-channel-lookup variable)))
    
    ;; subscribe to the variable
    
    (xplane-subscribe-variable variable)
    
    ;; wait for the next packet to come back from xplane
    
    (multiple-value-bind (data length) 
        (socket:receive-from *xplane-comm-port* 1024)
        
      ;; decode the data into channel information
        
      (let ((cdata (car (lookup (nth 1 ch-sig)
                                (packet-receive-decode *xplane-comm-port*)))))
          
        ;; loop through the variables in the channel
              
              (loop for (var type) in (nth 2 ch-sig)
                  for val in cdata do
                    
                    ;; when we've foudn the desired variable
                    
                    (when (eq var variable)
                      
                      ;; unsubscribe from the variable
                      
                      (xplane-unsubscribe-variable variable)
                      
                      ;; return the value of the variable
                      
                      (return-from xplane-get-variable-value val)))))))

;;
;; "shock" a channel so that when you set values on this channel
;; it won't cause xplane to relingquish controll of that system
;;

(defun xplane-shock-channel (channel-name)
  
  ;; look up the channel information
  
  (let ((ch-info (xplane-channel-lookup-by-name channel-name))
        
        ;; create the packet
        
        (packet (make-instance 'packet)))
    
    ;; add command type to packet
    
    (append-string packet "DATA")
    
    ;; append the channel number
    
    (append-int packet (nth 1 ch-info))
    
    ;; loop through the variables in the channel
    ;; and plug each one with -999.0
    
    (loop for (var type) in (nth 2 ch-info) do
          (append-float packet -999.0))
    
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)
    
    ;; sleep for just a moment so we don't work the net too hard
    
    (sleep 0.01)))

;;
;; place an aircraft at a given location in space
;;

(defun xplane-place-aircraft (aircraft-no latitude longitude elevation
                              psi theta phi
                              gear flap vect)
  
  ;; create the packet
  
  (let ((packet (make-instance 'packet)))

    ;; add command type to packet
    
    (append-string packet "VEH1")
    
    ;; append aircraft-no number (0 = main aircraf)
    
    (append-int packet aircraft-no)
    
    ;; place aircraft
    
    (append-double packet latitude)
    (append-double packet longitude)
    (append-double packet elevation)
    
    ;; pose aircraft
    
    (append-float packet psi)
    (append-float packet theta)
    (append-float packet phi)
    
    ;; configure aircraft
    
    (append-float packet gear)
    (append-float packet flap)
    (append-float packet vect)
    
    ;; send off the packet
    
    (packet-send packet *xplane-comm-port*)))

;;
;; receive and dispatch xplane packet data
;;

(defun xplane-receive ()
  ;;(sys:with-timeout (1)       
    (xplane-dispatch-measurements (packet-receive-decode *xplane-comm-port*)));;)
    

;;
;; decode arriving xplane data
;;

(defun xplane-decode-data (data length)
  
  ;; beginning of element 
  
  (let* ((start 0) 
         
         ;; get the command string
         
         (message (map 'string #'code-char (subseq data start (incf start 4))))
         
         ;; remove the zero
         
         (misc-index  (prog1 (aref data start) (incf start))))
    
    ;; return the parsed out the data for this channel
    
    (loop while (< start  ( - length 36)) 
                
                ;; get the channel index
                
        for id = (ff:fslot-value-typed :int :lisp (subseq data start (incf start 4)))
                 
                 ;; collect the channel number and the 8 valus
                 
        collect (list id
                      (loop for i from 1 to 8 
                          for type in (xplane-channel-types id)
                          collecting (ff:fslot-value-typed 
                                      type :lisp (subseq data start (incf start 4))))))))

;;
;; dispatch xplane measurments
;;

(defun xplane-dispatch-measurements (channels)
      
  ;; loop through data for each channel
      
  (loop for ch-data in channels do
            
        ;; get the channel signature (ignore unknow channels)
            
        (let ((ch-sig (xplane-channel-lookup-by-id (car ch-data) :supress-error t))
                  
              ;; variables used for traffic pattern info
                  
              (lat-deg nil)
              (lon-deg nil))
              
          ;; if we got a channel signature loop through the variables in the channel

          (when ch-sig
            (loop for (var type) in (nth 2 ch-sig)
                for value in (cadr ch-data) do
                  
                  ;; when this is an active variable
                  
                  (when (find var *active-variables*)
                    
                    ;; if we've got latitude or longitude record it
                    
                    (when (eq var 'lat-deg) (setf lat-deg value))
                    (when (eq var 'lon-deg) (setf lon-deg value))
                    
                    ;; generate measurment
                    
                    (inform `(,var ,*aircraft-object* = ,value)))))
            
          ;; when we've got  traffice pattern and lat/long
          ;; generate a traffic pattern status measurment
          
          (when (and lat-deg lon-deg *current-pattern*)
            (dispatch-pattern-status lon-deg lat-deg)))))

;;
;; init network connection to xplane
;;

(defun initialize-xplane-connection (&optional ip port)
  (setf *active-variables* nil)
  (when *xplane-comm-port*
    (close *xplane-comm-port*))
  (setf *xplane-comm-port* 
    (socket:make-socket 
     :address-family :internet 
     :type :datagram
     :local-port 49001
     :remote-host (or ip "127.0.0.1")
     :remote-port (or port 49000)
     :reuse-address t)))
  
  ;; tell x-plane to send us data (currently doesn't work as i would like)
  
  ;;(xplane-configure-network-io 23 "127.0.0.1" "49001" t))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;; aircraft/airport/runway selection code ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; select a particular aircraft type by name
;;


(defun select-aircraft (name)
           
  ;; init aircraft to nil
  
  (setf *current-aircraft* nil)
  
  ;; locate and set current aircraft
  
  (loop for aircraft in *xplane-aircraft-db* do
        (when (equal (name aircraft) name)
          (setf *current-aircraft* aircraft)
          (xplane-load-aircraft 0 (path *current-aircraft*))
          ))
           
  ;; if not found error out
           
  (if (not *current-aircraft*)
      (error "Unknown aircraft: ~a~%" name))
           
  ;; return current aircraft
           
  *current-aircraft*)

;;
;; select the current runway
;;

(defun select-runway (airport-code runway-number)
           
  ;; init airport and runway to nil
  
  (setf *current-airport* nil)
  (setf *current-runway*  nil)
  
  ;; locate and set current airport
  
  (loop for airport in *xplane-airports-db* do
        (when (equal (code airport) airport-code)
          (setf *current-airport* airport)
          (loop for runway in (runways airport) do
                (when (equal (runway-no runway) runway-number)
                  (setf *current-runway* runway)))))
           
  ;; if not found error out
           
  (if (not *current-airport*) 
      (error "Unknown airport: ~a~%" airport-code))
  (if (not *current-runway*) 
      (error "Unknown runway: ~a at ~a~%" runway-number airport-code))
           
  ;; return current runway
           
  *current-runway*)
;;
;; select traffic pattern
;;

(defmethod select-traffic-pattern ((runway runway) (aircraft aircraft-type) direction heading-mag)
  
  ;; if direction not 'left or 'right, error
  
  (if (not (or (eq direction 'left) (eq direction 'right)))
      (error "Uknown traffic pattern direction: ~a~%" direction))
  
  ;; compute feet in degrees at current latitude in the x and y dimentions
  
  (let* ((xf 303100)
         (yf 363919)
         
         ;; lat lon of center of runway
         
         (x (longitude runway))
         (y (latitude  runway))
         
         ;; length in feet from the center of the runway to each pattern line
         
         (up-len (+ (* (rw-width  runway) 0.5) (tp-radius aircraft)))
         (cr-len    (* (rw-length runway) 0.5)                      )
         (dn-len (+ (* (rw-width  runway) 0.5) (tp-radius aircraft)))
         (bs-len    (* (rw-length runway) 0.5)                      )
         (ex-len (+ (* (rw-length runway) 2.0) (tp-radius aircraft)))
         
         ;; heading for each line
         
         (up-hed (delta-heading (heading-true runway) (if (eq direction 'left) 090 270)))
         (cr-hed (delta-heading (heading-true runway) 000))
         (dn-hed (delta-heading (heading-true runway) (if (eq direction 'left) 270 090)))
         (bs-hed (delta-heading (heading-true runway) 180))
         (ex-hed (delta-heading (heading-true runway) 180))

         ;; compute point slope form for each line of the pattern
         
         (up-line (compute-radial-tan2 x y xf yf up-hed up-len))
         (cr-line (compute-radial-tan2 x y xf yf cr-hed cr-len))
         (dn-line (compute-radial-tan2 x y xf yf dn-hed dn-len))
         (bs-line (compute-radial-tan2 x y xf yf bs-hed bs-len))
         (ex-line (compute-radial-tan2 x y xf yf ex-hed ex-len)))

    ;; configure traffic pattern object
  
    (setf *current-pattern*
      (make-instance    'traffic-pattern
        :runway         runway
        :direction      direction
        :runway-heading heading-mag
        :up-heading     (compute-leg-heading heading-mag 'up     direction)
        :cross-heading  (compute-leg-heading heading-mag 'cross  direction)
        :down-heading   (compute-leg-heading heading-mag 'down   direction)
        :base-heading   (compute-leg-heading heading-mag 'base   direction)
        :extend-heading (compute-leg-heading heading-mag 'extend direction)
        :center-line    (compute-point-slope-form x y (heading-true runway))
        :regions (list  (list 'up     up-line)
                        (list 'cross  cr-line)
                        (list 'down   dn-line)
                        (list 'base   bs-line)
                        (list 'extend ex-line)))))
  
  ;; return the traffic pattern
  
  *current-pattern*)

;;
;; deselect traffic pattern
;;

(defun deselect-traffic-pattern ()
  (setf *current-pattern* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; traffic pattern code ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; traffic pattern status combinations

(defparameter *traffic-status-lookup*
    '((up     (cross      )) 
      (cross  (down       ))
      (down   (base extend))
      (base   (extend up  ))
      (extend (up         ))))

;;
;; find any item in list1 in list2
;;

(defun find-any (list1 list2)
  (loop for a in list1 do
        (loop for b in list2 do
              (when (eq a b) (return-from find-any a))))
  nil)

;;
;; compute traffic pattern status
;;

(defun dispatch-pattern-status (aircraft-x aircraft-y)
  
  ;; get the current traffic pattern 
  
  (let* ((tp *current-pattern*)
         
         ;; compute status components
         
         (status (loop for leg in (regions tp)
                     if (not (on-same-side (list (longitude (runway tp))
                                                 (latitude  (runway tp)))
                                           (list aircraft-x aircraft-y)
                                           (pattern-leg-line leg)))
                     collect (pattern-leg-name leg))))
    
    ;; convert to single value and gen correct cogevent 
    
    (loop for (has hasnt) in *traffic-status-lookup* do
          (when (and (find has status) (not (find-any hasnt status)))
            (inform (list 'pattern-status has))))))

;; accessor function for leg of a pattern

(defun pattern-leg-name (leg) (first leg))
(defun pattern-leg-line (leg) (second leg))

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; autopilot code ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

;; autopilot mode constants

(defvar *ap-disable*           9)
(defvar *ap-airspeed-hold*    10)
(defvar *ap-heading-hold*     11)
(defvar *ap-altitude-hold*    15)
(defvar *ap-vertical-hold*    14)
(defvar *ap-pitch-hold*       17)
(defvar *ap-arm-heading-nav*  21)
(defvar *ap-heading-nav*      22)
(defvar *ap-arm-altitude-nav* 23)
(defvar *ap-altitude-nav*     24)
(defvar *ap-mode-first*        t)
(defvar *ap-value-first*     nil)

;; autopilot function signatures (which value to plug into which variable)

(defvar *autopilot-function-lut*
    
    ;; function   mode setting       mode-var   param-var   set value or mode first
    
    `((heading   ,*ap-heading-hold*     mode-hding set-hding ,*ap-value-first*)
      (arm-hnav  ,*ap-arm-heading-nav*  mode-hnav  nil       ,*ap-value-first*)
      (hnav      ,*ap-heading-nav*      mode-hnav  nil       ,*ap-value-first*)
      (airspeed  ,*ap-airspeed-hold*    mode-speed set-speed ,*ap-mode-first* )
      (altitude  ,*ap-altitude-hold*    mode-alt   dial-alt  ,*ap-value-first*)
      (pitch     ,*ap-pitch-hold*       mode-alt   set-pitch ,*ap-mode-first* )
      (vertical  ,*ap-vertical-hold*    mode-alt   set-vvi   ,*ap-mode-first* )
      (arm-vnav  ,*ap-arm-altitude-nav* mode-vnav  nil       ,*ap-value-first*)
      (vnav      ,*ap-altitude-nav*     mode-vnav  nil       ,*ap-value-first*)))
;;
;; lookup autopilot function signature
;;

(defun autopilot-lookup (function)
  (let ((func-sig (lookup function *autopilot-function-lut*)))
    (if (not func-sig) (error "Unknow autopilot function: ~a~%" function))
    func-sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            ;;
;; geometry functions                         ;;
;;                                            ;;
;; CAUTION: all angles are taken as a heading ;;
;; rather then degrees or radain              ;;
;;                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; takes a point, a heading and a radius and computes
;; the radial point from the given point, heading and radius.
;; from that radial point compute the point slop form
;; for the line perpendicular to the heading
;; x y heading radius -> (point slop)
;;

(defun compute-radial-tan (x y heading radius)
  (let ((point (compute-radial-point x y heading radius)))
    (compute-point-slope-form
     (point-x point)
     (point-y point)
     (delta-heading heading 90))))

;;
;; takes a point, a heading and a radius and computes
;; the radial point from the given point, heading and radius.
;; from that radial point compute the point slop form
;; for the line perpendicular to the heading
;; x y heading radius -> (point slop)
;;

(defun compute-radial-tan2 (x y xf yf heading radius)
  (let ((point (compute-radial-point2 x y xf yf heading radius)))
    (compute-point-slope-form
     (point-x point)
     (point-y point)
     (delta-heading heading 90))))

;;
;; compute a point on a radial from another point
;; x y heading radius -> (x y)
;;

(defun compute-radial-point (x y heading radius)
  (list
   (+ x (* radius (cos (head-to-rad heading))))
   (+ y (* radius (sin (head-to-rad heading))))))

;;
;; compute a point on a radial from another point
;; x y heading radius -> (x y)
;;

(defun compute-radial-point2 (x y xf yf heading radius)
  (list
   (+ x (* (/ radius xf) (cos (head-to-rad heading))))
   (+ y (* (/ radius yf) (sin (head-to-rad heading))))))

;;
;; establish if two points are on the same side of a line
;; point1 point2 line -> boolean
;;

(defun on-same-side (point1 point2 line)
  (let* ((slope (- (ps-slope line)))
         (y-int (ps-y-int line))
         (y-int1 (+ (* slope (point-x point1)) (point-y point1)))
         (y-int2 (+ (* slope (point-x point2)) (point-y point2))))
    (or 
     (and (< y-int y-int1) (< y-int y-int2))
     (and (> y-int y-int1) (> y-int y-int2)))))
;;
;; point accessor funcitons
;;

(defun point-x (point) (first point))
(defun point-y (point) (second point))

;;
;; compute point slope form from an angle and a point
;; x y heading --> (y-intercept slope)
;;

(defun compute-point-slope-form (x y heading)
  (let* ((slope (tan (head-to-rad heading)))
         (point (+ (* (- slope) x) y)))
    (list point slope)))

;;
;; simultaneously solve for the intersection of two lines in point slope form
;;

(defun simul-solve-point-slope-form (ps1 ps2)
  
  ;; get the Ms and Bs
  
  (let* ((b1 (ps-y-int ps1))
         (m1 (ps-slope ps1))
         (b2 (ps-y-int ps2))
         (m2 (ps-slope ps2))
         
         ;; compute parts 1 & 2
         
         (part1 (- b2 (* (/ m2 m1) b1)))
         (part2 (/ (- m1 m2) m1))
         
         ;; compute y
         
         (y (/ part1 part2))
         
         ;; compute x
         
         (x (/ (- y  b1) m1)))
    
    ;; return point
    
    (list (float x) (float y))))

;;
;; point slop accessor funcitons
;;

(defun ps-y-int (ps) (first ps))
(defun ps-slope (ps) (second ps))

;;
;; converters for heading, degrees and radians
;;

(defun deg-to-rad   (degrees) (* (/ degrees 180) pi))
(defun head-to-deg  (heading) (delta-heading (- 360 heading) 90))
(defun head-to-rad  (heading) (deg-to-rad (head-to-deg heading)))

;;
;; add a delta to a heading and still get a 0 - 359 value
;;

(defun delta-heading (heading delta) (mod (+ heading (+ delta 360)) 360))

;;
;; coerce a number to a double float
;;

(defun to-double-float (real)
  (coerce real 'double-float))

;;
;; coerce a number to a single float
;;

(defun to-single-float (real)
  (coerce real 'single-float))

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; some test code ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

(defun x ()
  (packet-sniffer "143.232.72.162" 
                  :exclude '("VEHN" "STAT"))
  ;; :include '("PAPT")))
  )
;;  (xplane-set-variable 'lailn1 20.0))
  
;  (xplane-subscribe-many-channels)
;  (xplane-unsubscribe-all-channels)

  ;; flush out the socket stream
  
;  (mp:with-timeout (0.5) (while t (format t "packet data: ~a~%" (packet-receive *xplane-comm-port*))))

;  (format t "here!~%")
  
;  (setf *remove-last-channel-data* t)
;  (mp:with-timeout (0.5 (setf *remove-last-channel-data* nil)) (packet-receive *xplane-comm-port*))
 ; *remove-last-channel-data*)
  
;(defun packet-receive (port)
;  (multiple-value-bind (data length)

(defparameter *xplane-channel-range* '(0 112))

(defun xplane-unsubscribe-all-channels ()
  (loop for ch-id from (first *xplane-channel-range*)
      to (second *xplane-channel-range*) do
        (xplane-unsubscribe ch-id)))

(defun ttt ()
  (loop for x in '(0.01 0.05 0.074 0.075 0.076 0.077)  do (tt x)))

(defun tt (sleep)
  (let ((start (current-milliseconds)))
    (sleepless-delay sleep)
    (format t "time: ~a == ~a~%" sleep (- (current-milliseconds) start))))

(defun xplane-subscribe-many-channels ()
  (loop for ch-id from 0 to 10 do
        (xplane-subscribe ch-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;
;;;; the code graveyard ;;;;
;;;;                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
 ;; enable measurements
 ;;(step (xplane-enable-measurements (hding-mag vor1-hdef)))
 ;; establish heading for base leg of this traffic pattern
 ;; (step (ask +current-pattern+ for base-heading => ?base-heading))
 ;;       (waitfor (:measurement (vor1-hdef ac1 = 0 +/- 2.49))))
 ;;       (waitfor (:measurement (hding-mag ac1 = <?base-heading> +/- 2))))
 ;; disable the measurments
 ;; (step (xplane-disable-measurements (hding-mag vor1-hdef)))
