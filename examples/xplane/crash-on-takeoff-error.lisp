;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/xplane/crash-on-takeoff-error.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: crash-on-takeoff-error.lisp,v 1.6 2006/01/15 03:42:58 dalal Exp $


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      ;;;;
;;;; application PDL code ;;;;
;;;;                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; x-plane fly traffic pattern application definition
;;

(defapplication "X-Plane Demo (with takeoff error)"
  :files   ("xplane-api.lisp")
  :reset   (format t "[unimplemented x-plane reset]~%") 
  :start   (xplane-execute nil)
  :stop    (xplane-stop)
  :step    (xplane-execute t)
  :restart nil)

(in-package :common-lisp-user)

;;
;; application entry point
;;

(procedure :seq
 (index (fly traffic pattern))

 ;; selecte aircarft, airport and runway
 
 (step (setup selecting plane "Altair" at runway "KSBD" "06x")) 
 
  ;; take off
 
 (step (takeoff))
 
 ;; fly  pattern until we hit the extend region of the traffic pattern
 
 (step (fly pattern around airport))
 
 ;; land
 
 (step (land))
 
 ;; signal termination of x-plane application
 
 (step  (signal-app-termination)))

;;
;; takeoff
;;

(procedure
 (index (takeoff))
 
 ;; log knots indicated airspeed and feet above ground level
 
 (log ( (vind-kias ac1) :count-limit 5)
      ( (alt-ftagl ac1) :count-limit 5))
 
 ;; setup for takeoff 
 
 (step (setup for takeoff => (?ias-rotate  ?agl-gear-up ?vfe-ktas)))
 (step (extend flaps for takeoff))
 
 ;; after all set up, punch up the throttle
 
 (step (set throttle to 100 percent)
       (waitfor ?setup ?extend))
  
 ;; release the brakes when the engine(s) is(are) at about 70%
 
 (step (release brakes at 70 percent of power))
 
 ;; when we are going fast enough, start the climb
 
 (step (rotate)
       (waitfor ?setup (:measurement (vind-kias ac1 = ?kias) :value (> <?ias-rotate>))))
 
 ;; when high enough, pick up the gear
 
 (step (raise gear)
       (waitfor ?setup (:measurement (alt-ftagl ac1 = ?ftagl) :value (< ?agl-gear-up))))
 ;;    (waitfor ?setup (:measurement (alt-ftagl ac1 = ?ftagl) :value (> ?agl-gear-up))))
 
 ;; put up the flaps before we get to Vfe
 
 (step (retract flaps)
       (waitfor ?setup (:measurement (vind-kias ac1 = ?kias0) :value (> (* 0.90 <?vfe-ktas>)))))
 
 ;; clean up after takeoff
 
 (step (teardown from takeoff)
       (waitfor ?rotate ?raise))
 ;;    (waitfor ?rotate ?raise ?retract))
 
  ;; all done

 (step (terminate)
       (waitfor ?teardown)))

;;
;; start takeoff sequence
;;

(procedure
 (index (setup for takeoff))
 
 ;; disable any current autopilot settings
 
 (step (autopilot-disable-all))

 ;; compute mangnetic runway heading and set heading hold accordingly
 
  (step (compute-runway-heading-magenetic => ?heading-mag))
  (step (autopilot-hold heading ?heading-mag)
	(waitfor ?autopilot-disable-all
                 ?compute-runway-heading-magenetic))

  ;; lock rudder to center positon
  
  (step (xplane-set ruddr-yoke 0.0))
  
  ;; set climb rate to zero
  
  (step (autopilot-hold vertical 0.0))
  
  ;; measure max horse power and engine count, and measure current horse power
  ;; measure knots indicated airspeed, and altitude in feet above ground level
  
  (step (xplane-enable-measurements 
         (flap-postn power1-hp power2-hp vind-kias alt-ftagl)))
  
  ;; get rotate speed, gear up altitude, and Vfe for this aircraft
  
  (step (ask +current-aircraft+ for ias-rotate => ?ias-rotate))
  (step (ask +current-aircraft+ for agl-gear-up => ?agl-gear-up))
  (step (xplane-query-value vfe-ktas => ?vfe-ktas))
  
  ;; terminate when we've done it all
  
  (step (terminate  >> (?ias-rotate  ?agl-gear-up ?vfe-ktas))
        (waitfor ?autopilot-hold-1
                 ?xplane-set
                 ?autopilot-hold-2
                 ?xplane-enable-measurements
                 ?ask-1
                 ?ask-2
                 ?xplane-query-value)))
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
;; fly left or right traffic pattern until we turn to given leg
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

 (step (xplane-disable-measurements (lat-deg lon-deg)))

 ;; and deselect the traffice pattern

 (step (deselect-traffic-pattern)))

;;
;; land 
;;

(procedure
 (index (land))

 ;; start land sequence

 (step (start-land-sequence)) 

 ;; measure knots indicated airspeed, and altitude in feet above ground level
 
 (step (xplane-enable-measurements (vind-kias alt-ftagl))
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

 ;; we've stopped, we don't need these measurements anymore 
 
 (step (xplane-disable-measurements (vind-kias alt-ftagl))
       (waitfor (:measurement (vind-kias ac1 = 0 +/- 3))))

 ;; clean up the flaps
 
 (step (set-flaps 0.0)
       (waitfor ?xplane-disable-measurements))
  
  ;; infrom the world we are stoped
 
 (step (xplane-print "full stop")
       (waitfor ?set-flaps-2))
 
 ;; all done

 (step (terminate)
       (waitfor ?xplane-print-2)))

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

 ;; enable heading measurements
 
 (step (xplane-enable-measurements (hding-mag vor1-hdef)))
 
 ;; establish the heading of the base leg of this pattern
 
 (step (xplane-query-value set-hding => ?base-heading))
 
 ;; when we are going in that direction, enable vnav
 
 (step (autopilot-enable vnav)
       (waitfor (:in-order ?xplane-query-value
                           (:measurement (hding-mag ac1 = ?base-heading +/- 2)))))
 (step (autopilot-disable altitude))
 
 ;; if we're close enough to the landing cooridor, enable hnav
 
 (step (autopilot-enable hnav)
       (waitfor (:measurement (vor1-hdef ac1 = ?v1) :value (< 2.49))))
 (step (autopilot-disable heading))

 ;; disable measurements
 
 (step (xplane-disable-measurements (hding-mag vor1-hdf)))
 
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

;;
;; raise the gear
;;

(primitive
 (index (raise gear))
 (on-start (xplane-set-variable 'gear 0.0)))


;;
;; select aircraft, airport runway
;;

(procedure 
 (index (setup selecting plane ?plane at runway ?airport ?runway))
 (step (select-aircraft ?plane))
 (step (select-runway ?airport ?runway))
 (step (terminate) (waitfor ?select-aircraft ?select-runway)))

;;
;; fly pattern around airport
;;

(procedure :seq 
  (index (fly pattern around airport))
  (step (fly extended right pattern to extend)))

;;
;; fly to a given latitude and longitude
;;

(procedure :seq
 (index (fly-to ?target-loc ?altitude))
 (step (get-aircraft-location => ?ac-loc))
 (step (terminate >> ?loc)))

;;
;; get current location of the aircraft
;;

(procedure :seq
 (index (get-aircraft-location))
 (step (xplane-enable-measurements (lat-deg lon-deg)))
 (step (xplane-disable-measurements (lat-deg lon-deg))
       (waitfor (:and (lat-deg ac1 = ?lat)
                      (lon-deg ac1 = ?lon))))
 (step (terminate >> (?lat ?lon))))

;;
;; the teardown procedure at takeoff is just to stop some measurements.
;;

(procedure :seq
 (index (teardown from takeoff))
 (step (xplane-disable-measurements (flap-postn power1-hp power2-hp vind-kias alt-ftagl))))

;;
;; retract flaps
;; 

(procedure :seq (index (retract flaps))
	   (step (set-flaps 0.0)))

;;
;; set throttle
;;

(procedure :seq (index (set throttle to ?amt percent))
	   (step (set-throttle (/ <?amt> 100.0))))

;;
;; set climbe rate
;;

(procedure :seq (index (climb at ?climb-rate))
	   (step (autopilot-hold vertical ?climb-rate)))

;;
;; release breaks when engine(s) have achieved sufficient power output
;;

(procedure 
 (index (release brakes at ?amt percent of power))
 
 ;; get max horse power and engine count, and measure current horse power
 
 (step (ask +current-aircraft+ for horse-power => ?hp))
 (step (ask +current-aircraft+ for engine-count))
 (step (release-brakes)
       (waitfor (:or
                 
                 ;; one engine case
                 
                 (:and 
                  (engine-count 1)
                  (:measurement (power1-hp ac1 = ?hp1) :value (> (* <?hp> (/ <?amt> 100.0)))))
                 
                 ;; two engine case
                 
                 (:and 
                  (engine-count 2)
                  (:measurement (power1-hp ac1 = ?hp1) :value (> (* <?hp> (/ <?amt> 100.0) )))
                  (:measurement (power2-hp ac1 = ?hp2) :value (> (* <?hp> (/ <?amt> 100.0) )))))))
 (step (terminate) (waitfor ?release-brakes)))

;;
;; set takeoff flaps
;;

(procedure :seq
  (index (extend flaps for takeoff))
  (step f (ask +current-aircraft+ for flaps-takeoff))
  (step g (set-flaps ?flaps-takeoff)
	(waitfor (flaps-takeoff ?flaps-takeoff))))

;;
;; start climb from airport
;;

(procedure :seq
  (index (rotate))
  (step (ask +current-aircraft+ for climb-rate => ?climb-rate))
  (step (climb at <?climb-rate>)))

;;
;; signal that the application has completed
;;

(primitive
 (index (signal-app-termination))
 (on-start (stopapp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;
;;;; application lisp code ;;;;
;;;;                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; init th explane app
;;

(defun xplane-init ()

  (init-db)

  ;; init connection

  (initialize-xplane-connection)

  ;; work around for residual routers

  (reset-ps-routers)
  
  ;; reset the trace info
  
  (reset-trace-info)
  
  ;; init the simulation time
  
  (xplane-init-sim-time)

  ;; make the agent
  
  (let* ((locale (make-instance 'locale :name 'xplane-world))
	 (agent (make-instance 'agent
		  :locale locale
		  :name 'xp-pilot
		  :initial-task '(fly traffic pattern))))
    
    ;; setup the trace

    (set-trace-constraint
     '(or
       ;;     t
       (event-type task-started)
       ;; (event-type pattern-status)
       ;; (event-type turned)
       ;; (event-type flap-postn)
       ;; (event-type terminated)

       ;; (event-type vind-kias)
       ;; (event-type alt-ftagl)
       ;; (event-type power1-hp)
       ;; (event-type power2-hp)
       ;; (event-type lat-deg)
       ;; (event-type lon-west)
       ;; (event-type hding-mag)
       ))

    ;; wake the sleeping monster
    
    (setf *xplane-app-inited* t)

    ;; give the asa a kick in the ass

    (asamain agent)
    
    ;; return the agent
    
    agent))

;; run one or more xplane execution cycles

(defun xplane-execute (step)
  
  ;; if not inited, call init
  
  (when (not *xplane-app-inited*)
    (xplane-init))
  
  ;; the note that the app is alive
  
  (setf *xplane-app-running* t)
  
  ;; if paused, unpause sim
  
  (xplane-unpause-sim-time)
  
  ;; if this just a single step, execute one cycle
  
  (if step (xplane-receive)
    
    ;; otherwise execute cycles until interrupted
    
    (loop while *xplane-app-running* do
          (xplane-receive)))
  
  ;; the note that the app is not alive
  
  (setf *xplane-app-running* nil))

;;
;; stop the xplane process
;;

(defun xplane-stop ()
  (xplane-pause-sim-time)
  (setf *xplane-app-running* nil))


