;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/xplane/example.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: example.lisp,v 1.9 2006/04/08 00:36:48 rharris Exp $


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      ;;;;
;;;; application PDL code ;;;;
;;;;                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; x-plane example application definition
;;

(defapplication "X-Plane Demo"
    :type :realtime
    :files   ("xplane-api.lisp")
    :init    (xplane-init)
    :reset   (xplane-reset)
    :start   (xplane-execute nil)
    :stop    (xplane-stop)
    :step    (xplane-execute t)
    :restart nil)

(in-package :common-lisp-user)

;;
;; application entry point
;;

(procedure :seq
 (index (fly the plane))

 ;; select aircraft and runway

 (step (xplane-place-aircraft "KPOC" "26L"))
 (step (select-aircraft "King Air"))
 
  ;; take off, fly a left traffic pattern and land

 (step (takeoff))
 (step (fly extended left pattern to extend))
 (step (land))
 
  ;; take off from where we landed, fly a right traffic pattern and land

 (step (takeoff))
 (step (fly extended right pattern to extend))
 (step (land))
 
 ;; signal termination of x-plane application
 
 (step (signal-app-termination)))

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
;; signal that the application has completed
;;

(primitive
 (index (signal-app-termination))
 (on-start 
  (mp:process-run-function
      (list :name "x-plane application termination")
            #'stopapp)))

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
  
  ;;(reset-trace-info)
  
  ;; init the simulation time
  
  ;;(xplane-init-sim-time)

  ;; make the agent
  
  (let* ((locale (make-instance 'locale :name 'xplane-world))
	 (agent (make-instance 'agent
		  :locale locale
		  :name 'xp-pilot
		  :initial-task '(fly the plane))))
    
    ;; setup the trace

;;    (set-trace-constraint
;;     '(or
;;       (event-type task-started)
;;       ))

    ;; wake the sleeping monster
    
    (setf *xplane-app-inited* t)

    ;; give the asa a kick in the ass

    (asamain agent)
    
    ;; return the agent
    
    agent))

;; reset xplane app

(defun xplane-reset ()
  
  ;; if not inited, call init
  
  (setf *xplane-app-inited* nil)
  
  ;; the note that the app is not alive
   
  (setf *xplane-app-running* nil)
  
  ;; pause sim
  
  (xplane-pause-sim-time))
  
;; run one or more xplane execution cycles

(defun xplane-execute (step)
  
  ;; if not inited, call init
  
  (when (not *xplane-app-inited*)
    (xplane-init))
  
  ;; the note that the app is alive
  
  (setf *xplane-app-running* t)
  
  ;; unpause the sim
  
  (xplane-unpause-sim-time)
  
  ;; if this just a single step, execute one cycle
  
  (if step (xplane-receive)
    
    ;; otherwise execute cycles until interrupted
    
    (loop while (eq *xplane-app-running* t) 
        do (xplane-receive)))
  
  ;; pause the sim
  
  (xplane-pause-sim-time)

  ;; the note that the app is not alive (in case this was a step)
  
  (setf *xplane-app-running* nil))

;;
;; stop the xplane application
;;

(defun xplane-stop ()

  ;; when the app is running
  
  (when (eq *xplane-app-running* t)
    
    ;; indicate that it is halting (which causes it to
    ;; stop and set *xplane-app-running* to nil)
    
    (setf *xplane-app-running* 'halting)
    
    ;; wait until the app has actually stoped
    
    (while (eq *xplane-app-running* 'halting)
      (sleep 0.1)))
  
  ;; now pause the sim
  
  (xplane-pause-sim-time))


(trace xplane-init)
(trace xplane-execute)
(trace xplane-reset)
(trace xplane-stop)
(trace startapp)
(trace stepapp)
(trace stepapp/pause)
(trace stopapp)
(trace resetapp)
(trace reloadapp)
;;(trace xplane-receive)


 
