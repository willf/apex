;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/simulation/simulation.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: simulation.lisp,v 1.23 2006/01/15 03:43:02 dalal Exp $


;;; This file contains the Apex simulation engine and control interface,
;;; and has the following sections:
;;;
;;;     1. API
;;;     2. Simulation class support (NOTE: the class is defined in classes.lisp)
;;;     3. Load time actions (! may want to obviate these)

(in-package :common-lisp-user)


;;; ---- 1. Application Programming Interface
;;; ---------------------------------------------------------------------

(defmacro do-after-each-trial (&body forms)
  ;;
  ;; List(Any) -> ()
  ;; Defines actions to take place between trials.
  ;;
  `(setf (after-each-trial-action *application*)
     #'(lambda () ,@forms)))


(defmacro do-after-all-trials (&body forms)
  ;;
  ;; List(Any) -> ()
  ;; Defines actions to take place after all trials complete.
  ;;
  `(setf (after-all-trials-action *application*)
     #'(lambda () ,@forms)))


(defmacro do-before-all-trials (&body forms)
  ;;
  ;; List(Any) -> ()
  ;; User can define action to take place before any trials begin.
  ;;
  `(setf (before-all-trials-action *application*)
     #'(lambda () ,@forms)))


;;; Command Interface
;;;
;;; Commands to the simulation can come from 3 sources: externally from
;;; a GUI running on a separate thread, externally from the listener,
;;; and internally from appobject code.  The correct response to a
;;; command will depend mainly on the STATE of the simulation.

;;; A flag reset by fluid binding to effect proper behavior with respect
;;; to scheduled pauses.
(defvar *ignore-pauses* nil)

(defmethod startsim (&optional (sim *application*))
  ;; Opt(Native-Sim-Application) -> ()
  ;; Run simulation to its completion.
  (unless (eq 'on (state sim))
    (let ((*ignore-pauses* t))
      (simulate sim))))

(defmethod stepsim/pause (&optional (sim *application*))
  ;; opt(native-sim-application) -> ()
  ;; Advance simulation to the next scheduled pause, if there is one;
  ;; else run to completion.
  (unless (eq 'on (state sim))
    (if (and (null (next-pause-time sim))
             (null (pause-interval sim))
             (null (pause-cycle sim)))
        (setf (step? sim) t))
    (simulate sim)))


(defmethod stepsim (&optional (sim *application*))
  ;; Opt(Native-Sim-Application) -> ()
  "Advances simulation one event. Ignored if sim is running."
  (unless (eq 'on (state sim))
    (setf (step? sim) t)
    (simulate sim)))


(defmethod restartsim (&optional (sim *application*))
  ;; Opt(Native-Sim-Application) -> ()
  "Forces start from beginning of first trial at any time."
  (reset sim)
  (simulate sim))


;;; ---- Resetting the Simulation
;;;
;;; The simulation needs to be reset in three different ways:
;;;
;;; 1. Between trials.  Done with RESET-FOR-TRIAL (internally used only).
;;; 2. Between runs (a run is a set of trials).  Done with RESETSIM.
;;; 3. Between applications.  Done in DEFAPPLICATION.
;;;
;;; At present, 1 and 2 are automated but 3 is not.

(defmethod resetsim (&optional (sim *application*))
  ;; Opt(Native-Sim-Application) -> ()
  (reset-for-trial sim)
  (setf (trial sim) 0)
  (setf (before-all-trials-done? sim) nil)
  (reset-ps-routers)
  (values))


(defmethod pausesim (&optional (sim *application*))
  ;; Opt(Native-Sim-Application) -> ()
  (declare (ignore sim))
  (commandsim 'pause)
  (values))


(defmethod commandsim (command &optional (sim *application*))
  ;;
  ;; symbol * (Native-Sim-Application + ()) -> ()
  ;; To be used only by external controller on a separate thread to
  ;; affect ongoing simulation
  ;;
  (setf (command sim) command))


(defmethod set-number-of-trials (num &optional (sim *application*))
  ;; Type = int * (Native-Sim-Application + ()) -> ()
  ;; By default, there is just one trial.
  (setf (numtrials sim) num))

(defmethod current-trial (&optional (sim *application*))
  ;; Type = Native-Sim-Application + () -> int
  (trial sim))


(defun end-trial ()
  ;;
  ;; () -> ()
  ;; End the current trial.
  ;; !! Quite a hack.  Cannot find a clean way to do this without
  ;; refactoring simulation-inner-loop.
  ;;
  (case (state *application*)
    (paused
     (if *sherpa-running* (format t "Total time: ~a~%" (current-time)))
     (setf (command *application*) 'advance-trial)
     (setf (state *application*) 'on)
     (simulate-1 *application*))
    (initializing
     ;; ! This isn't quite the right thing to do.  It doesn't support multiple
     ;; trials -- but there are not separate sim states for end of trial and end
     ;; of all trials, so that may be needed.
     (setf (state *application*) 'done))
    (on
     (throw 'end-trial 'end-trial))
    (otherwise
     (user-error 'end-trial (format nil "Unexpected simulation state: ~a"
                                    (state *application*))))))


(defmethod end-application ((sim native-sim-application))
  (end-trial))


;;; ---- Pausing the Simulation
;;;
;;; There are several ways to pause a simulation, each having a separate
;;; interface so that these ways can be easily combined.

;;; Duration = an expression readable by DURATION-READ

(defmethod set-pause-time1 (duration (sim native-sim-application))
  ;; opt(Duration) * opt(native-sim-application) -> ()
  (if (null duration)
            (clear-pause-time sim)
    (setf (pause-time sim) (duration-read duration)))
  (setf (next-pause-time sim) (pause-time sim))
  (setf (pause-cycle sim) nil)
  (setf (pause-interval sim) nil))


(defmethod set-pause-interval1 (duration (sim native-sim-application))
  ;; opt(Duration) * opt(native-sim-application) -> ()
  (if (null duration)
      (setf (pause-interval sim) NIL)
    (setf (pause-interval sim) (duration-read duration)))
  (clear-pause-time sim)
  (setf (pause-cycle sim) nil)
  (update-pause-time sim))
    
                       
(defmethod update-pause-time ((sim native-sim-application)) ; -> ()
  (let ((next-pause-time (next-pause-time sim))
        (pause-interval (pause-interval sim))
        (now (current-time)))
    (if pause-interval
        (cond ((null next-pause-time)
               (setf (next-pause-time sim) (+ now pause-interval)))
              ((>= now next-pause-time)
               (setf (next-pause-time sim) (+ next-pause-time pause-interval))))
      (if (and next-pause-time
               (>= now next-pause-time))
          (setf (next-pause-time sim) nil)))))

(defun show-pause-settings (&optional (sim *application*))
  (format t "Current time: ~a~%" (current-time))
  (format t "Pause time: ~a~%" (pause-time sim))
  (format t "  Next pause at: ~a~%" (next-pause-time sim))
  (format t "Pause interval: ~a~%" (pause-interval sim))
  (format t "Pause cycle: ~a~%" (pause-cycle sim)))

(defmethod clear-pause-time ((sim native-sim-application))
  (setf (pause-time sim) nil)
  (setf (next-pause-time sim) nil))
  
(defmethod set-pause-cycle1 (num-events (sim native-sim-application))
  ;; opt(int) * opt(native-sim-application) -> ()
  (type-check set-pause-cycle
              (num-events (opt posint)))
  (setf (pause-cycle sim) num-events)
  (clear-pause-time sim)
  (setf (pause-interval sim) nil))


(defun pause-after-trial (flag &optional (sim *application*))  
  ;; bool * opt(native-sim-application) -> ()
  (setf (pause-after-each-trial? sim) flag))


(defun pause-after-init (flag &optional (sim *application*))  
  ;; bool * (Native-Sim-Application + ()) -> ()
  (setf (pause-after-initialization? sim) flag))



;;; ---- 2. Native-Sim-Application class support
;;; ---------------------------------------------------------------------


(defmethod print-object ((sim native-sim-application) s) 
  ;; Native-Sim-Application * Stream -> ()
  (format s "#native-sim-application(t=~a;~a)"
          (current-time) (state sim)))


(defmethod initialize ((sim native-sim-application))
  ;;
  ;; Native-Sim-Application -> ()
  ;; Initialization should instantiate one or more appobs and start
  ;; one or more activities.  If there are to be multiple trials and/or
  ;; periodic pauses, simulation parameters for these are set during
  ;; initialization.
  ;;
  (reset-ps-routers)
  (incf (trial sim))
  (when (initialize-action sim)
    ;;; ! Should wrap the following in an error handler
    (setf (state sim) 'initializing)
    (funcall (initialize-action sim)))
  ;;
  ;; initialize-action might put simulation into another state (most
  ;; likely 'done) and if so we don't want to execute the rest of this
  ;; function
  ;;
  (when (eq (state *application*) 'initializing)
    ;; ! The order of the two seems reversed (KMD 9/14/04)
    (setf (state sim) 'initialized)
    (initialize-all-activities sim)
    (when (pause-after-initialization? sim)
      (pause-sim sim "simulation initialized"))))


(defmethod after-each-trial ((sim native-sim-application))
  ;; Native-Sim-Application -> ()
  (funcall (after-each-trial-action *application*)))


(defmethod after-all-trials ((sim native-sim-application))  
  ;; Native-Sim-Application -> ()
  (unless *sherpa-running* (format t "Simulation finished.~%"))
  (funcall (after-all-trials-action *application*)))


(defmethod before-all-trials ((sim native-sim-application))  
  ;; Native-Sim-Application -> ()
  (funcall (before-all-trials-action *application*)))


(defmethod initialize-instance :after ((sim native-sim-application)
                                       &rest initargs)
  ;;
  ;; Native-Sim-Application -> ()
  ;; Provided to resolve a potential evaluation order problem, but not
  ;; sure if really needed.
  ;;
  (declare (ignore initargs))
  (setf (stepable sim) t)
  (setf (pausable sim) t)
  (setf (initialize-action sim) #'do-nothing)
  (setf (after-each-trial-action sim) #'do-nothing)
  (setf (after-all-trials-action sim) #'do-nothing)
  (setf (before-all-trials-action sim) #'do-nothing)
  (setf (reset-action sim) #'resetsim)
  (setf (start-action sim) #'startsim)
  (setf (stop-action sim) #'pausesim)
  (setf (restart-action sim) #'restartsim)
  (setf (step-action sim) #'stepsim)
  (setf (step/pause-action sim) #'stepsim/pause)
  (reset-trace-info))


(defmethod simulate ((sim native-sim-application))  
  ;;
  ;; Native-Sim-Application -> ()
  ;; Main function for simulation engine.  This function should not
  ;; normally be called directly, but rather via startsim.  The SIMULATE
  ;; function handles different possible entry conditions, essentially
  ;; gating for SIMULATION-INNER-LOOP.  Note that the inner loop handles
  ;; both within-trial simulation events and transitions between trials.
  ;;
  (setf (command sim) nil)              ; deletes old simulation command
  (case (state sim)
    (clear
     (reset sim)
     (initialize-and-maybe-simulate sim))
    (initialized 
     (simulate-1 sim))
    (paused 
     (setf (event-count sim) 0)
     ;; the following looks redundant
     (setf (state sim) 'on)
     (simulate-1 sim))
    (done 
     (if (= (trial sim) (numtrials sim))
         (reset sim)
       (reset-for-trial sim))
     (initialize-and-maybe-simulate sim))
    (error 
     (reset sim)
     (initialize-and-maybe-simulate sim))
    (on (error "Invalid simulation start.  Try (restartsim)."))))


(defun initialize-and-maybe-simulate (sim)
  (initialize sim)
  (if (not (eq 'done (state sim)))
      (simulate-1 sim)))


(defmethod simulate-1 ((sim native-sim-application))
  ;;
  ;; Native-Sim-Application -> ()
  ;; This wraps an error-handling mechanism around the simulation inner
  ;; loop that sets the simulation's state equal to 'error (making it
  ;; possible to tell the difference between a redundant start when the
  ;; simulation is already going (on) and a (re)start after encountering
  ;; an in error simulation-model code.  Technically, this handler
  ;; "declines" to handle the error condition, instead passing
  ;; responsibility to the debugger.  ! Although this is part of the
  ;; Common Lisp standard, it really ought to be tested for portability.
  ;;
  ;; Note: an error that occurs during trial initialization will leave
  ;; the simulation in state CLEAR.  This produces correct behavior
  ;; since a subsequent call to startsim will again call INITIALIZE.
  ;;
  (handler-bind ((error #'(lambda (error-condition) 
                            (declare (ignore error-condition))
                            (setf (state sim) 'error))))
    (simulation-inner-loop sim)))


#|
;;; Error handler-omitted version for debugging
(defmethod simulate-1 ((sim native-sim-application))
  (simulation-inner-loop sim))
|#

(defmethod simulation-inner-loop ((sim native-sim-application))  
  ;;
  ;; Native-Sim-Application -> () + symbol
  ;; Basic process: (1) handle any command sent to the simulation; (2)
  ;; check for pause- preset condition; (3) do a simulation event (an
  ;; activity update or completion) including advancing the current-time
  ;; value.
  ;;
  (unless (eq (state sim) 'paused)
    (progn
      (setf (state sim) 'on)
      (unless (before-all-trials-done? sim)
        (before-all-trials sim)
        (setf (before-all-trials-done? sim) t))
      (loop
        ;; handles both events within trials and transitions to new trials
        (progn 
          (case (command sim) 
            ;; handle any command received from external process
            ((nil) nil) ;; if no command received do nothing
            (pause
             (pause-sim sim "received pause command")
             (return (state sim)))
            (advance-trial
             (progn
               (setf (activities sim) '())
               (setf (agenda sim) (make-instance 'sim-agenda))))
            (stop 
             (setf (state sim) 'done) 
             (format t "Simulation stopped.~%"))
            (otherwise (error "Simulation received unknown command ~a." 
                              (command sim))))
          (unless *ignore-pauses*
            (when (and (pause-cycle sim) 
                       (= (event-count sim) (pause-cycle sim)))
              (pause-sim sim (format nil "~a events processed" 
                                     (event-count sim)))
              (return (state sim)))
            (when (and (next-pause-time sim)
                       (>= (current-time) (next-pause-time sim)))
              (pause-sim sim "scheduled pause time")
              (return (state sim))))
          ;; If not, we really are done...
          (when (and (null (activities sim)) (agenda-empty-p (agenda sim)))
            ;; !! should after-each-trial happen after a prematurely
            ;; ended trial?
            (after-each-trial sim)
            ;; ! >= could be made more precise as either > or =.
            ;; Figure it out.
            (if (>= (trial sim) (numtrials sim))
                (progn
                  (setf (state sim) 'done) 
                  (after-all-trials sim)
                  (return (state sim)))
              (if (pause-after-each-trial? sim)
                  (progn
                    (pause-sim sim "end of trial")
                    (setf (pause-after-each-trial? sim) nil)
                    (return (state sim)))
                (advance-trial sim))))
          (cond 
           ((equal (state sim) 'on)
            (incf (event-count sim))
            (when (eq (catch 'end-trial (process-next-activity-or-agendum sim))
                      'end-trial)
              (setf (agenda sim) (make-instance 'sim-agenda))
              (setf (activities sim) nil))
            (when (step? sim)
              (pause-sim sim "stepped")
              (setf (step? sim) nil)
              (return (state sim))))
           (t 
            ;; returns from loop after pause or end-sim
            (return (state sim)))))))))


(defmethod reset-for-trial (&optional (sim *application*))
  ;; Native-Sim-Application + () -> ()
  ;; See notes on resetting in API section.
;   (restore-instance-registry)
;   (reset-object-numbers)
;   (reset-events)
;   (clear-event-tally)
;   (reset-current-time sim)
;   (setf (state sim)'clear)
  (setf (activities sim) nil)
  (setf (agenda sim) (make-instance 'sim-agenda))
  (setf (command sim) nil)
  (setf (event-count sim) 0)
;  (setf *all-agents* nil)
;  (clear-event-history)
  (if (pause-time sim)
      (setf (next-pause-time sim) (pause-time sim))
    (setf (next-pause-time sim) nil))
  (update-pause-time sim)
;  (setf (locales sim) nil)
  (values))


(defmethod process-next-activity-or-agendum ((sim native-sim-application))
  ;; -> ()
  (cond ((and (activities sim) (not (agenda-empty-p (agenda sim))))
         (let ((act-time (next-activity-time sim))
               (ag-time (next-agendum-time sim)))
           (if (< act-time ag-time)
               (process-next-activity sim)
             (process-next-agendum sim))))
        ((activities sim)
         (process-next-activity sim))
        ((not (agenda-empty-p (agenda sim)))
         (process-next-agendum sim))
        (t (error "Shouldn't happen"))))

        (defmethod complete-the-activity ((act activity))
  (complete-the-activity1 act *application*))
  
(defmethod complete-the-activity1 ((act activity) (app application))
  (let ((ob (primary-object act)))
    (complete-activity act ob)
    (remove-activity act)))

(defmethod process-next-activity ((sim native-sim-application)) 
  ;;
  ;; Native-Sim-Application -> ()
  ;; Does the next scheduled activity update or activity completion.
  ;; Note: completing an activity cancels (implicitly) scheduled updates
  ;;
  (let ((next-act (dequeue-activity sim)))
    (multiple-value-bind (act-time act-type)
        (next-event next-act)
      ;; Note: concurrent events possible, so sometimes no clock change
      (set-current-time sim act-time)       
      (cond
       ((eq act-type 'update)
        (update-the-activity next-act)
        (if (complete? next-act)
            (complete-the-activity1 next-act sim)
          (schedule-activity next-act sim)))
       ((eq act-type 'complete)
        (complete-the-activity1 next-act sim))))))


(defmethod next-activity-time ((sim native-sim-application)) ; -> int
  (multiple-value-bind (time ignore)
      (next-event (first (activities sim)))
    (declare (ignore ignore))
    time))

(defmethod agenda-empty-p ((agenda sim-agenda))
  (tree-empty-p agenda))

(defmethod next-agendum ((agenda sim-agenda))
  (tree-first agenda))

(defmethod next-agendum-time ((sim native-sim-application)) ; -> int
  (if (not (agenda-empty-p (agenda sim)))
      (scheduled-time (tree-first (agenda sim)))
    most-positive-fixnum  ; i.e. infinity
    ))

(defmethod process-next-agendum ((sim native-sim-application)) ; -> ()
  (let* ((next-agendum (dequeue-agendum sim)))
    (set-current-time sim (scheduled-time next-agendum))
    (do-agendum next-agendum)))

(defun dequeue-activity (sim)           
  ;;
  ;; Native-Sim-Application -> Activity
  ;; Removes the activity with the next scheduled update or completion
  ;; event from the simulation activity queue and then returns it.
  ;;
  (pop (activities sim)))

(defmethod dequeue-agendum ((sim native-sim-application)) ; -> Agendum
  (tree-pop (agenda sim)))

(defmethod schedule-activity ((act activity) (sim native-sim-application))
  ;; -> ()
  (enqueue-activity sim act))
  
(defmethod enqueue-activity ((sim native-sim-application) (act activity))
  ;; -> ()
  ;;
  ;; Inserts an activity into the simulation activity queue (i.e
  ;; "schedules" it).
  ;;
  (setf (activities sim) 
    (insert-into-ordered-list act (activities sim) #'earlier-event)))


(defmethod advance-trial ((sim native-sim-application))
  ;; Native-Sim-Application * bool -> ()
  (reset-for-trial sim)
  (initialize sim)
  (unless (eq (state sim) 'paused)
    (setf (state sim) 'on)))


(defmethod pause-sim ((sim native-sim-application) reason)
  ;; Native-Sim-Application * string -> ()
  (setf (state sim) 'paused)
  (update-pause-time sim)
  (format t "[~a] Simulation paused (~a).~%"
          (time-format (current-time)) reason))


(defmethod initialize-all-activities ((sim native-sim-application))
  ;;
  ;; Native-Sim-Application -> ()
  ;; Calls the initialization method for all activities in the
  ;; simulation.  Invoked at the end of the simulation init process
  ;; which requires that initialization of started activities be delayed
  ;; until after sim-init.
  ;;
  (dolist (act (activities sim))
    (initialize-activity act (primary-object act))))

(defmethod agenda-insert ((agenda sim-agenda) (agendum agendum))
  (tree-insert agenda agendum))


;;; ---- Time ----

;;; Note that get-current-time is just a slot accessor for this class.

(defmethod set-current-time ((app native-sim-application)
                             (milliseconds integer)) ; -> ()
  (setf (get-current-time app) milliseconds)
  (values))

(defmethod reset-current-time ((app native-sim-application)) ; -> ()
  (setf (get-current-time app) 0)
  (values))

(defmethod schedule-event ((sim native-sim-application) (time integer) thunk)
  ;; Native-Sim-Application * int * thunk -> ()
  (agenda-insert (agenda sim) 
		 (make-instance 'agendum
		   :time (+ (get-current-time sim) time)
		   :action thunk))
  (values))


;;; ---- 3. Load time actions
;;; ---------------------------------------------------------------------

(eval-when (load eval compile)
(define-show-level simulation
    (or (event-type started-activity)
	(event-type initialized-activity)
	(event-type updated-activity)
	(event-type stopped-activity)
	(event-type completed-activity))))
