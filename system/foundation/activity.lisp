;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/activity.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: activity.lisp,v 1.16 2006/01/15 03:43:01 dalal Exp $

;;; Activity class and support

;;; ! NOTE: this code depends on the sim engine, which is bad.  To be
;;; refactored.  -- KMD

(in-package :user)

;;; The main role of simulation is to allow events to play out over time.
;;; Activities are representations of the time-structure of simulation events -
;;; i.e. they inform the simulation process that something that takes more than
;;; 0 time is to happen.  In the simplest case, an activity is a single,
;;; discrete, delayed response to some occurrence such as a message to a appob.
;;; But an activity can also represent multiple and/or continuous responses.
;;; E.g. a falling activity (resulting, say, from the removal of a supporting
;;; structure for a physical object) produces responses such as changing the
;;; position and motion-vector of an object, as well as an eventual collision
;;; event.

;;; Note: it may be desirable to support activities that are fundamentally
;;; multiobject (e.g.  mixing, bonding, stampeding)

(defclass activity (appob)       ; activity subclass names should end
                                    ; with -ing.
  ((complete? :initform nil         ; Boolean.  This may involve into
              :accessor complete?)  ; into a more general "state"
				    ; slot later, but for now provides a
                                    ; means of knowing that an activity
                                    ; is complete.
   (primary-object 
    :initarg :primary-object     ; primary simulation object undergoing
    :initform nil                ; activity
    :accessor primary-object)            
   (start-time :initarg :start-time     ; timestamp for start of activity
               :accessor start-time)    ; instance
   (completion-time                     ; time of expected completion if known
    :accessor completion-time
    :initarg :completion-time
    :initform nil)
   (completion-action                   ; thunk to execute upon completion
    :type function
    :accessor completion-action
    :initarg :completion-action
    :initform nil)
   (update-action                       ; thunk to evaluate upon updates
    :type function
    :accessor update-action
    :initform nil)
   (update-time :accessor update-time
                :initarg :update-time
                :initform nil)          ; time of next expected intermediate
                                        ; update
   (update-interval 
    :initarg :update-interval           ; time interval between
    :initform nil                       ; intermediate-update
    :accessor update-interval)          ; cycles; nil if no cycling
   (cause                               ; Cause (see event.lisp).  Cause
    :initarg :cause                     ; of activity start.
    :initform nil
    :reader cause)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(complete? completion-action update-action))))



(defmethod start-activity ((ob appob) act-type &rest parameters)
  ;;
  ;; Appob * ActivityType * List(ParamSpec) -> Activity
  ;;
  ;; Starting an activity means telling the simulation to schedule update
  ;; and/or completion events of a particular activity-type associated
  ;; with a particular application object.  The activity specification must
  ;; include a :update-interval and/or :completion-time keyword
  ;; parameter.  These can be either an integer or function - if a
  ;; function then it is called with the new activity as sole argument.
  ;; Optionally, a call can specify a :simulation parameter; if not the
  ;; default simulation is used.  Below is an example of a START-ACTIVITY
  ;; call.
  ;; 
  ;; (start-activity left-hand 'moving-mouse :mouse m :target icon-3
  ;;         :duration #'fitts-law)
  ;; 
  ;; (defmethod fitts-law ((act moving-mouse))
  ;;    (let ((target (target act))  ; this fn depends on having defined
  ;;                                 ; physob-distance fn and
  ;;       (hand (primary-object act)))      ; size accessor method
  ;;       (+ 1.03 (* .096 
  ;;                  (log (+ (/ (physob-distance hand target) (size target))
  ;;                          .5) :base 2)))))
  ;;
  (let ((now (current-time)))
    (multiple-value-bind (sim update-interval duration cause otherparams)
        (parse-activity-parameters parameters)
      (if (not (or update-interval duration))
          (error
           "Tried to start activity ~a with no update value or duration"
           act-type))
      (let* ((simulation (or sim *application*))
             (new-activity 
              (apply #'make-instance act-type :primary-object ob 
                     :update-interval update-interval :start-time now 
                     :cause 
                     (or cause 
                         (if (eq (state simulation) 'initializing) 
                             (make-initial-event 
                              '(initialize-simulation))
                           (make-initial-event '(unspecified))) )
                     otherparams))
             (update-time
              (and update-interval 
                   (+ now (activity-interval update-interval new-activity))))
             (completion-time 
              (and duration (+ now (activity-interval duration new-activity)))))
        (push new-activity (activities ob))
        (setf (update-time new-activity) update-time)
        (setf (completion-time new-activity) completion-time)
        (schedule-activity new-activity simulation)
	(unless (eq (state simulation) 'initializing)
          ;; wait until simulation is finished initializing to init activities
	  (initialize-activity new-activity ob))
        new-activity))))


(defmethod stop-activity ((act activity))
  ;; Activity -> ()
  ;; Used to terminate an activity without running its completion method.
  (remove-activity act))


(defmethod schedule-completion ((act activity) time)
  ;;
  ;; Activity * Time -> ()
  ;; Schedules a completion for the activity the given number of time
  ;; units from now.
  ;;
  (let ((tick (duration-read time)))
    (setf (completion-time act) (+ (current-time) tick))
    ;; the activity should no longer be updated...
    (setf (update-interval act) nil)))


(defmethod update-cause ((act activity))
  ;;
  ;; Activity -> Event
  ;; Generates a cause argument to use in events signaled or logged from
  ;; update-activity.
  ;;
  (make-instance 'cogevent
    :content `(update-activity ,act (primary-object ,act))
    :cause (cause act)))


(defmethod complete-cause ((act activity))
  ;;
  ;; Activity -> Event
  ;; Generates a cause argument to use in events signaled or logged from
  ;; complete-activity.
  ;;
  (make-instance 'cogevent
    :content `(complete-activity ,act)
    :cause (cause act)))


;;; ---- 1b. API (methods to be specialized)
;;; ---------------------------------------------------------------------

(defmethod update-activity ((act activity) (ob appob))
  ;; Activity * Appob -> ()
  (declare (ignorable act ob)))


(defmethod initialize-activity ((act activity) (ob appob))
  ;;
  ;; Activity * Appob  -> ()
  ;; An application can optionally specify a class-specific
  ;; initialization method to be invoked immediately following the start
  ;; of an activity.
  ;;
)

(defmethod update-the-activity ((act activity))
  (update-activity act (primary-object act))
  (setf (update-time act)
    (if (update-interval act)
        (+ (current-time)
           (activity-interval (update-interval act) act))
      nil)))

(defmethod complete-activity ((act activity) (ob appob))
  ;; Type = Activity * Appob -> ()
  (declare (ignorable ob))
  (if (completion-action act)
      (funcall (completion-action act))))

(defmethod complete-activity ((act activity) x)  ; !- x is nil
  ;; Type = Activity * nil -> ()
  (assert (null x))
  (if (completion-action act)
      (funcall (completion-action act))))

(defun activity= (act1 act2)            
  ;; Type = Activity * Activity -> Bool
  ;; Compare two activities for "equality".
  (eq (id act1) (id act2)))


;;;(defun activity-interval (intvl act)
;;;  ;; Type = (Time + (Activity -> int)) * Activity -> int
;;;  (typecase intvl 
;;;    (integer intvl) 
;;;    (function (funcall intvl act))
;;;    ;; ! not as good a check as TIME-VALUE?
;;;    (consp (parse-time intvl))          
;;;    (otherwise
;;;     (error "Interval value must be integer, function or method."))))

(defun activity-interval (intvl act)
  ;; Type = (Time + (Activity -> int)) * Activity -> int
  (if (functionp intvl)
    (funcall intvl act)
    intvl))

(defun parse-activity-parameters (params)
  ;;
  ;; List(ParamSpec) -> 
  ;;   (Native-Sim-Application + 'nil) * int * int * Cause * List(ParamSpec)
  ;;        
  ;; Separates out keyword parameters for the ACTIVITY class from
  ;; parameters for its subclasses.
  ;;
  ;; ! would be a good idea to check whether params list is properly
  ;; pair-structured.
  ;;
  (loop with (sim update-interval duration cause otherparams) t
             ;; set these local vars to nil
      for par-list on params by #'cddr;; at each iteration, dequeue 2 items
      do
        (let ((param-type (first par-list)) 
              (param-val (second par-list)))
          (case param-type
            ((:simulation) (setf sim param-val))
            ((:update-interval) (setf update-interval param-val))
            ((:duration) (setf duration (duration-read param-val))) ;; (parse-time param-val)))
            ((:cause) (setf cause param-val))
            (otherwise 
             (setf otherparams
               (append (list param-type param-val) otherparams)))))
      finally 
        (return (values sim update-interval duration cause otherparams))))


(defmethod remove-activity ((act activity))
  ;; Type = Activity -> ()
  ;; A cleanup method called by stop-activity and complete-activity.
  (let ((app *application*)
        (ob (primary-object act)))
    (if ob
        (setf (activities ob)
          (remove act (activities ob) :test #'activity=)))
    (setf (activities app) 
      (remove act (activities app) :test #'activity=))
    (setf (complete? act) t)))          ; prevents requeuing


(defmethod process-the-activity ((act activity)
                                 (app realtime-application)) ; -> ()
  ;;
  ;; Repeating until the activity is complete, sleep until the next
  ;; thing (update or completion) should happen, and then do that thing.
  ;;
  (do ()
      ((complete? act) (complete-the-activity act))
    (multiple-value-bind (time type)
        (next-event act)
      (sleep (/ (- time (current-time)) 1000)) ; durations are always in ms.
      (cond
       ((eq type 'update)
        (update-the-activity act)
        ;; Check if the activity is to be immediately completed
        (if (complete? act) (complete-the-activity act)))
       ((eq type 'complete)
        (complete-the-activity act))))))

(defmethod schedule-activity ((act activity) (app realtime-application)) ; -> ()
  ;; Put activity into que/list/table for potential access by foreign app
  ;; Just push it on for now, figure out how to access later
  (push act (activities *application*))
  ;; Get it going in its own thread.
  (thread `(process-the-activity ,act) (process-the-activity act app)))

    
(defun earlier-event (act1 act2)
  ;;
  ;; Activity * Activity -> Activity + nil
  ;; Determines whether the first of two activities needs to be handled
  ;; (updated or completed) earlier then the second.
  ;;
  ;; KMD: see how return value is used (just as boolean?)
  ;; KMD: note that second value of next-event is ignored.
  ;;
  (if (<= (next-event act1) (next-event act2))  act1 nil))


(defun next-event (act)                 
  ;;
  ;; Activity -> int * symbol
  ;; Returns the time and type (completion or update) of the next simulation
  ;; event for a given activity.
  ;;
  (let* ((tf (completion-time act)) 
         (tu (update-time act))
         (tf-sooner (or (and tf tu (< tf tu)) (null tu))))
    (if tf-sooner 
        (values tf 'complete) 
      (values tu 'update))))

