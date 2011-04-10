;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/application.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: application.lisp,v 1.40 2006/03/17 23:19:51 rharris Exp $

;;; Application class and support.


(in-package :common-lisp-user)

;;; ---------------------- Application classes

;;; --- Application

;;; Application is an abstract base class that represents the
;;; application being run in Apex (there is always exactly one).

(defclass application (appob)
   ((name                                ; name of application
     :type string
     :accessor app-name)
    (pathname                            ; Pathname of application def file
     :accessor app-pathname)             ; (type is string or pathname).

    (libraries                           ; Libraries (maybe not all) used by app.
     :type list                          ; List of strings
     :accessor libraries
     :initform '())
    (files                               ; Files (maybe not all) loaded by app.
     :type list                          ; List of pathnames.
     :accessor files
     :initform '())
    ;;
    ;; A list of the "global" publish/subscribe routers available to all
    ;; entities.  Provided here for descendant support.  These routers
    ;; are accessed via the global variables.
    (routers
     :type list
     :accessor routers
     :initform nil)
    ;;
    ;; A list of the Locales in this application
    (locales
     :type list
     :accessor locales
     :initform nil)
    ;;
    ;; State of application.  Possible values are: clear, initialized,
    ;; initializing, on, paused, done, and error.  CLEAR indicates that
    ;; nothing has yet begun or the app has been reset since the last
    ;; run.  INITIALIZED occurs after the INITIALIZE method has been run.
    ;; INITIALIZING occurs while an app's INITIALIZE method is running.
    ;; While an app is ongoing, it is in the ON state.  A PAUSED state
    ;; can occur either by a commanded or preset pause.  The DONE state
    ;; holds when all runs have ended (! may also hold when current trial
    ;; has ended; need to sort out).  If a fatal error occurs while the
    ;; app is running, presumably as a result of incorrect application
    ;; code, the app goes into the ERROR state.
    ;;
    (state
     :type symbol
     :initform 'clear
     :accessor state)
    (activities                          ; Queue of ongoing activities.
     :initform nil
     :accessor activities)
    (objects                             ; Top level objects
     :type list                          ; list(Appob)
     :initform nil
     :accessor objects)
    (initialize-action                   ; function to initialize the app
     :type function                      ; () -> ()
     :initform #'do-nothing
     :accessor initialize-action)
    (reset-action                        ; function to reset the app
     :type function                      ; () -> ()
     :initform #'do-nothing
     :accessor reset-action)
    (start-action                        ; function to start the app
     :type function                      ; () -> ()
     :initform #'do-nothing
     :accessor start-action)
    (stop-action                         ; function to stop the app
     :type function                      ; () -> ()
     :initform #'do-nothing
     :accessor stop-action)
    (restart-action                      ; function to restart the app
     :type function                      ; () -> ()
     :initform #'do-nothing
     :accessor restart-action)
    (step-action                         ; function to step the app
     :type function                      ; () -> ()
     :initform #'do-nothing
     :accessor step-action)
    (step/pause-action                   ; function to advance to next pause
     :type function                      ; () -> ()
     :initform #'do-nothing
     :accessor step/pause-action)
    (component-slots
     :allocation :class
     :type list
     :initform '(locales routers)
     :reader component-slots)
    (t-started
     :initform NIL
     :accessor start-of
     :documentation "When application started.")
    (pausable                            ; boolean
     :initform nil
     :accessor pausable
     :documentation "Is this application pausable?")
    (stepable                            ; boolean
     :initform nil
     :accessor stepable
     :documentation "Is this application stepable?")
    (pause-time-setter
     :type function                      ; opt(Duration -> ())
     :initform nil
     :accessor pause-time-setter
     :documentation "Function to set a pause time")
    (pause-interval-setter
     :type function                      ; opt(Duration -> ())
     :initform nil
     :accessor pause-interval-setter
     :documentation "Function to set a pause interval")
    (pause-cycle-setter
     :type function                      ; opt(Duration -> ())
     :initform nil
     :accessor pause-cycle-setter
     :documentation "Function to set a pause cycle")
    (log-stream
     :initform nil
     :accessor log-stream
     :documentation "An output stream to send logged events to.")
    (suppressed-slots
     :reader suppressed-slots
     :allocation :class
     :initform '(initialize-action reset-action restart-action
                 start-action step-action step/pause-action stop-action
                 pause-cycle-setter pause-interval-setter pause-time-setter log-stream))
    ))

;;;  The single instance of Application
(defvar *application* nil)

;;; Initialization

(defmethod initialize ((app application))
   (funcall (initialize-action app)))

(defun reset-ps-routers ()
   (reset *default-ps-router*)
   (if *application* (mapc #'reset (routers *application*))))

(defmethod initialize-instance :after ((r ps-router) &rest initargs)
   (if *application* (pushnew r (routers *application*))))


;;; Control

;;; NOTE: reset method defined in asa/agent.lisp, to avoid
;;; cross-dependency.

(defmethod apex-step ((app application)) ; 'step' defined in ACL
   (unless (start-of app) (reset-current-time app))
   (funcall (step-action app)))

(defmethod apex-restart ((app application)) ; 'restart defined in ACL
   (reset-current-time app)
   (funcall (restart-action app)))

(defmethod stop ((app application))
   (funcall (stop-action app)))

(defmethod step/pause ((app application))
  (funcall (step/pause-action app)))

(defmethod start ((app application))
  ;; trebor: start is used to start and restart an application
  ;; restting time breaks teh restart case
  ;;
  ;;   (if (member (state app) '(initialized clear))
  ;;     (reset-current-time app))
  (funcall (start-action app)))

(defun set-pause-time (duration &optional (app *application*))
   ;; Duration * opt(application) -> ()
   (set-pause-time1 duration app))

(defun set-pause-cycle (num-cycles &optional (app *application*))
   ;; int * opt(application) -> ()
   (set-pause-cycle1 num-cycles app))

(defun set-pause-interval (duration &optional (app *application*))
   ;; Duration * opt(application) -> ()
   (set-pause-interval1 duration app))

;;; --- Non-native application (an abstract class)

(defclass non-native-application (application) ())

(defmethod reset :before ((app non-native-application))
   (clear-event-history)
   (reset-current-time app))

(defmethod reset :after ((app non-native-application))
  (setf (state app) 'clear))

(defmethod start :after ((app non-native-application))
  (setf (state app) 'on))

(defmethod apex-restart :after ((app non-native-application))
  (setf (state app) 'on))

(defmethod stop :after ((app non-native-application))
  (setf (state app) 'paused))

(defmethod apex-step :after ((app non-native-application))
  (setf (state app) 'paused))

(defmethod step/pause :after ((app non-native-application))
  (setf (state app) 'paused))

(defmethod initialize :before ((app non-native-application))
  (setf (state app) 'initializing))

(defmethod initialize :after ((app non-native-application))
  (setf (state app) 'initialized))

(defmethod end-application ((app non-native-application))
   (reset-threads))

(defmethod set-pause-time1 (duration (app non-native-application))
   (if (pause-time-setter app)
       (funcall (pause-time-setter app) duration)
     (warn "No function specified to set pause time!")))

(defmethod set-pause-cycle1 (num-cycles (app non-native-application))
   (if (pause-cycle-setter app)
       (funcall (pause-cycle-setter app) num-cycles)
     (warn "No function specified to set pause cycle!")))

(defmethod set-pause-interval1 (duration (app non-native-application))
   (if (pause-interval-setter app)
       (funcall (pause-interval-setter app) duration)
     (warn "No function specified to set pause interval!")))


;;; --- Realtime application

(defclass realtime-application (non-native-application) ())

(defmethod get-current-time ((app realtime-application))
   (unless (start-of app) (reset-current-time app))
   (- (current-milliseconds)
      (start-of app)))

(defmethod set-current-time ((app realtime-application)
                              (milliseconds integer))
   (error "Cannot set the time of a realtime application!"))

(defmethod reset-current-time ((app realtime-application))
   (setf (start-of app) (current-milliseconds)))

(defmethod schedule-event ((app realtime-application) (delay integer) thunk)
   ;;
   ;; realtime-application * int * thunk -> ()
   ;; An event (represented by a thunk) is scheduled to the system clock.
   ;; The handler for each events runs in its own thread.
   ;;
   (thread `(schedule-event ,delay)
    (progn
      (sleep (/ delay 1000)) ; delay must be milliseconds
      (funcall thunk))))


;;; --- Foreign simulation application

(defclass foreign-sim-application (non-native-application)
   (;; all slots have type function or symbol (naming function)
    (time-function
     :accessor time-function
     :initarg :time-function)
    (time-setter
     :accessor time-setter
     :initarg :time-setter)
    (time-resetter
     :accessor time-resetter
     :initarg :time-resetter)
    (scheduler
     :accessor scheduler
     :initarg :scheduler)))

(defmethod get-current-time ((app foreign-sim-application))
   (funcall (time-function app)))

(defmethod set-current-time ((app foreign-sim-application)
                              (milliseconds integer))
   (funcall (time-setter app) milliseconds))

(defmethod reset-current-time ((app foreign-sim-application))
   (funcall (time-resetter app)))

(defmethod schedule-event ((app foreign-sim-application)
                            (delay integer) thunk)
   ;;
   ;; foreign-sim-application * int * thunk -> ()
   ;; An event (represented by a thunk) is scheduled as specified by the
   ;; application's method.
   ;;
   (funcall (scheduler app) delay thunk))


;;; --- Time

;;; Generic functions for time.

(defun current-time ()                      ; -> integer number of ms since epoch
   (if *application*
     (get-current-time *application*)
     (current-milliseconds)))

(defmethod set-clock ((milliseconds integer))
   (if *application*
       (set-current-time *application* milliseconds)
     (error "No application with which to set time!")))

(defun reset-clock ()
   (if *application*
       (reset-current-time *application*)
     (error "No application with which to reset time!")))

(defmacro schedule (delay &body body)   ; Time * any+ -> ()
   ;;
   ;; Schedule an arbirary event, which is encapsulated in a thunk
   ;; (function of zero arguments) that is called after the given delay
   ;;
   `(schedule-event
     *application*
     ;; (parse-time ',delay)
     (duration-read ,delay)
     #'(lambda () ,@body)))

(defun scheduling* (timefunction dofunction)
   (let ((when (funcall timefunction)))
     (when when
       (schedule when
		 (funcall dofunction)
		 (scheduling* timefunction dofunction)))))

(defmacro scheduling (timing &body body)
   `(scheduling* (lambda () ,timing)
		(lambda () ,@body)))

(defun sim-time ()
   ;; ! For backwards compatibility.  Remove eventually.
   (format t "WARNING: call to (sim-time), a deprecated function!~%")
   (format t "Please replace with (current-time) in application code~%")
   (current-time))


;;; ----------------- DEFAPPLICATION form and support


(defparameter *testing-defapp* nil)

(defstruct app-data
   ;;
   ;; An intermediate data structure created while evaluating the
   ;; DEFAPPLICATION form.
   ;;
   name
   type
   libraries
   files
   init
   reset
   start
   stop
   step
   restart
   time-function
   time-setter
   time-resetter
   scheduler
   pause-time-setter
   pause-interval-setter
   pause-cycle-setter)


(defun quote-symbols-and-lists (x)      ; list -> list
   (cond ((null x) '())
         ((or
           (and (symbolp (car x)) (not (keywordp (car x))))
           (consp (car x)))
          (cons `(quote ,(car x)) (quote-symbols-and-lists (cdr x))))
         (t (cons (car x) (quote-symbols-and-lists (cdr x))))))


(defmacro defapplication (name &rest args)
   ;; string * <keyword-argument pair>* -> application
   `(progn
      (unless *testing-defapp*
        (if (null *load-truename*)
            (error "DEFAPPLICATION can be evaluated only by loading its file.")))
      (create-application
       (let ((data (make-app-data
                    :name ,name ,@(quote-symbols-and-lists `,args))))
         (if (null (app-data-type data)) (setf (app-data-type data) :native-sim))
         data))))


(defmethod create-application ((data app-data)) ; -> application
   (macrolet
       ((disallow-slots (&rest slot-names)
          `(mapc (lambda (slot)
                   (when (slot-value data slot)
                     (error
                      "Slot '~a' not allowed in ~a applications."
                      slot (app-data-type data))))
                 ',slot-names))
        (require-slots (&rest slot-names)
          `(mapc (lambda (slot)
                   (if (null (slot-value data slot))
                       (error "Slot ~a required in ~a applications."
                              slot (app-data-type data))))
                 ',slot-names)))
     (let ((app nil)
           (type (app-data-type data)))
       ;;
       ;; Create the right kind of application
       ;;
       (case type
         (:native-sim
          (require-slots init)
          (disallow-slots start stop step reset restart time-function
                        time-setter time-resetter scheduler
                        pause-time-setter pause-interval-setter
                        pause-cycle-setter)
          (setq app (make-instance 'native-sim-application)))
         (:realtime
          (disallow-slots time-function scheduler time-setter time-resetter)
          (setq app (make-instance 'realtime-application)))
         (:foreign-sim
          (setq app (make-instance 'foreign-sim-application)))
         (t
          (if (and (symbolp type)
                   (find-class type))
              ;; ! should also check for being subclass of Application
              (setq app (make-instance type))
            (error
             "Illegal application type: ~a" type))))
       ;;
       ;; Populate its slots
       ;;
       (with-slots
           (reset start stop step restart pause-time-setter
            pause-interval-setter pause-cycle-setter time-function
            time-setter time-resetter scheduler init name libraries files)
           data
         (if reset (setf (reset-action app) (thunkify reset)))
         (if start (setf (start-action app) (thunkify start)))
         (when stop
           (setf (pausable app) t)
           (setf (stop-action app) (thunkify stop)))
         (when step
           (setf (stepable app) t)
           ;; trebor: changed from step-action to step/pause-action to enable
           ;; stepping of no-native applications in shpera
           (setf (step/pause-action app) (thunkify step)))
         (if init (setf (initialize-action app) (thunkify init)))
         (if restart (setf (restart-action app) (thunkify restart)))
         (if pause-time-setter (setf (pause-time-setter app) pause-time-setter))
         (if pause-interval-setter
             (setf (pause-interval-setter app) pause-interval-setter))
         (if pause-cycle-setter
             (setf (pause-cycle-setter app) pause-cycle-setter))
         (if time-function (setf (time-function app) time-function))
         (if time-setter (setf (time-setter app) time-setter))
         (if time-resetter (setf (time-resetter app) time-resetter))
         (if scheduler (setf (scheduler app) scheduler))
         (setf (app-name app) name)
         (setf (libraries app) libraries)
         (setf (app-pathname app) *load-truename*)
         (setf (files app)
           (mapcar
            (lambda (file)              ; string + pathname -> pathname
              (let ((source *load-truename*))
                (make-pathname
                 :device (pathname-device source)
                 :host (pathname-host source)
                 :directory (pathname-directory source)
                 :name (pathname-name file)
                 :type (pathname-type file))))
            files)))
       ;;
       ;; Cache it on the global and return it
       ;;
       (setq *application* app)
       app)))

;;; Application interface (convenient wrappers of methods)

(defun initapp (&optional (in-listener t))
   (doapp #'initialize in-listener))

(defun resetapp (&optional (in-listener t))
   (doapp #'reset in-listener))

(defun startapp (&optional (in-listener t))
   (doapp #'start in-listener))

(defun stopapp (&optional (in-listener t))
   (if (pausable *application*)
       (doapp #'stop in-listener)
     (warn "Application not pausable -- ignoring request.")))

(defun stepapp (&optional (in-listener t))
   (if (stepable *application*)
       (doapp #'apex-step in-listener)
     (warn "Application not stepable -- ignoring request.")))

(defun stepapp/pause (&optional (in-listener t))
  (doapp #'step/pause in-listener))

(defun restartapp (&optional (in-listener t))
   (doapp #'apex-restart in-listener))

(defun doapp (fn in-listener?)  ; (application * bool -> ()) -> ()
   (if *application*
       (let ((*trace-destination*
              (if in-listener? 'listener *trace-destination*)))
         (funcall fn *application*))
     (format t "There is no application!  Please load one.~%")))

;;; Misc

(defun application-directory ()         ; -> string
   (pathname-to-string
    (make-pathname :directory
                   (pathname-directory (app-pathname *application*)))))


(defmethod start-logging1 ((app application) filename)
  (unless (log-stream app)
    (setf (log-stream app)
      (open filename :if-does-not-exist :create :direction :output :if-exists :append))))

(defmethod stop-logging1 ((app application))
  (when (log-stream app)
    (close (log-stream app))
    (let ((str (log-stream app)))
      (setf (log-stream app) nil)
      (pathname str))))

(defun start-logging (filename &optional (app *application*))
  (start-logging1 app filename))

(defun stop-logging (&optional (app *application*))
  (stop-logging1 app))
