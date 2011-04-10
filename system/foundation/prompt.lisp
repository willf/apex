;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/prompt.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: prompt.lisp,v 1.8 2006/01/15 03:43:01 dalal Exp $

;;; Prompt-driven interface for Apex applications.

;;; Based on facilities in utility/menu.lisp.


(in-package :common-lisp-user)

;;; Command specifications of type Menu.  Extensions to this interface
;;; can be achieved largely by augmenting these.  Supporting functions
;;; and type definitions found in system/utility/menu.lisp.

(defparameter *toplevel-commands*       
    '(((l load) "load an application" prompt-load-application)
      ((s start) "start the application" prompt-start)
      ((r reset) "reset application" prompt-reset)
      ((ss step) "advance application one step" prompt-step)
      ((rl reload) "reload current application" prompt-reload)
      ((p pause) "pause options" prompt-pause)
      ((t trace) "trace options" prompt-trace)
      ((i inspect) "object inspection options" prompt-inspect)
      ((f format) "toggle time display format" prompt-toggle-time-display-mode)
      ((d display) "toggle display of trace"
       prompt-toggle-runtime-trace-display)
      ((e eval) "evaluate a Lisp expression" prompt-eval)
      ((h help ?) "print this message" explain-toplevel)
      ((q quit exit) "exit to Lisp prompt" prompt-quit)
      ((ex system) "exit Apex & Lisp to system prompt" exit)))

(defparameter *inspection-commands*
    '(((o objects) "display the object hierarchy" prompt-inspect-objects)
      ((i inspect) "inspect an object" prompt-inspect-inspect)
      ((t terse) "toggle terse mode" prompt-inspect-toggle-terse)
      ((n null) "toggle display of null object slots" prompt-inspect-toggle-null)
      ((h help ?) "print this message" explain-inspection)
      ((q quit exit) "return to toplevel menu" prompt-inspect-quit)))

(defparameter *pause-commands*
    '(((r rem) "remove pauses" prompt-pause-remove)
      ((t time) "set pause time" prompt-pause-time)
      ((c cycle) "set pause cycle" prompt-pause-cycle)
      ((tr trial) "toggle pausing after each trial" prompt-pause-trial)
      ((i init) "toggle pausing after initialization" prompt-pause-init)
      ((h help ?) "print this message" explain-pausing)
      ((q quit exit) "return to toplevel menu" prompt-pause-quit)))


(defun apex ()				; -> ()
  "Top level function for prompt interface to Apex application."
  (toplevel-menu
   "Welcome to Apex.  Type h for list of available commands."
   "Apex"
   *toplevel-commands*))

;;; ---------- Begin section A: functions for *toplevel-commands* --------------

(defun prompt-reset ()
  (application-command #'resetapp))

(defun prompt-step ()
  (application-command #'stepapp))

(defun prompt-reload ()
  (application-command #'reloadapp))

(defun prompt-start ()
  (application-command #'startapp))

(defun prompt-toggle-time-display-mode ()
  (cond ((hms-time?)
         (turn-off-hms-mode)
         (format t "Displaying time in milliseconds."))
        (t
         (turn-on-hms-mode)
         (format t "Displaying time in hours:minutes:seconds format.")))
  (values))

(defun explain-toplevel ()
  (explain "The available commands are:"
           *toplevel-commands*))

(defvar *prompt-show-recent-app-pathname*
    ;;
    ;; When true, the pathname of an application is shown along with its
    ;; name in the loading menu."
    ;;
    t)

(defun get-recent-app-selections () ; -> list(string)
  (if *prompt-show-recent-app-pathname*
      (mapcar #'pathname-to-string (get-recent-application-pathnames))
    (get-recent-application-names)))

(defun prompt-load-application ()
  (if *application*
      (format t "Current application is ~a.~%" (app-name *application*)))
  (let* ((other "<enter a new application>")
         (app (get-menu-selection "Recent Applications:"
                                 "Select an application to load"
                                  (append (get-recent-app-selections)
                                          `(,other)))))
    (if app
        (if (equal other app)
            (process-keyboard-input
             "Enter the application file as a string:"
             #'(lambda (x) (and
                            (or (stringp x) (pathnamep x))
                            (probe-file x)))
             #'(lambda (file) (load-application-file file))
             "Input must be a string naming an existing file")
          (load-application-file
           (if *prompt-show-recent-app-pathname*
               app
             (cdr
              (assoc app
                     (getf *apex-info* 'load-history) :test #'equal))))))))


(defun application-command (command)    ; (() -> ()) -> ()
  (if *application*
      (funcall command)
    (format t "There is no application.  Please load one!~%")))

;;; End Section A.

;;;----------- Begin Section B: functions for *pause-commands* -----------------

(defun prompt-pause ()
  (explain-pausing)
  (catch 'pause-finished
    (loop (process-menu-command "Apex-Pause" *pause-commands*))))

(defun prompt-pause-remove ()
  (set-pause-cycle nil)
  (set-pause-time nil)
  (pause-after-trial nil)
  (pause-after-init nil)
  (values))

(defun prompt-pause-time ()
  (process-keyboard-input
   "Enter time (duration expression) at which to pause sim"
   (lambda (x) (time-expression? x))
   #'set-pause-time
   "Invalid time representation"))

(defun prompt-pause-cycle ()
  (process-keyboard-input
   "Enter number of simulation events to pause after"
   (function (lambda (x) (and (integerp x) (> x 0))))
   (function set-pause-cycle)
   "Input must be an integer > 0"))

(defun prompt-pause-trial ()
  (let ((pausing? (pause-after-each-trial? *application*)))
    (pause-after-trial (not pausing?))
    (format t "~aPausing after each simulation trial."
            (if pausing? "NOT " ""))
    (values)))

(defun prompt-pause-quit ()
  (throw 'pause-finished nil))

(defun prompt-pause-init ()
  (let ((pausing? (pause-after-initialization? *application*)))
    (pause-after-init (not pausing?))
    (format t "~aPausing after simulation initialization."
            (if pausing? "NOT " ""))
    (values)))

(defun explain-pausing ()
  (explain "The available pause commands are:"
           *pause-commands*))

;;; End Section B

;;; ---------- Begin Section C: functions for *inspection-commands* ------------

(defun prompt-inspect ()
  (explain-inspection)
  (catch 'inspection-finished
    (loop (process-menu-command "Apex-Inspect" *inspection-commands*))))

(defun prompt-inspect-objects ()
  (format t "~a~%" (all-vobs))
  (values))

(defun prompt-inspect-inspect ()
  (not-implemented))

(defun prompt-inspect-toggle-null ()
  (not-implemented))

(defun prompt-inspect-toggle-terse ()
  (not-implemented))

(defun prompt-inspect-quit ()
  (throw 'inspection-finished nil))

(defun explain-inspection ()
  (explain "The available object inspection options are:"
           *inspection-commands*))

;;; End Section C

;;; ---------- Begin Section D: functions for *trace-commands* ------------

(defparameter *trace-commands*
    '(((t trace) "generate trace" prompt-trace-generate)
      ((l level) "set show level" prompt-trace-showlevel)
      ((s show) "show current trace constraint" show-current-trace-constraint)
      ((a add) "add an event type to trace" prompt-trace-add)
      ((x exclude) "exclude an event type from trace" prompt-trace-remove)
      ((r range) "limit trace to a time range" prompt-trace-time)
      ((n new) "specify a new trace constraint"  prompt-trace-new)
      ((f file) "save trace to file" prompt-trace-save)
      ((e eval) "evaluate a Lisp expression" prompt-eval)
      ((h help ?) "print this message" explain-tracing)
      ((q quit exit) "return to toplevel menu" prompt-trace-quit)))

(defun prompt-trace ()
  (explain-tracing)
  (catch 'trace-finished
    (loop (process-menu-command "Apex-Trace" *trace-commands*))))

(defun prompt-trace-showlevel ()
  (let ((level (get-menu-selection "Defined Show Levels:"
                                   "Select a show level"
                                   (mapcar #'car *show-levels*))))
    (when level
      (set-trace-level level)
      (show-current-trace-constraint))))

(defun prompt-trace-generate ()
  (format t "~a" (show-trace))
  (values))

(defun prompt-trace-save ()
  (process-keyboard-input
   "Enter filename"
   (function stringp)
   (function save-trace)
   "Filename must be a string"))
   
(defun prompt-trace-add ()
  (announce-current-trace-constraint)
  (operate-on-event-type (function filter-in-event-type)))

(defun prompt-trace-remove ()
  (announce-current-trace-constraint)
  (operate-on-event-type (function filter-out-event-type)))

(defun prompt-trace-new ()
  (announce-current-trace-constraint)
  (process-keyboard-input
   "Enter trace constraint"
   (function trace-constraint-expression?)
   (function (lambda (x)
               (set-trace-constraint x)
               (show-current-trace-constraint)))
   "Invalid trace constraint expression"))

(defun prompt-trace-time ()
  (process-keyboard-input
   "Enter a time range of form (m n)"
   (function valid-time-range?)
   (function (lambda (tr)
               (constrain-trace (list 'time-range tr))
               (show-current-trace-constraint)))
   "Invalid time range"))
               
(defun prompt-trace-quit ()
  (throw 'trace-finished nil))

(defun announce-current-trace-constraint ()       ; -> ()
  (format t "Current trace constraint is:~%")
  (show-current-trace-constraint))

(defun operate-on-event-type (fn)
  ;;
  ;; (EventType -> ()) -> ()
  ;; Prompt for event type and take upon it the given action.
  ;;
  (process-keyboard-input
   "Enter event type"
   (function symbolp)
   (function (lambda (x)
               (funcall fn x)
               (show)))
   "Event type must be a symbol."))

(defun explain-tracing ()
  (explain "The available trace options are:"
           *trace-commands*))


(defun prompt-toggle-runtime-trace-display ()
  (cond ((runtime-trace?)
         (turn-off-runtime-trace)
         (format t "Trace not displayed during run."))
        (t
         (turn-on-runtime-trace)
         (format t "Trace displayed during run.")))
  (values))

;;; End Section D
