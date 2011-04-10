;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/trace.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: trace.lisp,v 1.33 2006/01/15 03:43:01 dalal Exp $

;;; Event tracing system.


;;; This file contains the Apex event facilities and has these sections:
;;;
;;;     0. Classes and predefinitions.
;;;     1. API.
;;;     2. Event logging support.
;;;     3. Event filtering support.
;;;     4. Misc functions.
;;;     5. Causal Tracing.

(in-package :common-lisp-user)

;;; ---- 0. Classes and predefinitions.
;;; ---------------------------------------------------------------------

(defclass cogevent (attributes-mixin)
  ;;
  ;; Abstraction for events of interest occurring in the execution of an
  ;; Apex application.
  ;;
  ((content                ; The logged proposition/relation/description.
    :type list             ; EventExpr
    :reader content
    :initarg :content)
   (timestamp              ; Time when event was signalled or logged.
    :type integer
    :reader timestamp
    :initarg :timestamp)
   (agent                  ; Agent of this event (type is Agent instance)
    :reader agent
    :initform nil
    :initarg :agent)
   (event-type                          ; first element of content
    :type symbol
    :reader event-type
    :initarg :event-type)
   (task-id
    :type symbol
    :reader task-id
    :initform nil
    :initarg :task-id)
   (proc-id                             ; procedure id
    :type symbol
    :reader proc-id
    :initform nil
    :initarg :proc-id)
   (primitive?                          ; is the procedure a primitive?
    :type string                        ; yes or no
    :reader primitive?
    :initform nil
    :initarg :primitive?)
   (task-type                           ; first element of task description
    :type symbol
    :reader task-type
    :initform nil
    :initarg :task-type)
   (description
    :type list
    :reader description
    :initform nil
    :initarg :description)
   (source                              ; source of this event`
    ;; unsure of type (either symbol or string)
    :reader source
    :initform nil
    :initarg :source)
   (cause                               
    ;; set of measurements/episodes that caused this event
    ;; type is TBD
    :reader cause
    :initform nil
    :initarg :cause)
   (trigger
    ;; final measurements/episode that precipitated this event
    ;; type is TBD
    :reader trigger
    :initform nil
    :initarg :trigger)
   (resource
    ;; type is either symbol or resource instance
    :reader resource
    :initform nil
    :initarg :resource)
   (deferred-task
    ;; id of "loser" task in (resource-allocated ...) event
    :type integer
    :reader deferred-task
    :initform nil
    :initarg :deferred-task)
   ;; ASA support: list of tasks (partially) enabled by this cogevent
   (hits 
    :type list
    :accessor hits
    :initarg :hits
    :initform nil)
   ;; Flag indicating whether this event should be matched by a monitor
   (match-required
    :reader match-required
    :initform nil
    :initarg :match-required)
   ;; An explanation of why a match should be required
   ;; (goes with match-required)
   (match-rationale
    :type string
    :reader match-rationale
    :initform ""
    :initarg :match-rationale)
   ))

(defmethod event-type ((x cogevent))
  (and (content x) (first (content x))))

;; -- removed; using timeseries of logged values instead.
(defclass trace-info ()
  ;;
  ;; A singleton class containing the current event history and
  ;; trace-related settings.
  ;;
  ((history                             ; List of events that have occurred.
    :type timeseries
    :accessor ti-history
    :initform (make-timeseries))
  (hms-time?				; Flag for hours/mins/secs time display.
    :type boolean
    :accessor ti-hms-time?
    :initform nil)
   (runtime-trace?			; Flag to show trace during app run.
    :type boolean
    :accessor ti-runtime-trace?
    :initform t)
   (trace-spec				; Event trace specification.
    :accessor ti-trace-spec
    :initform nil)))

(defmethod initialize-instance :after ((ti trace-info) &rest initargs)
  ;;
  ;; Creates a default trace-spec for trace-info.
  ;;
  (declare (ignore initargs))
  (setf (ti-trace-spec ti) (make-instance 'trace-spec)))

(defclass trace-spec ()
  ;;
  ;; Vestigial abstraction that maintains current trace specification
  ;; settings.  ! Factor into trace-info
  ;;
  ((level                    ; ShowLevel
    :accessor level
    :initform 'none          ; 'none level shows no events.
    :initarg :level)
   (constraint               ; Expression denoting trace constraint.
    :type list               ; TraceConstraintExpr
    :accessor constraint
    :initform nil            ; nil constraint means show no events.
    :initarg :constraint)
   (trace-filter             ; Filter function constructed from constraint.
    :type function           ; Event -> bool
    :accessor trace-filter
    :initform #'(lambda (event)
                  (declare (ignore event)) 
                  nil)
    :initarg :trace-filter)))

(defclass event-trace (appob)
  ;;
  ;; Abstraction for an event trace, which just wraps the event list.
  ;; ! This is a wimpy class.  Is it really useful?
  ;;
  ((events
    :type list        ; list(Event)
    :accessor events
    :initarg :events
    :initform '())))


(defvar *trace-info*
;;;    ;; The singleton instance of Trace-Info
   (make-instance 'trace-info))

;;; ---- 1. Application Programming Interface
;;; ---------------------------------------------------------------------

;;; Flags controlling whether events are stored locally (in
;;; participating objects) and globally (in the singleton trace-info).
;;; Mainly useful for limiting memory usage, as it is possible that the
;;; event storage mechanism generates garbage.
;;;
(defvar *record-events-locally* t)
(defvar *record-events-globally* t)

(defmethod log-event (eventform &key source cause trigger
                                     agent attributes
                                     (timestamp (current-time)))
  ;;
  ;; EventForm, Opt(Agent), opt(Attributes) -> ()
  ;; Creates and processes a proposition event.
  ;;
  (let ((event (make-event eventform source cause trigger
                           agent attributes timestamp nil nil)))
    (record-event event)))

(defun make-event (eventform source cause trigger agent attributes timestamp
                   match-required match-rationale)
  (multiple-value-bind
      (task-id proc-id task-type description deferred-task
       primitive? resource)
      (extract-event-form-parts eventform)
    (make-instance 'cogevent
      :content eventform
      :agent agent
      :timestamp timestamp
      :event-type (car eventform)
      :task-id task-id
      :proc-id proc-id
      :primitive? primitive?
      :task-type task-type
      :description description
      :source source
      :cause cause
      :trigger trigger
      :resource resource
      :deferred-task deferred-task
      :match-required match-required
      :match-rationale match-rationale
      :attributes attributes)))

(defmacro signal-event (form &key agent attributes)
  ;;
  ;; EventForm, Opt(Agent), Opt(Attributes) -> Any
  ;; Evaluates the given form with the side effect of recording an event
  ;; that form as its content.
  ;;
  `(progn
     (log-event ',form :agent ,agent :attributes ,attributes)
     ,form))
       

(defmacro setx (place value &key agent attributes)
  ;;
  ;; any, any, opt(Agent), opt(Attributes) -> any
  ;; Event-logging wrapper for SETF.  Behaves just like SETF, with the
  ;; additional side effect of logging an event.  The event will have
  ;; the event form (OBJ PROP VALUE) when PLACE is the list (OBJ PROP),
  ;; and (PLACE VALUE) otherwise.
  ;;
  ;; Example: (setx (temperature cake) 110
  ;;            :agent agent
  ;;            :attributes '((:source temp-sensor1)))
  ;;
  (if (and (consp place) (= 2 (length place))) ; unary variable expression
      (let ((obj (first place)))
        `(let ((prop ,(second place)))
           (log-event (list ',obj prop ,value)
                      :agent ,agent
                      :attributes ,attributes)
           (setf ,place ,value)))
    ;; symbolic variables and non-unary variable expressions
    `(progn
       (log-event (list ',place ,value)
                  :agent ,agent
                  :attributes ,attributes)
       (setf ,place ,value))))


(defmacro show (&optional specifier &rest params)
  ;;
  ;; :runtime + :hms + (:level * ShowLevel) + EventType + TraceConstraintExpr
  ;; -> ()
  ;; Specify what to show in trace.
  ;;
  (cond 
   ((null specifier)
    `(show-current-trace-constraint))
   ((eq specifier :runtime)
    '(turn-on-runtime-trace))
   ((eq specifier :hms)
    '(turn-on-hms-mode))
   ((and (eq specifier :level)
         (= 1 (length params))
         (potential-show-level? (car params)))
    `(let ((level ',(car `,params)))
       (if (lookup-show-level level)
           (set-trace-level ',(car `,params))
         (user-error 'show (format nil "No show level named ~a" level)))))
   ((potential-event-type? specifier)
    `(filter-in-event-type ',specifier))
   ;; !! In general this won't work for more than one time range.
   ;; Composing time ranges correctly would require more work.
   ((time-range-expression? specifier)
    `(constrain-trace ',specifier))
   ((trace-constraint-expression? specifier)
    `(filter-in ',specifier))
   (t 
    `(user-error 'show (format nil "Invalid arguments: ~a ~a" ',specifier
                               (if (null ',params) "" ',params))))))


(defun turn-on-runtime-trace ()
  (setf (ti-runtime-trace? *trace-info*) t))

(defun turn-off-runtime-trace ()
  (setf (ti-runtime-trace? *trace-info*) nil))

(defun turn-on-hms-mode ()
  (setf (ti-hms-time? *trace-info*) t))

(defun turn-off-hms-mode ()
  (setf (ti-hms-time? *trace-info*) nil))

(defun filter-in-event-type (etype) ; EventType -> ()
  (filter-in `(event-type ,etype)))

(defun filter-out-event-type (etype) ; EventType -> ()
  (filter-out `(event-type ,etype)))

(defmacro unshow (&optional specifier)
  ;;
  ;; 'runtime + 'hms + ShowLevel + TraceConstraintExpr -> ()
  ;; Specify what NOT to show in trace.
  ;;
  (cond 
   ((null specifier) 
    '(set-trace-level 'none))
   ((eq specifier :runtime)
    '(turn-off-runtime-trace))
   ((eq specifier :hms)
    '(turn-off-hms-mode))
   ((potential-event-type? specifier)
    `(filter-out '(event-type ,specifier)))
   ((trace-constraint-expression? specifier)
    `(filter-out ',specifier))
   (t 
    `(user-error 'unshow (format nil "Invalid argument: ~a" ',specifier)))))


;;; there was a bug in the previous version of DEFINE-SHOW-LEVEL
;;; which led to a memory leak. This version will rebind a show level
;;; name.

(defvar *show-levels*
    ;;
    ;; Alist (symbol, TraceConstraintExpr)
    ;; Registry for show levels.
    ;;
    '((none . (or)) (all . t)))

(defun define-show-level* (name constraint)
  (declare (special *show-levels*))
  (let ((pair (assoc name *show-levels*)))
    (if pair
      (setf (cdr pair) constraint)
      (setq *show-levels* 
	(cons (cons name constraint) *show-levels*)))
    (if pair
      (values name :rebound)
      (values name :new))))

(defmacro define-show-level (name constraint)
  ;;
  ;; symbol * TraceConstraintExpr -> symbol
  ;; Define a show level with given name and associated constraint.
  ;;
  `(define-show-level* ',name ',constraint))

(defun generate-trace (&optional (stream *standard-output*)) ; stream -> ()
  ;; Generate a trace based on current trace constraint.
  (let ((filter (trace-filter (get-current-trace-spec))))
    (timeseries-for-each/filtered 
     (get-event-history)
     #'(lambda (value timepoint)
       (declare (ignore timepoint))
       (format stream "~a~%" value))
     #'(lambda (value timepoint)
	 (declare (ignore timepoint))
	 (funcall filter value))))
    (values))

;;; For Listener use: displays in readable format
;
(defun show-trace ()
  (with-readable-trace
    (generate-trace)))

(defun filter-events/count ()
  (let ((cnt 0)
	(filter (trace-filter (get-current-trace-spec))))
    (timeseries-for-each/filtered 
     (get-event-history)
     #'(lambda (value timepoint)
       (declare (ignore value timepoint))
       (incf cnt))
     #'(lambda (value timepoint)
	 (declare (ignore timepoint))
	 (funcall filter value)))
    cnt))

(defun save-trace (filename)            ; string -> ()
  ;;
  ;; Generates trace and saves it to a file (without printing on screen)
  ;; If filename contains a slash (/) or colon (:) the implied directory
  ;; path (absolute, or relative to current directory) will be used;
  ;; otherwise the file is created in the current simworld's directory.
  ;; If the trace is empty no file is written.
  ;;
  ;;
  (cond ((not (stringp filename))
	 (format t "Argument must be a string!"))
	(t (with-open-file (out (compute-trace-pathname filename)
			    :direction :output :if-exists :supersede)
	     (let ((cnt 0)
		   (filter (trace-filter (get-current-trace-spec))))
	       (timeseries-for-each/filtered 
		(get-event-history)
		#'(lambda (value timepoint)
		    (declare (ignore timepoint))
		    (incf cnt)
                    (with-readable-trace
                      (format out "~a~%" value)))
		#'(lambda (value timepoint)
		    (declare (ignore timepoint))
		     (funcall filter value)))
	       (when (= cnt 0)
		 (format t "No events in trace.~%")
		 (format t "You may need to run the sim or set a trace level."))))))
    (values))


(defun compute-trace-pathname (filename) ; string -> pathname + string
  ;; 
  ;; Compute where to store trace.  If filename contains a slash or
  ;; colon, assume it to be a complete specification (i.e. absolute or
  ;; relative directory ending with file name) and return it.  Otherwise
  ;; use the current simworld's directory if a simworld exists.
  ;;
  (if (or (position #\/ filename)
          (position #\: filename)
          (null *application*))
      filename
    (merge-pathnames (application-directory)
                     (make-pathname :name filename))))


(defun filter-in (constraint)
  ;;
  ;; TraceConstraintExpr -> ()
  ;; Admit events meeting the given constraint.
  ;;
  (let ((current-constraint (constraint (get-current-trace-spec))))
    (set-trace-constraint `(or ,constraint ,current-constraint))))


(defun constrain-trace (constraint)
  ;;
  ;; TraceConstraintExpr -> ()
  ;; Add the given constraint to current trace constraint
  ;;
  (let ((current-constraint (constraint (get-current-trace-spec))))
    (set-trace-constraint `(and ,constraint ,current-constraint))))


(defun filter-out (constraint)
  ;;
  ;; TraceConstraintExpr -> ()
  ;; Exclude events meeting the given constraint.
  ;;
  (let ((current-constraint (constraint (get-current-trace-spec))))
    (set-trace-constraint 
     `(and (not ,constraint) ,current-constraint))))


(defun set-trace-constraint (constraint)
  ;;
  ;; TraceConstraintExpr -> ()
  ;; Effect a new trace constraint.
  ;;
  (let ((spec (get-current-trace-spec)))
    (setf (constraint spec) constraint)
    (setf (trace-filter spec) (make-trace-filter constraint))
    (values)))


(defvar *recorded-event-types*
    ;;
    ;; list(symbol)
    ;; List of event types that have have been recorded.
    ;;
    '())



;;; ---- 2. Event Logging Support
;;; ---------------------------------------------------------------------

;;; Trace Tally

;;; This facility tallies the number of events of given type.  Its
;;; purpose is to support canonical display of events in Sherpa.

;;; Question: should we tally the number of *unique* events of each
;;; event type?

(defvar *event-tally* (make-hash-table)) ; symbol -> int

(defun clear-event-tally ()             ; -> ()
  (clrhash *event-tally*)
  (values))

(defun tally-event (eventform)          ; list -> ()
  (let* ((event-type (car eventform))
         (tally (or (gethash event-type *event-tally*) 0)))
    (setf (gethash event-type *event-tally*) (1+ tally))
    (values)))

(defun event-type-count (event-type)         ; symbol -> int
  (or (gethash event-type *event-tally*) 0))

(defun recorded-event-types ()
  (let (event-types)
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (push k event-types)) *event-tally*)
    event-types))
    

(defun extract-event-form-parts (eventform)
  (let* ((event-type (car eventform))
         (task (unless (eq event-type 'resource-deallocated)
                 (find-if (lambda (x) (typep x 'task)) eventform)))
         (task-id (if task (id task)))
         (description (if task (specify-task-description task) eventform))
         (proc-id (if task (id (proc task))))
         (task-type  (if description (car description)))
         (primitive?
          (if task (if (typep (proc task) 'primitive-procedure) "yes" "no")))
         (deferred-task
          (if (member event-type '(resource-deallocated conflict-resolved))
              (id (car (reverse eventform)))))
         (resource
          (cond ((eq event-type 'resource-allocated) (third eventform))
                ((eq event-type 'resource-deallocated) (second eventform))
                (t nil)))
         )
    (values task-id proc-id task-type description deferred-task
            primitive? resource)))


;;; Trace support


(defmethod event-object-ids ((e cogevent))
  ;;
  ;; Event -> list(symbol)
  ;; Return ID's of all Appobs in an event's content.
  ;;
  (object-ids (filter-appobs (content e))))


(defmethod event-task-types ((e cogevent))
  ;;
  ;; Event -> list(symbol)
  ;; Return TaskTypes of all Tasks in an event's content.
  ;;
  (task-types (filter-appobs (content e) 'task)))


(defmethod record-event ((ev cogevent))
  ;;
  ;; Event -> () Stores an event in the history of each of its
  ;; participants, as well as the history of the simulation.  (!
  ;; Redundancy is deliberate for now).  Prints the event if needed.
  ;;
  (if (and
       (runtime-trace?)
       (funcall (trace-filter (get-current-trace-spec)) ev))
      (format t "~a~%" ev))
  
  ;; HACK: substituting this (the old way) to make vision work
  ;;       -- also needed for pert chart generation.
  (when *record-events-locally*
    (dolist (part (content ev))
      (when (typep part 'appob)
	(add-to-history ev part))))
  
  (if *record-events-globally* (add-event-to-history ev))

  (tally-event (content ev))

  (register-recorded-event-type (event-type ev))
  
  ;; The following is the new way
  ;;  (mapcar #'(lambda (entity)  ; Appob + Simulation -> ()
  ;;             (add-to-history ev entity))
  ;;        (get-participants ev)))
  )

(defconstant quoted-format "")

(defmethod print-object ((e cogevent) stream)
  ;; Event -> ()
  (let ((*print-pretty* nil)) ; ! should this be set to nil globally?
    (if (eq 'sherpa *trace-destination*)
        (format stream
                (concatenate 'string
                  ;; definite fields
                  "(event (timestamp \"~a\") (event-type \"~a\")"
                  ;; possible fields
                  "~a~a~a~a~a~a~a~a~a~a~a)")
                (time-format (timestamp e))
                (event-type e)
                (if (agent e) (format nil "(agent \"~a\")" (agent e)) "")
                (if (task-id e) (format nil "(task-id \"~a\")" (task-id e)) "")
                (if (proc-id e) (format nil "(proc-id \"~a\")" (proc-id e)) "")
                (if (primitive? e) (format nil "(prim? \"~a\")" (primitive? e))
                     "")
                (if (task-type e) (format nil "(task-type \"~a\")" (task-type e))
                  "")
                (if (description e)
                    (format nil "(description \"~a\")" (description e))
                  "")
                (if (source e) (format nil "(source \"~a\")" (source e)) "")
                (if (cause e) (format nil "(cause \"~a\")" (cause e)) "")
                (if (trigger e) (format nil "(trigger \"~a\")" (trigger e)) "")
                (if (resource e) (format nil "(resource \"~a\")" (resource e)) "")
                (if (deferred-task e)
                    (format nil "(deferred \"~a\")" (deferred-task e))
                  "")
                )
      (format stream "[~a~a] ~a"
              (time-format (timestamp e))
              (if (agent e)
                  (format nil " ~a" (name (agent e)))
                "")
              (content e)))
    (values)))


(defmethod add-to-history ((e cogevent) (s appob))
  ;; Event * Appob -> ()
  (push e (history s)))


(defun extract-cause (e)
  ;;
  ;; Args -> Args * Cause
  ;;
  (cond ((null e) (values nil nil))
        ((eq (car e) :cause)
         (values (cddr e) (cadr e)))
        (t (multiple-value-bind (content cause)
               (extract-cause (cdr e))
             (values (cons (car e) content) cause)))))

;;; This a helper function for filter-appobs.  An "inner appob" is one
;;; somewhere "inside" an appob that should be included in the filter.
;;;
;;; ! Instances of this method might be mutually recursive with
;;; filter-appobs, and an infinite recursion would occur if two objects
;;; have each other as inner appob.
;;;
(defmethod inner-appobs ((x appob) &optional (type 'appob))
  (declare (ignorable x type))
  nil)

(defun filter-appobs (expr &optional (type 'appob))
  ;;
  ;; list(Any) -> list(Appob)
  ;; Collects Appob-valued elements of a list.
  ;; ! list argument is assumed
  ;; ! Can make more efficient (e.g. tail recursion)
  ;;
  (cond ((null expr) '())
        ((typep (car expr) type)
         (append (list (car expr))
                 (inner-appobs (car expr) type)
                 (filter-appobs (cdr expr) type)))
        ((typep (car expr) 'cogevent)
         (filter-appobs (content (car expr)) type))
        ((consp (car expr)) (append (filter-appobs (car expr) type)
                                    (filter-appobs (cdr expr) type)))
        (t (filter-appobs (cdr expr) type))))


(defun make-initial-event (content)
  ;;
  ;; EventExpr -> Event
  ;; Event that has no cause (i.e. beginning of a causal chain such as
  ;; start of simulation).
  ;;
  (make-instance 'cogevent
                 :content content))

(defun task-types (tasks)
  ;;
  ;; list(Task) -> list(symbol)
  ;; Return list of Task Types (first element of procedure's index) from
  ;; a list of tasks.
  ;;
  (remove nil
          (mapcar (lambda (task) (car (description task)))
                  tasks)))

(defun object-ids (obs)  
  ;;
  ;; list(Appob) -> list(symbol)
  ;; Return ID's of a list of Appobs.  ! Might not need this abstraction.
  ;;
  (mapcar #'id obs))


(defun short-prop-expr? (e)
  ;;
  ;; Any -> bool
  ;; Predicate for identifying a "short proposition expression", e.g.
  ;;   (filled #{glass-1})
  ;;
  (and (prop-expr? e)
       (typep (second e) 'appob)))


(defun long-prop-expr? (e)
  ;;
  ;; Any -> bool
  ;; Predicate for identifying a "long proposition expression", e.g.
  ;;   (seen new (color #{car-1} blue))
  ;;
  (and (prop-expr? e)
       (short-prop-expr? (second e))))


(defun function-call-expr? (e)
  ;;
  ;; Any -> bool
  ;; Predicate for identifying a function call expression, e.g.
  ;;   (complete-activity #{looking-1} #{car-1})
  ;;
  (and (consp e)
       (fboundp (first e))))
  

(defun prop-expr? (e)
  ;;
  ;; Any -> bool
  ;; Predicate for identifying a proposition expression.
  ;;
  (and (consp e)
       (> (length e) 2)
       (symbolp (first e))))



;;; ---- 3. Event filtering support
;;; ---------------------------------------------------------------------



(defun show-current-trace-constraint () ; () -> ()
  (format t "~(~a~)~%" (constraint (get-current-trace-spec)))
  (values))

(defmethod print-object ((spec trace-spec) stream)
  (format stream "#{Trace Spec}"))


(defun set-trace-level (level)
  ;;
  ;; ShowLevel -> ()
  ;; Effect a new show level for traces.
  ;;
  (let ((spec (get-current-trace-spec))
        (new-constraint (lookup-show-level level)))
     (if new-constraint
         (progn  
           (setf (level spec) level)
           (setf (constraint spec) new-constraint)
           (setf (trace-filter spec) (make-trace-filter new-constraint))
           (values))
       (user-error 'set-trace-level 
                   (format nil "No such show level: ~a" level)))))

(defun lookup-show-level (name)
  ;; symbol -> TraceConstraintExpr + nil
  (cdr (assoc name *show-levels*)))


;;; ! The following "potential" predicates could be replaced by more
;;; robust predicates if a registry of legal candidates is created.

(defun potential-show-level? (x)
  ;; Any -> bool
  (and x (or (integerp x) (symbolp x))))

(defun potential-event-type? (x)
  ;; Any -> bool
  (symbolp x))


(defmethod print-object ((et event-trace) stream)
  ;; EventTrace * stream -> ()
  (dolist (e (events et) (values))
    (format stream "~a~%" e)))

;;; Trace parameters whose 'value' is a symbol; distinguished only for
;;; ease of handling in the code.
(defparameter *symbol-valued-trace-parameters*
    '(event-type task-type object-id agent-name agent-id))

(defparameter *trace-parameters*
    (cons 'time-range *symbol-valued-trace-parameters*))

(defun trace-constraint-expression? (x)
  ;; Any -> bool
  (or (trace-parameter? x)
      (boolean-expression? x)))


(defun make-trace-filter (constraint)
  ;;
  ;; TraceConstraintExpr -> Event -> bool
  ;;
  ;; This is the top level function of a compiler which constructs a
  ;; filtering function from a constraint expression.
  ;;
  (eval `(function (lambda (event)
                     (declare (ignorable event))
                     ,(expand-constraint constraint 'event)))))


;;; The remaining functions in this section construct the body for a
;;; filter function (Event -> bool) with formal parameter E.

(defun expand-constraint (c e)
  ;; TraceConstraintExpr * symbol -> Any
  (cond
   ((null c) nil) ; the null constraint accepts no events
   ((eq c 't) t)  ; the 't' constraint accepts all events
   ((trace-parameter? c t) (expand-trace-parameter c e))
   ((boolean-expression? c t) (expand-boolean-expression c e))
   (t (error "Illegal trace constraint syntax:~%~a" c))))


;;; NOTE: The following predicates do "deep" syntax checks unless the
;;; second parameter is non-NIL.  Shallow checks are fine for the
;;; expander, because they are fast and a complete check happens at the
;;; leaf level.

(defun trace-parameter? (c &optional shallow?)
  ;; TraceConstraintExpr * bool -> bool
  (and (consp c) ; ! remove this check?
       (if shallow? 
           (member (car c) *trace-parameters*)
         ;; ! The following is awful!  Pattern matching would be better.
         (or
          (and (member (car c) *symbol-valued-trace-parameters*)
               (= 2 (length c))
               (symbolp (second c)))
          (time-range-expression? c)))))

(defun time-range-expression? (c)       ; cons -> bool
  (and (eq (car c) 'time-range)
       (= 2 (length c))
       (valid-time-range? (second c))))

(defun valid-time-range? (times)        ; any -> bool
  (and 
   (consp times)
   (= 2 (length times))
   (integerp (first times))
   (integerp (second times))
   (>= (second times) (first times))))

(defun boolean-expression? (c &optional shallow?)
  ;; TraceConstraintExpr * bool -> bool
  (and (consp c)  ; ! remove this check?
       (member (car c) '(and or not))
       (or shallow?
           (every #'trace-constraint-expression? (cdr c)))))

(defun expand-trace-parameter (param event)
  ;; TraceParameterExpr * symbol -> Any
  (let ((pname (first param))
        (pvalue (second param)))
    (cond
     ((eq pname 'event-type)
      `(eq ',pvalue (event-type ,event)))
     ((eq pname 'agent-name)
      `(if (agent ,event) (equal ',pvalue (name (agent ,event)))))
     ((eq pname 'agent-id)
      `(if (agent ,event) (eq (lowercase-intern-id ',pvalue) (id (agent ,event)))))
     ((eq pname 'object-id)
      `(member (lowercase-intern-id ',pvalue) (event-object-ids ,event)))
     ((eq pname 'task-type)
      `(member ',pvalue (event-task-types ,event)))
     ((eq pname 'time-range)
      `(and (>= (timestamp ,event) ,(first pvalue))
            (<= (timestamp ,event) ,(second pvalue))))
     (t (error "Invalid trace parameter: ~a" param)))))

(defun expand-boolean-expression (expr event)
  ;; TraceConstraintExpr * symbol -> Any
  (let ((type (car expr))
        (operands (cdr expr)))
    (case type
      ((and or) 
       `(,type ,@(mapcar 
                  #'(lambda (c) (expand-constraint c event)) 
                  operands)))
      (not
       (if (= 1 (length operands))
           `(not ,(expand-constraint (first operands) event))
         (error "Illegal trace constraint: ~a" expr)))
      (t (error "Illegal trace constraint: ~a" expr)))))


(defun filter-events (predicate events)
  ;;
  ;; (event -> bool) * list(event) -> list(event)
  ;;
  (let ((result nil))
    (dolist (e events result)
      (if (funcall predicate e)
        (push e result)))))


(defun register-recorded-event-type (type)
  ;; symbol -> ()
  (pushnew type *recorded-event-types*)
  (values))


(defun reset-events ()
  ;; () -> ()
  (setq *recorded-event-types* '())
  (values))



;;; ---- 4. Misc functions
;;; ---------------------------------------------------------------------

(defun get-current-trace-spec ()        ; -> Trace-Spec
  (ti-trace-spec *trace-info*))

(defun runtime-trace? ()                ; -> bool
  (ti-runtime-trace? *trace-info*))

(defun hms-time? ()                     ; -> bool
  (ti-hms-time? *trace-info*))

(defun hms-string-no-zeros (hrs mins secs frac)
  (cond
   ((and (zerop hrs) (zerop mins))
    (if (zerop frac) 
      (princ-to-string secs)
      (format nil "~d.~3,'0,d" secs frac)))
   ((zerop hrs)
    (format nil "~d:~2,'0,d.~3,'0,d" mins secs frac))
   (t (format nil "~d:~2,'0,d:~2,'0,d.~3,'0,d" hrs mins secs frac))))

(defun time-format (n &optional (zeros t)) ; int * opt(bool) -> string
  ;;
  ;; Return the time representation in either milliseconds or the format
  ;; "hours:minutes:seconds:milliseconds", depending on the current mode
  ;; set for trace output.
  ;;
  (if (hms-time?)
      (multiple-value-bind (hrs mins secs frac)
          (apex.utility.datetime:ms2hms n)
	(if zeros
	  (format nil "~2,'0,d:~2,'0,d:~2,'0,d.~3,'0,d" hrs mins secs frac)
	  (hms-string-no-zeros hrs mins secs frac)))
      (format nil "~a" n)))

(defmethod add-event-to-history ((e cogevent)) ; Event -> ()
  (timeseries-insert (ti-history *trace-info*) (timestamp e) e  :filter-in-p t))

(defun get-event-history ()             ; -> List(Event)
  (ti-history *trace-info*))

(defun clear-event-history ()           ; -> ()
  (setf (ti-history *trace-info*) (make-timeseries)))

(defun reset-trace-info ()		; -> ()
 (setq *trace-info* (make-instance 'trace-info)))

(defun user-warning (message)           ; string -> ()
  (format t (format nil "User Warning: ~a~%" message)))

(defun user-error (construct message)   ; symbol * string -> ()
  (format t (format nil "Error in ~a: ~a~%" construct message)))


;;; 5. Causal Tracing

;;; Crude and simple experimental stuff right now...

;;;(defun causal-trace ()
;;;  (dolist (e (get-event-history))
;;;    (show-causal-trace e)))

(defun causal-trace ()
  (timeseries-for-each 
   (get-event-history)
   (lambda (value tp)
     (declare (ignore tp))
     (show-causal-trace value))))

(defmethod show-causal-trace ((e cogevent))
  ;; Event -> ()
  (show-causal-trace1 e 0 *standard-output*))

(defmethod show-causal-trace1 ((e cogevent) col (s stream))
  ;; Event * int * stream -> ()
  (dolist (c (cause e))
    (show-causal-trace1 c (+ 2 col) s))
  (format s "~a~a~%" (indent col) e))

(defun indent (size)
  ;; int -> string
  (make-string size :initial-element #\Space))

;;; 
;;;
;;; WITH-TRACE-LEVEL 
;;;
;;; e.g. (with-trace-level 'all (save-trace "trace.txt"))
;;;
;;;

(defun copy-trace-info ()
  (let ((ti (make-instance 'trace-info)))
    (setf (slot-value ti 'history) (ti-history *trace-info*))
    (setf (slot-value ti 'hms-time? ) (ti-hms-time? *trace-info*))
    (setf (slot-value ti 'runtime-trace? ) (ti-runtime-trace? *trace-info*))
    (setf (slot-value ti 'trace-spec ) (make-instance 'trace-spec))
    ti))
      
(defmacro with-trace-level (level &body body)
  `(let ((*trace-info* (copy-trace-info)))
     (set-trace-level ,level)
     ,@body))