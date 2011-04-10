;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/agent.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: agent.lisp,v 1.42 2006/01/15 03:43:00 dalal Exp $


(in-package :common-lisp-user)

;;; Supporting classes

(defclass task-agenda (appob)
  ((tasks
    :type list                          ; list(task)
    :accessor tasks
    :initform nil)
   (agent                               ; containing agent
    :accessor agent
    :initarg :agent)
   (initial-task                        ; task
    :initform nil
    :accessor initial-task)
   (content-slots
    :allocation :class
    :type list
    :initform '(initial-task)
    :reader content-slots)
   (parent-slot
    :allocation :class
    :reader parent-slot
    :initform 'agent)))

(defclass monitor-array (appob)
  ((monitors
    :type list                          ;list(monitor)
    :initform nil
    :accessor monitors)
   (agent                               ; containing agent
    :accessor agent
    :initarg :agent)
   (content-slots
    :allocation :class
    :type list
    :initform '(monitors)
    :reader content-slots)
   (parent-slot
    :allocation :class
    :reader parent-slot
    :initform 'agent)))

(defclass resource-set (appob)
  ((resources
    :type list                          ;list(resource)
    :initform nil
    :accessor resources)
   (agent                               ; containing agent
    :accessor agent
    :initarg :agent)
   (content-slots
    :allocation :class
    :type list
    :initform '(resources)
    :reader content-slots)
   (parent-slot
    :allocation :class
    :reader parent-slot
    :initform 'agent)))

(defclass procedure-set (appob)
  ((procedures
    :type list                          ;list(procedure)
    :initform nil
    :accessor procedures)
   (agent                               ; containing agent
    :accessor agent
    :initarg :agent)
   (content-slots
    :allocation :class
    :type list
    :initform '(procedures)
    :reader content-slots)
   (parent-slot
    :allocation :class
    :reader parent-slot
    :initform 'agent)))


;;; This file defines a standard AGENT, a data structure for maintaining
;;; state information for the Apex Action-Selection Architecture (ASA).
;;; Typically, an agent will be associated with a set of resources including
;;; sensor (perception), effectors (motor) and other (cognitive) resources
;;; such as a memory systems.  Resource state information should maintained
;;; in some subclass of AGENT such as HUMAN.

;;; AGENT DEFINITION

(defclass agent (appob assertion-mixin ps-mixin)
  (;; same as view-location; also eye pos
   (location :accessor location 
             :initarg :location)
   ;; PDL partitions (bundles) used by this agent
   (use-bundles :accessor use-bundles 
                :initarg :use-bundles
                :initform nil)
   ;; PDL procedures usable by agent
   (procedure-set :accessor procedure-set     ; procedure-set
                  :initform (make-instance 'procedure-set :name "Procedures"))
   ;; queue of all observed but not processed events
   (cogevents :accessor cogevents 
              :initarg :cogevents
              :initform nil)
   ;; list of all waited-for cogevents
   (monitor-array                       ; Monitor-array
    :accessor monitor-array 
    :initarg :monitor-array
    :initform (make-instance 'monitor-array :name "Monitors"))
   ;; list of internally generated delayed events
   ;; information about current resource status
   (resource-allocation-table :accessor resource-allocation-table 
           :initarg :resource-allocation-table
           :initform (make-instance 'rat :monitors (make-hash-table :test 'eq)))
   ;; list of associated RA resource (structures)
   (components :accessor components 
               :initarg :components 
               :initform nil)
   ;; list of agent's resources
   (resource-set                           ; resource-set
    :accessor resource-set
    :initform (make-instance 'resource-set :name "Resources"))
   (agenda                              ; Tasks
    :initform (make-instance 'task-agenda :name "Agenda")
    :accessor agenda 
    :initarg :agenda)
   ;; variable bindings not local to any task
   (globals :accessor globals 
            :initarg :globals 
            :initform (extend-bindings '?swkld 5 (copy-bindings *bmem-init*)))
   ;;; (cons '(?swkld . 5) (copy-tree *bmem-init*)))
   ;; propositions describing current world state
   (beliefs :accessor beliefs 
            :initarg :beliefs
            :initform nil)
   ;; Initial agent task
   (initial-task :accessor initial-task 
                 :initarg :initial-task
                 :initform '(do-domain))
   ;; bias memory    
   ;; FIXME:  This needs to be looked at EJD
   (bias-memory-init 
    :accessor bias-memory-init
    :initarg :bias-memory-init
    :initform *bmem-init*)
   (component-slots
    :allocation :class
    :type list
    :initform '(resource-set agenda procedure-set monitor-array) ;; sv-memory ae-memory)
    :reader component-slots)
   (parent-slot
    :allocation :class
    :initform 'locale
    :reader parent-slot)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(use-bundles globals bias-memory-init))
   (sv-memory
    :initarg :sv-memory
    :initform (make-agent-sv-memory)
    :accessor agent-sv-memory)
   (ae-memory
    :initarg :se-memory
    :initform (make-ae-memory)
    :accessor agent-ae-memory)
   (cycle-stage 
    ;; Mainly for debugging, this is set in various places to a symbol
    ;; denoting the stage in its execution cycle the agent is in.
    :type symbol
    :initform nil
    :accessor agent-cycle-stage)
   (type-router
    ;;; for sending cogevents to just the right monitors
    :type type-router
    :initform (make-type-router)
    :accessor type-router)
   (process-lock
    :reader process-lock
    :initform (mp:make-process-lock))
   ))

;;; ! Hacks to work around hierarchy problem.  The problem is that there
;;; is human library code that checks the position of the parent object,
;;; which used to be a HUMAN but is now a resource-set.  Since
;;; resource-set is a property of AGENT, I'm defining these methods on
;;; this path of HUMAN's superclasses.  In practice, we only expect to
;;; take the position of a HUMAN (which, in addition to Agent, has
;;; Physob as a superclass, and Physobs have a valid POS method).

(defmethod pos ((x resource-set))
  (pos (agent x)))

(defmethod pos ((x agent))
  (error "Attempt to take the 'position' of a raw agent!"))

(defmethod resource-names ((a agent))
  (mapcar #'name (resources a)))

;;; The most recently created agent.
(defvar *agent* nil) 

;;; List of all existing agents.
(defvar *all-agents* nil) 

(defmethod initialize-instance :after ((agent agent) &rest initargs)
  (setq *agent* agent)
  (setq *all-agents* (append *all-agents* (list agent)))
  (setf (agent (agenda agent)) agent)
  (setf (agent (monitor-array agent)) agent)
  (setf (agent (resource-set agent)) agent)
  (setf (agent (procedure-set agent)) agent)
  (setf (apex.asa.sv:sv-memory-agent (agent-sv-memory agent)) agent)
  (setf (ae-agent (agent-ae-memory agent)) agent)
  (setf (globals agent) (extend-bindings '+self+ agent (globals agent)))
  (if (locale agent) (pushnew agent (contents (locale agent))))
  (when *proclib*
    (set-procedures agent
      (mapcar #'copy-procedure
              (filter
               #'(lambda (proc) (or (null (bundle proc))
                                    (member (bundle proc) (use-bundles agent))))
               *proclib*)))
    (when (initial-task agent) (init-agenda agent)))
  ;;
  ;; Publish subscribe router subscription
  ;;
  (subscribe agent *default-ps-router*)
  (dolist (router (getf initargs :routers))
    (subscribe agent router)))

(defun find-agent (name)                ; (symbol + string) -> Agent
  (let ((agents
         (filter #'(lambda (a) (equal (name a) name)) *all-agents*)))
    (cond ((= 1 (length agents)) (car agents))
          ((> (length agents) 1)
           (format t "~a ~a~%~a~%"
                   "Warning (find-agent): more than one agent named "
                   name
                   "Returning first.")
           (car agents))
          (t nil))))

;;; -------------------------------------------------------------------------
;;; Publish/subscribe support
;;; -------------------------------------------------------------------------

(defmethod deliver ((recipient agent) message
                    &key trigger-asa suppress-log attributes)
  ;; !- args not checked for validity
  (cogevent message recipient
            :trigger-asa trigger-asa
            :suppress-log suppress-log
            :attributes attributes))
  

;;; ------------------------------------------------------------------------
;;; ---- Propositions
;;; ------------------------------------------------------------------------

;;; ! maybe store proposition support in own library file

;;; Propositions are used to store the content of memory systems including
;;; visual-memory (associated with the vision resource) and semantic memory.

(defstruct (proposition 
	    (:print-function
	     (lambda (p s k)
               (declare (ignore k))
	       (format s "[~a ~a ~a]" (proposition-tag p) (proposition-status p)
		       (proposition-prop p)))))
  (tag (gentag "PROP")) 
  (timestamp (current-time)) ;when encoded/last-updated
  prop   ;the content
  status ;possible values: new,refined,revalued,refreshed
  superceded     ;list of props superceded by current prop; for semantic-mem
  lastretrieve   ;timestamp for last retreival
  maxage) ;default maximum lifetime of proposition (qv. Sperling-1960?)

;;; --- Defining fluents

;;; A fluent is a proposition that represents a changeable object
;;; property such as location, age or temperature.  A fluent form
;;; identifies parameters that can change.  E.g. if (temp obj1 20)
;;; changes to (temp obj1 30), that represents a change -- the 
;;; first proposition is now obsolete.  However, the proposition
;;; (temp obj2 30) does not represent a change; the old prposition
;;; still holds.

;;; *fluents* stores all fluent forms.

(defvar *fluents*)

;;; Storing fluent pattern info in a global isn't very efficient.  Better
;;; to create a table indexed by predicate.

(defmacro deffluent (propform &optional vars)
  (push (list propform vars) *fluents*)
  (values))

(defmacro def-single-value-fluents (&rest predicate-list)
  (dolist (p predicate-list) (push `((,p ?val) nil) *fluents*)))
	   
(defun find-fluent-form (p)
  (let ((binds nil) (flform nil))
    (loop for form in *fluents* do
	  (setf binds (pat-match (first form) p))
	(if binds (setf flform form)))
    (values flform binds)))

;;; -- Prop relation

;;; Determines whether one proposition refreshes, revalues, or is
;;; independent of another with respect to a given fluent-form.  A
;;; proposition is new either if it has no associated fluent form or
;;; is independent of ALL props currently in memory.

(defun prop-relation (p1 p2 p1binds fluent-form)  ;;assumes p1 matches form
  (if (equal p1 p2)
      'refreshed
    (let ((p2binds (pat-match (first fluent-form) p2)))
      (cond ((null p2binds) ;; props do not match same fluent
	     'independent)
	    ((every #'(lambda (var)  ;; props match on defining vars
			(equal (lookup var p1binds) (lookup var	p2binds)))
		    (second fluent-form))
		'revalued)
	    (t 'independent)))))  ;;props do not match on defining vars

;;; Resource set support

(defmethod resources ((a agent))        ; agent -> list(resource)
  (resources (resource-set a)))

;;; Procedure set support

(defmethod procedures ((a agent))        ; agent -> list(procedure)
  (procedures (procedure-set a)))

(defun make-agent-sv-memory ()
  (let ((mem (make-sv-memory))
	(sv (make-state-variable 'tick 'clock)))
    (start-sv-logging-policy mem sv :count-limit 500)
    mem))

;;; Application related

(defmethod initialize :after ((app application))
   (schedule-event app 0
                   (lambda ()
                     (mapc #'process-agenda *all-agents*)
                     (mapc #'asamain *all-agents*))))


(defmethod reset ((app application))    ; -> ()
   (reset-current-time app)
   (reset-ps-routers)
   (restore-instance-registry)
   (reset-object-numbers)
   (reset-events)
   (clear-event-history)
   (clear-event-tally)
   (setf (state app) 'clear)
   (setf *all-agents* nil)
   (setf (locales app) nil)
   (funcall (reset-action app))
   (values))

