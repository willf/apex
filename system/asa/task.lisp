;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/task.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: task.lisp,v 1.2 2006/03/15 23:25:12 will Exp $

(in-package :common-lisp-user)


;;; This file contains the class definition for the TASK structure
;;; as well as common functions and methods.


;;; == Task

;;; The local variable binding context for a task is a property of it's parent,
;;; not itself.  Thus, the VAR slot contains variable bindings available to
;;; child tasks while (task-var (task-parent self)) is the local context.

(defclass task (appob attributes-mixin)
  (;;activity descriptor (with unbound variables)
   (description :accessor description :initarg :description :initform nil)
   ;;pending|enabled|engaged|ongoing|suspended|terminated 
   (state :accessor state :initarg :state :initform nil)
   ;;preconditions specd by WAITFOR + if-satisified: disj of conjs   
   (monitors :accessor monitors :initarg :monitors :initform nil)
   ;;pointer to sponsor task 
   (parent :accessor parent :initarg :parent :initform nil)
   ;;resource-activity started by task (only for start-activity primitive tasks)  
   (action :accessor action :initarg :action :initform nil)
   ;;agent to whom the task belongs
   (agent :accessor agent :initarg :agent :initform nil)
   ;;pointer to procedure structure used to derive children  
   (proc :accessor proc :initarg :proc :initform nil)
   ;;normal|special|primitive|asa-act depending on selected proc 
   (tasktype :accessor tasktype :initarg :tasktype :initform nil)
   ;;child tasks formed after refinement OR activity object started by task
   (children :accessor children :initarg :children :initform nil)
   ;;step structure within parent procedure definition
   (pstep :accessor pstep :initarg :pstep :initform nil)
   ;;local var binding context for child tasks (not self!)
   (vars :accessor vars :initarg :vars :initform nil)
   ;;variable history.. tracks when vars were bound; used for periodicity mgt
   (vhist :accessor vhist :initarg :vhist :initform nil)
   ;;var|var-list to which val returned by terminated subtask is bound
   (returnvar :accessor returnvar :initarg :returnvar :initform nil)
   ;;body of select clause if any. Form: (<var> <code>)
   (select :accessor select :initarg :select :initform nil)
   ;;estimated time required to carry out task
   (duration :accessor duration :initarg :duration :initform nil)
   ;;copied from step.. not determined until method-selection
   (profile :accessor profile :initarg :profile :initform nil)
   ;;criterial-events from PRIORITY clause
   (priority-parameters :accessor priority-parameters :initform nil
                        :initarg :priority-parameters)  
   ;;last computed priority value 
   (priority :accessor priority :initarg :priority :initform nil)
   ;;interrupt cost if any, expressed as an evaluable expression
   (icost :accessor icost :initarg :icost :initform .1)
   ;;precedence with respect to siblings for resources; supercedes priority
   (rank :accessor rank :initarg :rank :initform nil)
   ;;<expr> determines whether task auto-restarts at reftime
   (restart-if :accessor restart-if :initarg :restart-if :initform nil)
   ;;refractory period.. time required after last exec for full priority
   (refract :accessor refract :initarg :refract :initform nil)
   ;;if = T (may be expression), reptask begins with all waitfors satisfied
   (rep-state :accessor rep-state :initarg :rep-state :initform nil)
   ;;reference event for refract/restart == enabled|started|terminated
   (reftime :accessor reftime :initarg :reftime :initform nil)
   ;;pointer to previous task instance if enable-time repetition
   (spawned-by :accessor spawned-by :initarg :spawned-by :initform nil)
   ;;task enablement timestamp
   (t-started :accessor t-started :initarg :t-started :initform nil)
   ;;previous task instance timestamp (terminated ref-time for PERIOD clause)
   (t-last :accessor t-last :initarg :t-last :initform nil)
   ;;task creation timestamp
   (t-created :accessor t-created :initarg :t-created :initform nil)
   ;;task terminated timestamp
   (t-terminated :accessor t-terminated :initarg :t-terminated :initform nil)  
   ;; state variable for task status
   (t-sv :accessor t-sv :initarg :t-sv :initform nil)
   ;; Function to call upon task termination, as specified in the
   ;; on-end step clause of the parent procedure
   (termination-action
    :accessor termination-action
    :initform nil)
   (containing-agenda                   ; task-agenda
    :accessor containing-agenda)
   (content-slots
    :allocation :class
    :reader content-slots
    :initform '(children))
   (logging-policies 
    :accessor logging-policies
    :initform '()
    :initarg :logging-policies
    :documentation "Logging policies for this task's agent's sv-history")
   (vars-to-unbind                      ; list of PDL variables
    :accessor vars-to-unbind
    :initform nil
    :documentation
    "Variables in the task's monitors to unbind when the task repeats")
   (min-rep-interval                    ; duration
    :accessor min-rep-interval
    :initform nil
    :documentation
    "Minimum duration to expire in between repititions of this task")
   
   ;; ! A hack to support on-start and on-end step level clauses in the
   ;; presence of :for-new (responding), which require the local
   ;; context of the task *prior* to the unbinding action in
   ;; live-reinstantiate.
   (saved-context
    :accessor saved-context
    :initform nil)
   ;;
   (earliest-start
    :accessor earliest-start
    :initform nil
    :documentation "An earliest point in time before which this task is not to be started."
    )
   
   ;; tracking repitition and fragment calls 
   
   (repitition-number 
    :accessor repitition-number 
    :initform 1
    :initarg :repitition-number 
    :documentation "Which repitition is this task on?")
   
   (fragment-number 
    :accessor fragment-number 
    :initform 0 ;; incremented on entering 'on-going'
    :initarg :fragment-number 
    :documentation "Which fragment is this task on?")
   
   ))


(defmethod default-rep-state ((task task))
  (cond
   ((repeating-task-p task) t)
   ((responding-task-p task) nil)
   (t nil)))

(defmethod repeating-task-p ((task task))
  (and (pstep task) (repitition (pstep task)) t))

(defmethod responding-task-p ((task task))
  (and (pstep task) (response-policy  (pstep task)) t))

(defmethod initialize-instance :after ((task task) &rest initargs)
  (declare (ignore initargs))
  (unless (rep-state task) ;; i.e., set on initialization ...
    (setf (rep-state task)
      (default-rep-state task))))

(defmethod appob-parent ((x task))
  (let ((parent (parent x)))
    (if (and parent (not (equal '(root) (description parent))))
        parent
      (containing-agenda x))))

(defmethod start-of ((task task))
  (t-started task))

(defun negnull (x) 
  (or x most-negative-fixnum))

(defmethod earliest-start-of ((task task))
  (max (negnull (start-of task))
       (negnull (earliest-start task))
       (negnull (t-created task))))


(defmethod end-of ((task task))
  (t-terminated task))


(defmethod name ((task task)) (format nil "~a" (description task)))


;;; TASK's description slot is a heterogenious list.  The following ADT
;;; is under development.  A better name for this slot might be ACTION.
;;;
;;; ---- begin

(defun description-action (desc)        ; TaskDescription -> symbol
  ;; name of primitive or PDL procedure being invoked
  (first desc))                         

(defun description-resource (desc)      ; TaskDescription -> symbol
  ;; if action is start-activity, this is the name of its resource
  (second desc))

;;;; ---- end

;;; See trace.lisp for explanation of this function.
(defmethod inner-appobs ((x task) &optional (type 'appob))
  (filter-appobs (specify-task-description x) type))

(defmethod print-object ((task task) s)
  (format s "#{~a ~a" (id task)
          (if (description task)
              (abbreviate-task-description (specify-task-description task))
	      "<no description>"))
  (when (or (> (repitition-number task) 1)
	    (> (fragment-number task) 1))
    (format s " ~a/~a" (fragment-number task) (repitition-number task)))
  (princ #\} s))

(defun abbreviate-task-description (expr)
  (mapcar 
   #'(lambda (item)
       (if (typep item 'task)
	   (format nil "#{~a}" (id item))
	 item))
   expr))

;;; task specification (variable binding) requires access to globals variables
;;; associated with the agent.  This function allows access to globals as if
;;; they were a TASK class variable.

(defun task-globals (task)
  (globals (agent task)))

(defmethod task-memory ((task task))
  (agent-sv-memory (agent task)))


(defvar *this-task* NIL "The currently visible task")

(defmacro with-task (task &body body)
  `(let ((*this-task* ,task))
    (declare (special *this-task*))
    ,@body))



(defun abbreviated-task-printforms (expr)
  (mapcar 
   #'(lambda (item)
       (cond
	((consp item)
	 (abbreviated-task-printforms item))
	((typep item 'task)
	 (format nil "#{~a}" (id item)))
	(t item)))
   expr))


(defun rootp (task)
  (equal (description task) '(root)))

;;; ----------------------------------------------------------------------------
;;; ---- Initializing the agenda
;;; ----------------------------------------------------------------------------

;;; Puts an initial set of goals on the agenda: BUILT-IN (including
;;; reflexes and general-skills) and DO-DOMAIN (which intiates basic
;;; behaviors for the domain of interest.

(defun init-agenda (agent)
  (set-tasks agent
    (let* ((root (make-init-task '(root) nil agent t))
           (initial-task-instance
            (make-init-task (initial-task agent) root agent)))
      (setf (parent root) root) ;;!circularity may cause trouble
      (setf (initial-task (agenda agent)) initial-task-instance)
      (list initial-task-instance)))
  (mapc #'attempt-resource-grab (tasks agent)))

(defun make-init-task (act parent agent &optional (root nil))
  (let ((now (current-time)))
    (make-instance 'task
     :agent agent
     :description act
     :state (if root 'ongoing 'engaged)
     :parent parent
     :reftime 'terminated
     :refract 0
     :t-last -10000
     :priority 0   ;;no priority but exec'd anyway because no resources required
     :fragment-number 1 ;; nothing else to set this
     :t-created now
     :t-started now)))

;;; ----------------------------------------------------------------------------
;;; ---- Context Handling
;;; ----------------------------------------------------------------------------

;;; Context handling deals with variable bindings.  Most of this code
;;; relies the specific implementation of the pattern-matching code
;;; store in pat-match.lisp.

;;; ------ Context handling

;;; Context refers to the state (variable binding) information to which
;;; a task has direct access.  LOCAL-CONTEXT refers to local var
;;; bindings shared with sibling tasks and accessible through the parent
;;; task.  Local context is formed by task instantiation and by values
;;; returned as a result of executing siblings (see the => construct).
;;; Local context handling functions assume use of the pattern-matching
;;; functions found in Peter Norvig's book, Paradigms of AI Programming.

;;; BIAS-CONTEXT consists of procedure-specific variables accessible to
;;; tasks directly generated by the procedure.  Unlike local variables,
;;; which are named in the procedure definition, bias-var names are
;;; created at procedure load time.  For simplicity of implementation,
;;; these are treated as global variables.

;;; A task's context includes its local context, bias context, plus
;;; certain globally accessible values, especially including the
;;; subjective workload variable ?swkld.

(defun get-local-context (task)
  (let ((parent (parent task)))
    (if (or (null parent) (rootp parent))
	no-bindings
      (vars parent))))

(defmethod set-local-context ((task task) bindings)
  (if (parent task)
      (progn
        (setf (vars (parent task)) bindings)
        (values))
    (error "Task ~a has no parent!" task)))

;;; Note: global context acquired through TASK object using function
;;; GLOBALS

;;; Extend local context adds variable binding information to the vars
;;; slot of a task.  If self is nil (default), then the var slot of the
;;; TASK's parent is used since this is where TASK's binding context is
;;; held.  If self, then TASK's var slot is used.  If a source value is
;;; provided, then an entry of the form (<var> <source>) is added to the
;;; vhist slot of task, thus providing history information for bindings.

(defun extend-local-context (task new-binds &optional self source)
  (let* ((local (if self task (parent task)))
	 (ctxt (vars local)))
    (when (not (equal new-binds no-bindings))   ;;do nothing if nothing to add
      (dolist (bind new-binds)              ;;else add each new binding
	(setf ctxt (extend-binding-list bind ctxt))
	(if source (setf (vhist local)
		     (cons (list (binding-var bind) source) (vhist local)))))
      (setf (vars local) ctxt))
      ctxt))

;;; ---- Extend-binding-list 

;;; Conses a binding of the form (?var . val) to a list of such bindings
;;; but handles two special cases: 1. when the list is "empty" ---
;;; i.e. equals ((t . t)) --- the dummy value is removed; 2. when a
;;; binding with the same variable name already exists in the list, the
;;; new supercedes the old.

;;; cdr used for case-2 should be the function BINDING-VAL but this was
;;; not defined to be settable in the pattern matcher I'm using
;;; (borrowed from Norvig 1991).  It may be worth restoring modularity
;;; here in case a different pattern matcher is eventually used.

(defun extend-binding-list (new-bind bindlist)
  (let* ((var (binding-var new-bind))
	 (old-bind (get-binding var bindlist)))
    (cond ((equal bindlist no-bindings)
	   (list new-bind))
	  (old-bind  ;;no attempt to check if binding-val has changed
	   (setf (cdr old-bind) (binding-val new-bind)) 
	   bindlist)
	  (t
	   (cons new-bind bindlist)))))

;;; -- Task binding history

(defun task-binding-history (task)
  (vhist (parent task)))

;;; -- Exclude binds by source

;;; Returns all task binds not generated by task designated by SOURCE.
;;; Used to live-reinstantiate a task.  Note: this assumes that binding
;;; history is only recorded if generated in response to fulfillment of
;;; a (waitfor) precondition.  No history is kept for bindings generated
;;; by procedure selection or task output.

(defun exclude-binds-by-source (bindlist history source)
  (let ((retval
	 (loop for bind in bindlist
	     when 
	       (let ((var (binding-var bind)))
		 (not (find-if #'(lambda (h) 
				   (and (equal var (first h)) (equal source (second h))))
			       history)))
	     collect bind)))
;;; For debugging
;;;    (when (not (equal retval bindlist))
;;;      (format t "---- excluded binds!~%")
;;;      (format t "Difference = ~a~%" (set-difference bindlist retval)))
    retval))


;;; -- Conditional-unbind 

;;; Removes a binding and associated history if the binding was
;;; generated by the task designated by SOURCE.  Allows reset waitfor
;;; preconditions in repeated tasks to supply different binding values
;;; on successive iterations.

;;; ! as above, this assumes no binding history is kept except for those
;;;   generated by precodnition fulfillment.  To unbind these,
;;;   TASK=SOURCE
;;; ! this would be much cleaner if every binding structure contained
;;; its own history -- i.e. what task generated the binding.  Is loss of
;;; efficiency from expanding the binding rep worth it?

(defun conditional-unbind (task source) 
  (let ((parent (parent task)))
    (setf (vars parent)
      (exclude-binds-by-source (vars parent) (vhist parent) source))
    (setf (vhist parent) nil)))


;;; -- Copy binding list

(defun copy-binding-list (bl) (mapcar #'copy-list bl))

;;; ---- Replace-vars

;;; Replace-vars swaps variables for their values within a pattern based
;;; on the availablility of appropriate bindings.  If quote, values are
;;; quoted (i.e. ?x given (?x . armadillo) becomes (quote armadillo).
;;; This allows variable replaced expressions to be eval'd as LISP
;;; expressions.  If nullbind, all variables not present in the bindings
;;; list are bound to nil in the result.
;;
;; ! KMD: added unquote-numbers as a hack to prevent quoting of numbers
;;; when quote is T.

(defun stored-task (bindings)
  (binding-value (get-binding '?this-task bindings)))

(defun replace-vars (pat bindings globals &key quote nullbind unquote-numbers)
  (with-task (or (stored-task bindings)
		 (stored-task globals)
		 *this-task*)
    (replace-vars* pat bindings globals quote nullbind unquote-numbers)))

(defun replace-vars* (pat bindings globals quote nullbind unquote-numbers)
  (cond ((null pat) nil)
	((variable-p pat) 
	 (let* ((bind (find-binding pat bindings globals))
		(bindval (and bind (binding-val bind))))
	   (if (null bind) 
	     (and (not nullbind) pat)
             (if quote
	       (if (and unquote-numbers (numberp bindval))
		 bindval
		 `(quote ,bindval))
               bindval))))
;;; ! KMD: the following line was previously in place of the preceding 'if
;;; quote' expression.  Uncomment this when I'm certain there are no
;;; issues.
;;;	     (or (and quote `(quote ,bindval)) bindval))))
	((atom pat) pat)
	((consp pat)
	 (if (equal (first pat) '?if) (setf quote t)) ;; handles embedded lisp
         (cons (replace-vars* (car pat) bindings globals
			      quote nullbind  unquote-numbers)
               (replace-vars* (cdr pat) bindings globals
                              quote  nullbind unquote-numbers)
	       ))))

;;; doesn't differentiate between nil binding and no binding
(defun find-binding (var bindlist &optional globals)
  (flet ((handle-if-bias (bind)
	   (if (and (consp (cdr bind)) (eql :bias (second bind)))
	       (cons (first bind) 
		     (and (third bind) (fourth bind)
			  (>= (fourth bind) (- (current-time) (third bind)))))
	       bind)))
    (or (get-binding var bindlist) 
	(handle-if-bias (get-binding var globals)))))

(defun eval-spcl (pat task)
  (eval (replace-vars pat (get-local-context task) 
		      (task-globals task) :quote t)))

(defun specify-task-slot (task slot)
  (replace-vars (slot-value task slot) (get-local-context task)
		(task-globals task)))

(defun get-subjective-workload (agent)
  (get-binding '?swkld (globals agent)))


;;; ----------------------------------------------------------------------------
;;; ---- Generic TRANSITION-TASK method
;;; ----------------------------------------------------------------------------
;;; 
;;; This is the generic transition-task method. The idea is that one
;;; writes EQL specializers that do the actual work of managing the
;;; task transition. In this way, new methods for transitions are easy
;;; (or, just easier) to add for other processing models. This was
;;; added to aid in the support of the Plexil universal executive.
;;;
;;; Thus, instead of a DEFUN
;;; 
;;; (defun enable-task (task &optional suppress-reinstantiate) ...)
;;;
;;; one writes:
;;;
;;; (defmethod transition-task 
;;;  ((task task) (to-state (eql 'enabled)) &rest args)
;;;   (let ((suppress-reinstantiate (car args))) ...))
;;;

(defgeneric transition-task (task state &rest args))

;;; ----------------------------------------------------------------------------
;;; ---- Generic and general VALID-TASK-TRANSITION-P method
;;; ----------------------------------------------------------------------------

;;; Is it ok to transition from this task state to another? In
;;; general, no; but we can write eql specializers.
;;;
;;; We also need to keep a list of possible task states.

(defvar +possible-task-states+ NIL "list of possible task states.")

(defgeneric valid-task-transition-p (system from-state to-state))
(defmethod  valid-task-transition-p ((system symbol) (from-state symbol) (to-state symbol)) NIL)

(defmacro def-valid-transition (system from to)
  `(progn
     (pushnew ',from +possible-task-states+)
     (pushnew ',to   +possible-task-states+)
     (defmethod valid-task-transition-p ((system (eql ',system)) (from-state (eql ',from)) (to-state (eql ',to))) T)))

;;; (def-valid-transition :apex :foo :bar)

(defmacro def-valid-transitions (system list)
  (let ((from (car list))
	(tos (cadr list)))
  `(progn
     ,@(loop for to in tos collecting `(def-valid-transition ,system ,from ,to)))))

;;; The CAR of the list is the FROM state
;;; The CADR is a list of valid TO states
;;; For example:
;;;
;;; (def-valid-transitions :apex (:foo (:bar :baz :bim :bam :bop)))

(defmacro def-valid-transition-network (system &rest lists)
  `(progn
     ,@(loop for list in lists collecting `(def-valid-transitions ,system ,list))
     ,(let ((cnt 0))
	(mapcar 
	 (lambda (list)
	   (incf cnt (+ (length (second list)) 1)))
	 lists)
	cnt)))

;;; Each list is as for def-valid-transitions
;;;
;;; (def-valid-transition-network :test
;;;    (:foo (:bar :baz :bim :bam :bop))
;;;    (:bar (:foo)))


