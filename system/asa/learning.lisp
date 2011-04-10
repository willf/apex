;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/learning.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: learning.lisp,v 1.2 2006/01/15 03:43:00 dalal Exp $

;;; A learning facility, part of Walter Talbott's summer project (2004).

;;;Learning is based on tasks,
;;;since tasks are what change state and have interesting properties,
;;;but the stats will be stored for the procedure and step level.  This 
;;;is because the structure, in general, of tasks is:
;;;_pstep_ picks a _task_ which picks a _procedure_
;;;so from each task, store stats for its pstep and its proc.
;;;
;;;
;;;data is stored like this:
;;;
;;;global hash table named *learning-stats*
;;; keys: procedureID/stepID made from make-proc-key-from-task and make-step-key-from-task
;;; values: attribute-keyed hash-table of learning-entries
;;;
;;;
;;;Learning-entry
;;; attribute
;;; predictor-fn
;;; data-points
;;;
;;;   -each data point has-
;;;       associated-task-id
;;;       value
;;;       conditions (var-bindings, user-specified features)
;;;
;;;
;;;The framework for storing information is as follows:
;;;
;;;First, a function is called from apex signalling that 
;;;   some interesting change has occurred.
;;;Second, a call to update-attribute, which calls
;;;   an update function specific to the attribute
;;;Third, a call to add-data-point, which creates
;;;   and manipulates data points and their values
;;;
;;;
;;;So to add an attribute to be learned, supply the following:
;;;  
;;;A function to call from Apex, which is basically a wrapper for
;;;  update-attribute.
;;;An attribute key, either a symbol, i.e. 'mean-duration
;;;  or a function to create the key, i.e. make-state-time-key
;;;An update function, which is basically a wrapper for
;;;  add-data-point.
;;;
;;;
;;;The attributes currently supported:
;;;
;;;  mean-duration
;;;  time-in-<state> for all states
;;;  state-transition-<from-state>-<to-state>
;;;     for all from and to states
;;;  num-interruptions
;;;  spawned tasks, stored for all steps
;;;  spawning tasks, stored for all procedures

(in-package :common-lisp-user)

(defvar *learning-stats*)
(defvar *learning-on*) ;flag for learning
(setf *learning-on* nil)



;;;Needs to be called before any learning can occur.  
;;;Initializes the global hash-table, and turns
;;;the learning flag on
(defun init-learning ()
  (progn
    (setf *learning-on* t)
    (setf *learning-stats* (make-hash-table :test #'equal))))

(init-learning) ;Should be called elsewhere, but I'll keep it all together for now


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data Structures: 
;;;    Learning-Entry
;;;    Data-Point
;;;    Predictor
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass learning-entry () 
  (;attribute of interest
   (attribute :accessor attribute :initarg :attribute)
   ;predictor object
   (predictor :accessor predictor :initarg :predictor :initform nil)
   ;data points
   (data-points :accessor data-points :type list :initform nil)))

(defclass data-point ()
  (;data points are associated with tasks, but must flush after each simulation so new data points can be added
   (associated-task-ID :accessor associated-task-ID :initarg :associated-task-ID)
   ;the value associated with the data-point.  Will vary depending on attribute
   (value :accessor value :initarg :value)
   ;pertinent variable bindings from when the value was attained
   (conditions :accessor conditions :initarg :conditions)
   ;for bic-split-mean to store centroid membership
   (centroid :accessor centroid :initform nil)))

(defclass predictor ()
  (;function called by get-attr to return predicted value
   (fn :accessor fn :initarg :fn :initform nil)
   ;update called periodically by add-data-point to 
   ;keep value returned by fn valuable
   (update :accessor update :initarg :update :initform nil)
   ;places to store data.  what type depends on
   ;what kind of prediction is happening.
   (data :accessor data :initarg :data :initform nil)
   (data2 :accessor data2 :initarg :data2 :initform nil)))

;;when a predictor object is created, the update is assigned based on
;;the fn.  the update for an fn is stored in this alist
(defconstant *updates-for-predictors* 
  (list (cons 'basic-mean 'basic-mean-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions Called From Apex: 
;;;    Learn-Initialized-Task
;;;    Start-Duration-Timing
;;;    End-Duration-Timing
;;;    Learn-From-Task-State-Change
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;----Storing Data

;;;These functions take a task as an argument
;;;and are called from various places in 
;;;action-sel.lisp

;;call this when a task is initiated.  
;;if step or procedure never seen before, add an entry to the proc/step hashtable
;;add task to list of spawning-tasks for proc and spawned-tasks for steps
(defun learn-initialized-task (task)
  (let* ((s-key (make-step-key-from-task task)) ;;s-key will be nil for primitives
	(p-key (make-proc-key-from-task task))) ;;p-key should always have a value?
    (if (and s-key (null (gethash s-key *learning-stats*)));outer hash-table is step/procedure keyed
	(setf (gethash s-key *learning-stats*) 
	      (make-hash-table :test #'equal))) ;inner hash-tables are attribute-keyed
    (if s-key (update-attribute #'add-task-as-value task 'spawned-tasks :no-proc t))
    (if (and p-key (null (gethash p-key *learning-stats*)))
	(setf (gethash p-key *learning-stats*)
	      (make-hash-table :test #'equal))) 
    (if p-key (update-attribute #'add-task-as-value task 'spawning-tasks :no-step t)))
  task) ;return task 
					;maybe want spawning step instead of spawning task? 

;;call this function the first time a task becomes ongoing.
;;Can only be called once per task per simulation (unless 
;;data-points have associated-task-id reset)
;;Further calls will have no effect.
(defun start-duration-timing (task)
  (update-attribute #'start-mean-duration-timing 
		    task
		    'mean-duration))

;;call this function when the task is terminated.
;;If called on a task that has not started timing,
;;there is no effect.
(defun end-duration-timing (task)
  (update-attribute #'end-mean-duration-timing
		    task
		    'mean-duration))

;;from-state is the state task is in at time of call.
;;call whenever state changes
;;use to learn:
;; state-transitions
;; time-in-state
;; num-interruptions
;; outcome?
(defun learn-from-task-state-change (task to-state)
  (progn
    (unless (null (state task)) ;null state implies task was just created.
      ;;increment correct state-transition
      (update-attribute #'learn-count 
			task 
			(make-state-change-key (state task) to-state))
      ;;end timing in from-state
      (update-attribute #'end-mean-duration-timing ;end-state-timing
			task
			(make-state-time-key (state task))))
    ;;start timing in to-state
    (update-attribute #'start-mean-duration-timing ;begin-state-timing
		      task
		      (make-state-time-key to-state))
    ;;update number of interruptions
    (if (equal to-state 'suspended)
	(update-attribute #'learn-count
			  task
			  'num-interruptions))))

(defun learn-resource-allocated (task resource)
  (update-attribute #'start-mean-duration-timing
		    task
		    (make-resource-time-key resource)))

(defun learn-resource-deallocated (task resource)
  (update-attribute #'end-mean-duration-timing
		    task
		    (make-resource-time-key resource)))

(defun learn-time-to-slack-start (task resource)
  (update-attribute #'start-mean-duration-timing
		    task
		    (make-time-to-slack-key resource)))

(defun learn-time-to-slack-end (task resource)
  (update-attribute #'end-mean-duration-timing
		    task
		    (make-time-to-slack-key resource)))

(defun learn-time-in-slack-start (task resource)
  (update-attribute #'start-mean-duration-timing
		    task
		    (make-time-in-slack-key resource)))

(defun learn-time-in-slack-end (task resource)
  (update-attribute #'end-mean-duration-timing
		    task
		    (make-time-in-slack-key resource)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update Functions: 
;;;    Update-Attribute
;;;    Learn-Count
;;;    Add-Task-As-Value
;;;    Start-Mean-Duration-Timing
;;;    End-Mean-Duration-Timing
;;;    Begin-State-Timing
;;;    End-State-Timing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Functions called from apex basically wrap update-attribute.
;;Its purpose is to store data for an attribute from a 
;;single task in two learning entries: one associated with the pstep
;;of task and one associated with the proc of task.
;;The arguments it takes are:
;;---------------------------
;;update-to-run  this argument is a function that updates the 
;;               data-points as called for by the attribute
;;               being updated.  Examples of these functions
;;               are learn-count and add-task-as-value.
;;task           the task from which the data is being extracted
;;attr-key       the attribute key for accessing the learning entry
;;               from the hash-table associated with the step
;;               and the hash-table associated with the procedure
;;&key no-step   if t, will not update the entry associated with 
;;               the task's step.  This can be either because 
;;               the task has no associated step (primitives)
;;               or because the attribute only applies at the
;;               procedure level.
;;&key no-proc   if t, will not update the entry associated with
;;               the task's procedure.  For use when an attribute
;;               applies only to the step level
;;---------------------------
(defun update-attribute (update-to-run task attr-key &key no-step no-proc)
  (let* ((step-table (and (pstep task) 
			  (gethash (make-step-key-from-task task) 
				   *learning-stats*)))
	 (proc-table (gethash (make-proc-key-from-task task) *learning-stats*)))
    (unless (or (null step-table) no-step)
      (setf (gethash attr-key step-table)
	    (funcall update-to-run 
		     (gethash attr-key step-table) task attr-key)))
    (unless (or (null proc-table) no-proc)
      (setf (gethash attr-key proc-table)
	    (funcall update-to-run 
		     (gethash attr-key proc-table) task attr-key))))) 
  
;;; --- Functions used as update-to-run in update-attribute

;;For attributes that are counters.
;;Examples: num-interruptions, all state transition attributes
;;if no data-point, create a new one with value 1.
;;if data-point exists, increment its value
(defun learn-count (le lt attr)
  (add-data-point le lt attr 1 #'basic-mean #'(lambda (x) (incf (value x)))))

;;For spawned/spawning task attributes
;;that store the task as the value
(defun add-task-as-value (le lt attr)
  (add-data-point le lt attr lt #'(lambda (le &rest unimportant) (loop for dp in (data-points le) 
						    collect (value dp))) nil))

;;For starting the timing of a task's duration
;;A task's duration is currently defined here
;;as the time from when it first becomes ongoing
;;to the time when it is set to terminated.
(defun start-mean-duration-timing (le lt attr)
  (if (factors (proc lt))
      (add-data-point le lt attr (list (current-time) 'waiting) 'lin-reg nil) ;regression when you can
    (add-data-point le lt attr (list (current-time) 'waiting) 'basic-mean nil)))

;;For ending the timing of a task's duration
(defun end-mean-duration-timing (le lt attr)
  (if (null le) ;timing ended on a task that didn't start timing, so do nothing
      nil
    (add-data-point le lt attr nil nil #'(lambda (x) 
				      (progn
			      ;;to store another data point later, if task is recurrent
					(setf (associated-task-id x) nil) 
			      ;;just store duration, no start/end times.  can easily change
					(setf (value x) (- (current-time) (first (value x))))
					x))))) ;return the data-point.  

;;For keeping track of time spent in each state.
;;Value of data points are lists of (start-time end-time) pairs
;;when timing starts on a state, the value (start-time 'WAITING) is placed
;;on the END of the list.
(defun begin-state-timing (le lt s-t-key)
  (add-data-point le lt s-t-key (list (list (current-time) 'waiting))
		  'basic-mean
                  #'(lambda (x)
		      (progn
			(setf (value x) 
			      (append (value x) 
				      (list (list (current-time) 'waiting))))
			x))))

;;For ending time spent in a state.
;;to end timing, replaces the 'WAITING
;;put in the list by begin-state-timing
;;with (current-time)
(defun end-state-timing (le lt e-t-key)
  (add-data-point le lt nil nil nil #'(lambda (x) 
				    (progn
				      (setf 
				       (second (nth (1- (length (value x))) (value x)))
				       (current-time))
				      x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data-Point Functions: 
;;;    Add-Data-Point
;;;    Find-Associated-Data-Point
;;;    Flush-Assoc-Task-IDs
;;;    Flush-ATIs-For-One-Entry
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Add-data-point is called by the Update Functions listed above.
;;Its basic functionality is to create a learning entry if one
;;has not been created for the procedure/step and attribute in
;;question.  Then, if no data point that corresponds to the 
;;current task can be found in the learning entry, it creates
;;a new data point.  If a data point can be found, it runs the
;;supplied function as an attribute specific way to update the
;;value of the attribute.
;;As arguments, it takes:
;;-----------------------
;;le          the learning entry accessed by a procedure or step 
;;            key and an attribute key.  If nil, this means no 
;;            data for the attribute has been stored, so a new
;;            instance of learning-entry is created and stored in
;;            the hash-tables.
;;lt          the task from which data is being extracted
;;attr-key    the attribute to which the data point pertains,
;;            i.e. mean-duration
;;value       if lt has not contributed a data point yet, a
;;            new one with this value is created and added to 
;;            the data-points of le
;;do-if-found if lt has contributed a data point, do-if-found
;;            is run on that data point.  Used as a way to update
;;            the value of the data point.
;;-----------------------
(defun add-data-point (le lt attr-key value predictor-fn do-if-found)
  (let* ((new-le 
	  (if (null le) 
	      (make-instance 
	       'learning-entry 
	       :attribute attr-key 
	       :predictor (make-instance 
			   'predictor
			   :fn predictor-fn
			   :update (and (factors (proc lt)) (find-update-fn predictor-fn)))) 
	    le))
	 (dp (find-associated-data-point (id lt) (data-points new-le))))
    (if (null dp)
	(unless  (null value)
	  (setf (data-points new-le) 
		(cons (make-instance 'data-point :value value 
				     :conditions (get-pertinent-var-binds 
						  lt (index (proc lt)))
				     :associated-task-id (id lt)) 
		      (data-points new-le)))
	  (unless (null (update (predictor new-le)))
	    (if (= (mod (length (data-points new-le)) 10) 2)
		(funcall (update (predictor new-le)) new-le lt attr-key))))
      (unless (null do-if-found) 
	(setf dp (funcall do-if-found dp))))
    new-le))

;returns a data-point if one has associated-task-id equal to task-id
(defun find-associated-data-point (task-id data-points)
  (find task-id data-points :test #'(lambda (x y) (equal (associated-task-id y) x))))

;;for finding the appropriate update for a predictor object
(defun find-update-fn (fn)
  (cdr (assoc fn *updates-for-predictors* :test #'equal)))

;;Each data-point needs to be associated with a task so that when the task changes,
;;it knows which data-point to update.  However, when a simulation finishes, these
;;associations must be erased so that the next run will store new data-points.
(defun flush-assoc-task-ids ()
  (maphash #'(lambda (key val) (flush-atis-for-one-entry key val)) *learning-stats*))

;;helper function for flush-assoc-task-ids
(defun flush-atis-for-one-entry (key val)
  (maphash #'(lambda (k v)
	       (and v (mapcar #'(lambda (x) (setf (associated-task-id x) nil)) (data-points v))))
	   val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make-key Functions: 
;;;    Make-Step-Key-From-Task
;;;    Make-Proc-Key-From-Task
;;;    Make-Proc-Key
;;;    Make-Step-Key
;;;    Make-State-Change-Key
;;;    Make-State-Time-Key
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;These functions return keys for accessing
;;;entries in either *learning-stats*
;;;or the attribute hash-table of
;;;an entry in *learning-stats* 

;;; all of these could probably be represented
;;; by one or two more general functions

;;returns the key for storing the information from a task 
;;in its associated step
(defun make-step-key-from-task (task)
  (if (null (pstep task))
      nil
    (remove '#\Space
	    (format nil "step-~a~a" (tag (pstep task)) (index (proc (parent task)))))))
  
;;returns the key for storing the information from a task 
;;in its associated procedure
;;
;; ! Will there be a problem when (proc task) is the same as (parent (proc task))
;;implying that task selected no procedure of its own?
(defun make-proc-key-from-task (task)
  (if (or (equal (proc task) (proc (parent task))) (null (proc task)))
      nil
    (remove '#\Space
	    (format nil "proc-~a" (index (proc task))))))

;;returns the key for retrieving stats for a procedure
(defun make-proc-key (proc)
  (remove '#\Space
	  (format nil "proc-~a" (index proc))))

;;returns the key for retrieving stats for a step
(defun make-step-key (step parent-proc)
  (remove '#\Space
	  (format nil "step-~a~a" (tag step) (index parent-proc))))

;;transitions stored as separate attributes.  i.e. ongoing->terminated and
;;ongoing->suspended are separate attributes.  A data point is a single task,
;;and the value for each data point is the number of transitions made during 
;;the life of that task.  A null learning-entry
;;for an attribute means that transition was not made for the proc/step.
;;this function returns a key for retrieving stats about state changes.
(defun make-state-change-key (from-state to-state)
  (remove '#\Space
	  (format nil "state-change-~a-~a" from-state to-state)))

;;The time a task spends in each state is stored as a separate attribute.
;;i.e. time in ongoing and time in pending are separate attribures.  A data
;;point represents a single task, and the value of the data point is a list
;;of start and end times for when that task entered and exited the respective state.
;;A null learning entry means the no task associated with the proc/step entered the state
;;this fuction returns a key for retrieving these stats.
(defun make-state-time-key (state)
 (remove '#\Space
	  (format nil "time-in-state-~a" state)))

(defun make-resource-time-key (resource)
  (remove '#\Space
	  (format nil "resource-time-~a" resource)))

(defun make-time-in-slack-key (resource)
  (remove '#\Space
	  (format nil "time-in-slack-for-~a" resource)))

(defun make-time-to-slack-key (resource)
  (remove '#\Space
	  (format nil "time-to-slack-for-~a" resource)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing Functions: 
;;;    Print-Learning-Rows
;;;    Print-All-Attributes
;;;    Print-Attributes
;;;    Print-Proc-Attributes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Helpful functions for inspecting the state
;;;of the learning data.

;;prints all keys and vals from *learning-stats*
;;keys are proc-keys or step-keys, and vals
;;are hashtables, which aren't very interesting to
;;look at
(defun print-learning-rows ()
  (maphash #'(lambda (key val)
	       (format t "key: ~a   val: ~a~%" key val) )
	   *learning-stats*))

;;prints all attributes for all steps and procedures
;;Use this if you want to see everything.
(defun print-all-attributes ()
  (maphash #'(lambda (key val)
	       (progn
		 (format t "proc/step: ~a~%" key)
		 (print-attributes key val)))
	   *learning-stats*))

;;helper function for print-all-attributes
(defun print-attributes (key val)
  (maphash #'(lambda (k v)
	       (progn 
		 (format t "   key: ~a   val: " k)
		 (and v (mapcar #'(lambda (x) (format t "~a from ~a under ~a | " (value x) (associated-task-id x) (conditions x))) (data-points v)))
		 (format t "~%")))
	   val))

;;given a task, will print all attributes associated
;;with the task's procedure.  Don't have one for steps
;;but could easily.
(defun print-proc-attributes-from-task (task)
  (maphash #'(lambda (key val)
	       (progn 
		 (format t "   key: ~a   val: " key)
		 (and val (mapcar #'(lambda (x) (format t "~a from ~a  under ~a | " (value x) (associated-task-id x))) (data-points val) (conditions val)))
		 (format t "~%")))
	   (gethash (make-proc-key-from-task task) *learning-stats*)))

(defun print-single-attribute (attr-key)
  (maphash #'(lambda (key val)
	       (progn
		 (format t "proc/step: ~a~%" key)
		 (print-one-task-one-attr attr-key key val)))
	   *learning-stats*))

(defun print-one-task-one-attr (attr-key key val)
  (let ((v (gethash attr-key val)))
    (format t "   key: ~a   val: " attr-key)
    (and v (mapcar #'(lambda (x) (format t "~a from ~a under ~a | " (value x) (associated-task-id x) (conditions x))) (data-points v)))
    (format t "~%")))  

(defun print-proc-attributes (proc)
  (maphash #'(lambda (key val)
	       (progn 
		 (format t "   key: ~a   val: " key)
		 (and val (mapcar #'(lambda (x) (format t "~a from ~a under ~a | " (value x) (associated-task-id x) (conditions x))) (data-points val)))
		 (format t "~%")))
	   (gethash (make-proc-key proc) *learning-stats*)))

;;;------------------test function
;;This is a simple test of storing
;;info based on state transitions.
;;for my benefit, mostly, and will
;;be deleted when I don't need it.
(defun test1-state-trans (task)
  (progn
    (learn-from-task-state-change task 'pending)
    (setf (state task) 'pending)
    (learn-from-task-state-change task 'ongoing)
    (setf (state task) 'ongoing)
    (learn-from-task-state-change task 'suspended)
    (setf (state task) 'suspended)))


(defun get-attr-for-proc (attr-key task)
  ;access le, access fn, run it
  (let* ((p-table (gethash (make-proc-key (proc task)) *learning-stats*))
	 (le (and p-table (gethash attr-key p-table))))
    (if (null le)
	nil
      (funcall (fn (predictor le))
	       le 
	       (proc task) 
	       (get-pertinent-var-binds task (index (proc task)))	
	       attr-key))))


(defun get-attr-for-step (attr-key task)
  (let* ((s-table (gethash (make-step-key
                            (pstep task)
                            (proc (parent task)))
                           *learning-stats*))
	 (le (and s-table (gethash attr-key s-table))))
    (if (null le)
	nil
      (funcall (fn (predictor le)) 
	       le 
	       (pstep task)
	       (get-pertinent-var-binds task (index (proc (parent task))))
	       attr-key))))


(defun basic-mean-update (le lt attr-key)
  nil)
;;this doesn't work because it might match a task exactly,
;;and set the expected-duration for the procedure to be
;;some function of only one of the procedure's matching tasks
 ; (if (equal attr-key 'mean-duration)
  ;    (setf (expected-duration (proc lt)) (get-attr-for-proc attr-key lt))))


(defun basic-mean (le proc-step vars attr-key)
  (let* ((dp-vals (try-matching-variables le vars)) ;first tries exact matches
	 (dp-vals (if (null dp-vals) (loop for dp in (data-points le) 
					   if (not (listp (value dp))) 
					   collect (value dp)) dp-vals))
	 (total (loop for val in dp-vals sum val))
	 (max (loop for val in dp-vals maximize val))
	 (min (loop for val in dp-vals minimize val))
	 (num-points (length dp-vals))
	 (avg (and (not (= 0 num-points)) (/ total num-points))))
    (if (null avg)
	nil
      (values avg (st-dev dp-vals num-points avg) max min))))

;;assumes vars are stored in the same order every time
;;is this a rational assumption?
(defun try-matching-variables (le vars)
  (let ((dps (data-points le)))
    (loop for dp in dps
	  if (and (equal vars (conditions dp)) (not (listp (value dp))))
	  collect (value dp))))

(defun st-dev (vals num-vals avg)
  (sqrt (basic-variance vals num-vals avg)))

(defun basic-variance (vals num-vals avg)
  (/ (loop for v in vals
	   sum (expt (- v avg) 2))
     num-vals))
	
(defun weed-out-unfinished (dps)
  (loop for dp in dps
	if (not (listp (value dp)))
	collect dp))

(defun avg (dps)
  (if (null dps)
      nil
    (/ (loop for dp in dps sum (value dp)) (length dps))))

(defun get-pertinent-var-binds (task index)
  (let ((vars (union (vars task) (get-local-context task) :test #'(lambda (x y) (equal (car x) (car y)))))
	(global-context (task-globals task))
	(factors (factors (proc task))))
    (if factors
	(loop for f in (replace-vars factors vars global-context :quote t :nullbind t)
	      if (numberp f)
	      collect f
	      else
	      if (not (null f))
	      collect (eval f))
      (loop for var in vars
	    if (member (first var) index)
	    collect var))))

(defun variance (dps centroids)
  (/ (loop for dp in dps sum (expt (- (value dp) (val (centroid dp))) 2))
     (- (length dps) (length centroids))))

(defun lin-reg (le proc-step vars attr-key)
  ;use data in predictor as thetas.. length of (conditions) + 1 for the intercept
  (let ((dps (weed-out-unfinished (data-points le)))
	(thetas (data (predictor le)))
	(last-update (data2 (predictor le))))
    (if (null last-update) (setf last-update 0))
    (unless (or (null dps) 
		;not sure this is working, but also not sure
		;why it isn't... does it run every time?
		(< (- (length dps) last-update) (* (length thetas) 20))
		(= 0 (length (conditions (first dps)))))
      (setf thetas (linear-regression dps)
	    last-update (length dps)))
    (if (and thetas (not (member nil vars)))
	(let ((estimate (+ (aref thetas 0 0)
			   (loop for v in vars
				 with i = 0
				 sum (* v (aref thetas (incf i) 0))))))
	  (if (< estimate 0) ;;significance of regression?
	      (basic-mean le proc-step vars attr-key)
	    estimate))
      (basic-mean le proc-step vars attr-key))))

(defun linear-regression (dps)
  (let* ((x (construct-design-matrix dps))
	(y (construct-target-value-vector dps))
	(invert (matrix:invert-matrix (matrix:multiply-matrix
						   (matrix:transpose-matrix x) x))))
    (if (not (equal invert 'matrix::singular))
	(matrix:multiply-matrix invert
				(matrix:multiply-matrix (matrix:transpose-matrix x) y))
      nil)))

(defun construct-design-matrix (dps)
  (let* ((num-examples (length dps))
	 (num-attributes (1+ (length (conditions (first dps)))))
	 (x (make-array (list num-examples num-attributes))))
    (loop for dp in dps
	  with i = 0
	  do (setf (aref x i 0) 1)
	  do (loop for f in (conditions dp)
		with j = 0
		do (setf (aref x i (incf j)) f))
	  do (incf i))
    x))
	
(defun construct-target-value-vector (dps)
  (let* ((num-targets (length dps))
	 (y (make-array (list num-targets 1))))
    (loop for dp in dps
	  with i = -1 ;so first incf makes it 0
	  do (setf (aref y (incf i) 0) (value dp)))
    y))
    