;;;-*- Mode: Lisp; Package: :cl-user -*-
;;; ------------------------------------------------------------------------
;;; File:           $Id: procdefs.lisp,v 1.43 2006/03/16 15:57:48 will Exp $
;;; Created:        1997
;;; Author:         The Apex Team
;;;
;;;
;;; Procedure/step classes and language.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :common-lisp-user)


;;; - Helper macros and functions ...
;;; ------------------------------------------------------
;;; this allows adding to, or replacing, a global
;;; association list, based on the car of an assication
;;; It's used for defining procedure and step requirements
;;; 'in place'--i.e., where the procedure and step level
;;; clauses are defined.

(defmacro define-assoc-in-global (global-var assoc)
  (let ((l (gensym))
	(assocvar (gensym)))
    `(let* ((,assocvar ,assoc)
	    (,l (member (car ,assocvar) ,global-var
			:test
			(lambda (here there)
			  (eql here (car there))))))
       (if ,l 
	 (setf (cdr (car ,l)) (cdr ,assocvar))
	 (setq  ,global-var
	   (nconc ,global-var
		  (list ,assocvar)))))))

(defun gentag (prefix)
  (gentemp (string-upcase (format nil "~a" prefix))))


;;; ---------------------------------------------------------------------------
;;; ------  Procedures and Steps
;;; ---------------------------------------------------------------------------

;;; Procedures are the primary syntactic construct in the APEX procedure
;;; language, defining the agent's long-term how-to knowledge.  This
;;; class is the superclass for the internal representations of all
;;; procedure types.  There is a close (but not exact) mapping between
;;; the slots of this class and the clauses of procedures.  See the Apex
;;; Reference Manual for a description of the syntax.


(defclass procedure (appob)
  (;; Index clause
   (index
    :accessor index
    :initform nil
    :initarg :index)
   ;; !- to be obsoleted
   ;; if special, steps contains an evaluable LISP function
   (proctype :accessor proctype :initarg :proctype :initform 'normal)
   ;; Profile clause
   (profile :accessor profile :initarg :profile :initform nil)
   ;; Original PDL text
   (pdl :accessor pdl :initarg :pdl :initform nil)
   ;; canonicalized pdl
   (canonical-pdl :accessor canonical-pdl :initarg :pdl :initform nil)
   ;; ! Walter
   ;; Slot filled by learning
   (expected-duration
     :accessor expected-duration
     :initarg :expected-duration
     :initform nil)
   ;; variables and factors that affect duration
   ;; (factors
   ;; :accessor factors
   ;; :initarg :factors
   ;; :initform nil)
   ;;
   ;; Actual duration
   (duration                            ; TimeExpression +
                                        ; () -> TimeExpression
    :accessor duration
    :initarg :duration
    :initform nil)
   ;; Bundle (procedure set) to which this procedure belongs.
   (bundle
    :accessor bundle
    :initarg :bundle
    :initform *apex-bundle*)
   (containing-procedure-set                      ; procedure-set
    :accessor containing-procedure-set)
   (parent-slot
    :allocation :class
    :initform 'containing-procedure-set
    :reader parent-slot)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(pdl containing-procedure-set proctype))
   (logging-policies 
    :documentation "Polices for logging to sv-histories."
    :accessor logging-policies
    :initarg :logging-policies
    :initform '())
   (termination-conditions
    :documentation "Procedure-level termination conditions"
    :accessor termination-conditions
    :initarg :termination-conditions
    :initform '())
   ))

(defmethod name ((procedure procedure))
  (format nil "~a" (index procedure)))

;;; ---------------------------------------------------------------------------
;;; ----- Procedure Library
;;; ---------------------------------------------------------------------------


;;; Currently all procedures are stored in a single library.  Action- selection
;;; mechanisms search the library whenever a task is to be refined (decomposed
;;; into subtasks).

(defvar *proclib* nil)

(defun search-proclib (idx)
  (find-if #'(lambda (p) (equal idx (index p))) *proclib*))

(defmethod procedure= ((proc1 procedure) (proc2 procedure)) ; -> bool
  ;; An equality predicate sufficient for testing for redefinition.
  (and
   (equal (index proc1) (index proc2))
   (equal (bundle proc1) (bundle proc2))))

(defparameter *warn-pdl-redefinition* t)

(defun install-procedure-in-library (new-proc)    ; procedure -> ()
  ;; First check if already defined
  ;; ! This is straightforward but not efficient.  Would be better to use a 
  ;; hash table for *proclib* (kmd)
  (when (member new-proc *proclib* :test #'procedure=)
    (when *warn-pdl-redefinition*
      (format t "Warning: procedure ~a redefined~%" (index new-proc))
      (if *load-truename* (format t " - using version in ~a~%"
                                  *load-truename*)))
    (setq *proclib* (remove new-proc *proclib* :test #'procedure=)))
  (push new-proc *proclib*)
  (values))

(defvar *bmem-init* nil "Gratuitous and useless global variable")

;; Was begin-loading-procedures 
(defun flush-asa ()
  ;; Include a maintain-memory-bias procedure as a temporary hack
  ;; The mechanism of how this works will need to be examined later.
  (setf *loaded-apex-libraries* nil) ;; Ouch we need to re-require everything
  (setf *proclib* (list (make-procedure (cons '(index (maintain-memory-bias)) nil))))
  (setf *all-agents* nil) ;; This also needs to be added elsewhere. 
  (setf *fluents* nil)
  (setf *bmem-init* nil))

(defmethod load-application-file :before (file)
  (declare (ignore file))
  (flush-asa))



;;; Copying

(defmethod copy-procedure1 ((source procedure)) ; -> procedure
  ;;
  ;; Create a practical (e.g. "deep enough") copy of a procedure
  ;; instance.  This is done by copying the bound slots of the source's
  ;; object's most immediate class (e.g. primitive-procedure) and the
  ;; bound slots of the PROCEDURE class.  We do NOT copy the slots of
  ;; the higher superclasses (Appob and Id-Mixin).
  ;;
  (let*
      (;; create the copy
       (target (make-instance (class-of source)))
       ;; get slot names of source's most immediate class
       (specific-slots
        (mapcar #'clos:slot-definition-name
                (clos:class-direct-slots (class-of source))))
       ;; unless source is an instance of PROCEDURE, get slot names of PROCEDURE
       (procedure-slots
        (if (not (eq 'procedure (class-of source)))
            (mapcar #'clos:slot-definition-name
                    (clos:class-direct-slots
                     (class-of (make-instance 'procedure)))))))
    ;; "copy" the slot values from source to target
    (loop for slot-name in (append specific-slots procedure-slots)
        do 
          (if (slot-boundp source slot-name)
              (setf (slot-value target slot-name)
                (copy-for-procedure (slot-value source slot-name)))))
    target))

(defmethod copy-for-procedure ((x list))
  (copy-list x))

(defmethod copy-for-procedure ((x t))
  x)


(defmethod print-object ((ob procedure) s)
  (format s "#{~a ~a}" (id ob) (index ob)))

(defmethod add-procedure ((p procedure) (a agent)) ; -> ()
  (let ((ps (procedure-set a)))
    (push p (procedures ps))
    (setf (containing-procedure-set p) ps)
    (values)))

(defmethod set-procedures ((a agent) procs) ; agent * list(procedure) -> ()
  (loop for p in procs do (add-procedure p a))
  (values))

;;; == Non-primitive procedure

;;; A non-primitive procedure has the syntactic form '(procedure ...)'
;;; in PDL.  This class it its internal representation.

(defclass non-primitive-procedure (procedure)
  (;; Representation of the procedure's 'step' clauses
   (steps :accessor steps :initarg :steps :initform nil)
   ;; Representation of the ASSUMES clause (! currently broken)
   (assumes :accessor assumes :initarg :assumes :initform nil)
   ;; ! Not sure (KMD)
   (interrupt-cost :accessor interrupt-cost :initarg :interrupt-cost
		   :initform 0.1) ;; small default "momentum"
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(assumes))
   ))

(defmethod copy-procedure ((source non-primitive-procedure))
;; non-primitive-procedure  -> non-primitive-procedure 
  (copy-procedure1 source))

;;; Steps are the main element of procedures.  Each step may have a variety 
;;; of properties depending on associated step-level clauses.  The main ones
;;; are waitfors (preconditions), selection rules (used to select between
;;; alternative procedures during refinement), period parameters (defining
;;; repetition) and priority information.


(defclass pstep (appob)
  ((tag :accessor tag :initarg :tag :initform nil)
   (activity :accessor activity :initarg :activity :initform nil)
   ;;step body.. describes what to do
   (waitfors :accessor waitfors :initarg :waitfors :initform nil)
   ;;preconditions
   (returnvar :accessor returnvar :initarg :returnvar :initform nil)
   ;; variable form following => if any in step body; binds to execution result
   (select :accessor select :initarg :select :initform nil)
   ;; body of select clause if any
   (forall :accessor forall :initarg :forall :initform nil)
   ;; multiply instanitiates tasks; form is (?item ?list)
   (period :accessor period :initarg :period :initform nil)
   (repitition :accessor repitition :initform nil)
   (response-policy :accessor response-policy :initform nil)
   (interrupt-cost :accessor interrupt-cost :initarg :interrupt-cost
		   :initform 0)
   (rank :accessor rank :initarg :rank :initform nil)
   (criteria :accessor criteria :initarg :criteria :initform nil)
   ;; information for computing prority
   (on-start
    :accessor on-start
    :initform nil)
   (on-end
    :accessor on-end
    :initform nil)
   (termination-conditions
    :documentation "step-level termination conditions"
    :accessor termination-conditions
    :initarg :termination-conditions
    :initform '())
   (restart-conditions
    :documentation "step-level restart conditions"
    :accessor restart-conditions
    :initarg :restart-conditions
    :initform '())
   (resumption-conditions
    :documentation "step-level resumption conditions"
    :accessor resumption-conditions
    :initarg :resumption-conditions
    :initform '())
   (interruption-conditions
    :documentation "step-level suspension conditions"
    :accessor interruption-conditions
    :initarg :interruption-conditions
    :initform '())
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(tag on-start on-end))
   ))


(defmethod print-object ((ob pstep) s)
  (format s "#{~a ~a}" (id ob) (activity ob)))


;;; ---------------------------------------------------------------------------
;;; Procedure Description Language (PDL)
;;; ---------------------------------------------------------------------------

;;; ---- Procedures

;;; A procedure specification has the form: 
;;;
;;;     (PROCEDURE (INDEX <indexform>) clauses*)
;;;
;;; where clauses can be any valud procedure-level clause type.  These include
;;; step clauses, profile clauses and assume clauses.  The proc object type is
;;; also used for "special" procedures which are wrappers for lisp callouts.

;;;----------------------------------------------------------------------------
;;; -- Proceure-level Clauses

;;; INDEX: clause has the form (index <pattern>) where pattern is a list
;;; possibly contianing variables.  Loosely speaking, an index specifies the
;;; main goal of the procedure.  More specifically, the index is pattern that
;;; determines a class of tasks for which the procedure supplies a valid way
;;; to decompose into subtasks.

;;; STEP: clause has the form (step <pattern> [<stepclause>*]) where pattern
;;; is variaxblized list that essentially specified a subgoal.  Steps specify
;;; how to break a task into subtasks.  Each step corresponds to a single 
;;; subtask; its pattern should either name a primitive activity or correspond
;;; to the index-pattern of some procedure in the library.

;;; PROFILE: clause has the form (profile (<resource> <dur> <cont>)+) where
;;; duration is an estimate of the amount of time the specified resource will
;;; be needed to carry out the steps in the procedure.  Cont is a continuity
;;; requirement -- i.e. how long (if at all) other tasks can interrupt control
;;; of a resource before being considered a resource-conflict.

;;; ASSUME: clause has the form (assume <var> <pattern> <time>).  Variable is
;;; set to NIL for <time> whenever a cogevent matching <pattern> occrs; after
;;; <time> it reverts to T, representing the idea that the assumption again
;;; is considered to hold.  Changes to var can occur even when there is no 
;;; active task corresponding to the procedure in which the clause is spec'd.

;;; ! Walter
;;; EXPECTED-DURATION: has the form (expected-duration <time> [(<factor>*)])
;;; <time> is a guess supplied by the user, which will be updated over time
;;; as the system learns.  The list of factors, if present, signals the
;;; factors important to the duration of the task.  Each factor must either
;;; be a number, or eval to a number after variable names have been replaced.  

;;; LOG: clause has one of two forms:
;;; (LOG (attr obj)+) where (attr obj) is a state variable,
;;; which means to log everything during this task's execution
;;; for this sv (up to MOST-POSITIVE-FIXNUM measurements), or
;;; (LOG ((attr obj) :frequency-limit duration :count-limit n :time-limit duration)+)
;;; which allows greater specification over 
;;;  -- the minimum duration between storing values (frequency-limit)
;;;  -- number of measurements to kee (count-limit)
;;;  -- how long to keep measurements (time-limit)
;;; The 'attr' and 'obj' must be bound at the time the task is created, as well
;;; as all parameters.
;;;

;;; The following keywords can follow the symbol PROCEDURE: :sequential,
;;; :seq, :concurrent, :conc, :special, :spcl.  Sequential indicates an
;;; abbreviated form of concurrent (the default) where ordering waitfors
;;; and a final terminate will be added.  A special procedure is a wrapper
;;; for a lisp expression (see below).



(defun make-pdl-object (canonical-pdl)
  (ast-convert canonical-pdl #'initialize-pdl-object
  #'define-pdl-feature #'pdl-postprocess))

  
(defmethod initialize-pdl-object (thang)
  (error 
   (make-condition 'invalid-ast-object-form 
     :format-control "There are no initialize-pdl-object methods for ~a "
     :format-arguments (list thang))))

(defmethod define-pdl-feature (thang attribute value)
  (error
   (make-condition 'invalid-ast-feature-form
     :format-control "~a has no feature methods for ~a ~a"
     :format-arguments (list thang attribute value))))

(defmethod define-pdl-feature ((procedure procedure) 
				 attribute
				 value)
  (error
   (make-condition 'invalid-ast-feature-form
     :format-control "(~a ~a) is an invalid procedure-level clause."
     :format-arguments (list attribute value))))

;;; ---
;;; Post-processing of procedures.
;;; ---

(defmethod tag-name-from-activity ((procedure non-primitive-procedure) (step pstep))
  (if (and (activity step) (first (activity step))
	   (not (consp (first (activity step))))
	   (not (variable-p (first (activity step)))))
    (let* ((act-name (first (activity step)))
	   (pos (position step (filter (lambda (pstep)
					 (eql act-name (first (activity pstep))))
				       (steps procedure)))))
      (let ((cnt (count act-name 
			(steps procedure)
			:key (lambda (pstep)
			       (first (activity pstep))))))
	(cond
	 ((= cnt 0) nil) ;; shouldn't happen, but ...
	 ((= cnt 1) act-name)
	 (t 
	  (if pos
	    (intern (string-upcase (format nil "~a-~a" act-name (1+ pos))))
	    nil)))))
    ;; for some reason, no activity, or a non-list
    nil))

(defmethod new-tag-name ((procedure non-primitive-procedure) (step pstep))
  (let ((tag (tag-name-from-activity procedure step)))
    (if (or (null tag)
	    (find-if (lambda (step)
		       (eql (tag step) tag))
		     (steps procedure)))
      (gentemp "STEP-")
      tag)))

(defmethod force-tag-name ((procedure non-primitive-procedure) (step pstep))
  (or (tag step)
      (let ((tag (new-tag-name procedure step)))
	(setf (tag step) tag)
	tag)))

(defmethod add-termination-step-maybe ((procedure non-primitive-procedure))
  (unless (find-if (lambda (step)
		     (eql (car (activity step))
			  'terminate))
		   (steps procedure))
    (let ((termstep (make-instance 'pstep
		      :activity `(terminate +this-task+ ,(no-outcome-val)
					    >> ,(no-return-val)))))
      (enqueuef (steps procedure) termstep)
      termstep)))
	      

(defmethod add-enablement-condition ((procedure non-primitive-procedure) (prev-step pstep) (step pstep))
  (let ((prev-tag (force-tag-name procedure prev-step)))
    (let ((condition `(state ,(convert-variable (make-variable prev-tag) :bound) = terminated)))
      (let ((current-wfs (car (waitfors step))))
      (unless (find condition current-wfs
		    :test #'equalp)
	(setf (waitfors step)
	  (list (append current-wfs (list condition)))))))))


(defmethod add-sequential-enablement-conditions ((procedure non-primitive-procedure))
  (assert (eql (proctype procedure) 'sequential) nil
    "Attempting to add sequential enablement conditions a ~a procedure"
    (proctype procedure))
  (loop 
      for prev-step in (steps procedure)
      for step in (cdr (steps procedure))
      doing
	(add-enablement-condition procedure prev-step step)))

(defmethod add-rankings ((procedure non-primitive-procedure))
  (assert (eql (proctype procedure) 'ranked) nil
    "Attempting to add ranks ~a procedure"
    (proctype procedure))
  (loop 
      for step in (steps procedure)
      for rank from 1
      doing
	(u-setf (rank step) rank))
  )

(defmethod convert-multiple-waitfors-into-conjunction-maybe ((procedure non-primitive-procedure) (step pstep))
  (let ((wfs (car (waitfors step))))
    (when (cdr wfs)
      (setf (waitfors step)
	`((  (:and ,@wfs) )) )
      )))
  


(defmethod add-step-from-terminate-maybe ((procedure non-primitive-procedure))
  (when (termination-conditions procedure)
    (let ((step (make-instance 'pstep
		  :activity `(terminate +this-task+ ,(no-outcome-val)
					>> ,(no-return-val))
		  :waitfors (list (termination-conditions procedure)))))
      (enqueuef (steps procedure) step)
    step)))

(defmethod add-step-from-step-terminate-maybe ((procedure non-primitive-procedure) (step pstep))
  (when (termination-conditions step)
    (let ((nstep (make-instance 'pstep
		   :activity `(terminate ,(make-variable (force-tag-name procedure step))
					 ,(no-outcome-val)
					 >> ,(no-return-val))
		  :waitfors (list (termination-conditions step)))))
      (enqueuef (steps procedure) nstep)
      nstep)))

(defmethod add-step-from-step-restart-maybe ((procedure non-primitive-procedure) (step pstep))
  (when (restart-conditions step)
    (let ((nstep (make-instance 'pstep
		  :activity `(reset ,(make-variable (force-tag-name procedure step)))
		  :waitfors (list (restart-conditions step)))))
      (enqueuef (steps procedure) nstep)
      nstep)))

(defmethod add-step-from-step-resume-maybe ((procedure non-primitive-procedure) (step pstep))
  (when (resumption-conditions step)
    (let ((nstep (make-instance 'pstep
		  :activity `(resume ,(make-variable (force-tag-name procedure step)) t)
		  :waitfors (list (resumption-conditions step)))))
      (enqueuef (steps procedure) nstep)
      nstep)))

(defmethod add-steps-from-step-suspend-maybe ((procedure non-primitive-procedure) (step pstep))
  (when (interruption-conditions step)
    (let ((suspend-wfs (cdr (assoc 'when (interruption-conditions step))))
	  (until-wfs (cdr (assoc 'until (interruption-conditions step)))))
      (let ((step1 (make-instance 'pstep
		     :activity `(suspend ,(make-variable (force-tag-name procedure step)))
		     :waitfors (list suspend-wfs)))
	    (step2 (make-instance 'pstep
		     :activity `(resume ,(make-variable (force-tag-name procedure step)))
		     :waitfors (list until-wfs))))
	(let ((news (list step1 step2)))
	  (appendf (steps procedure) news)
	  ;; (add-enablement-condition procedure step1 step2)
	  news)))))


  
;; in general, do nothing.
(defmethod pdl-postprocess (thang) thang)

;;; but for procedures, 
;;;  -- add waitfors and ranks if necessary,
;;;  -- termination steps.
;;;  -- ensure steps have tags
;;;  -- convert multiple waitfors into a single one (maybe)
;;;  -- check for bad tag names


(defmethod collected-waitfor-variables ((procedure non-primitive-procedure))
  (remove-duplicates 
   (loop for step in (steps procedure)
       appending (collected-waitfor-variables step))))

;; ( ((:and ...)) )
(defmethod condition-clauses ((step pstep))
  (loop for accessor in
	'(waitfors termination-conditions restart-conditions 
		  resumption-conditions interruption-conditions)
      appending (car (funcall accessor step))))

;; (terminated ?x ?) (reset ?x) (:and (terminated ?x ?) ) ?x ...

(defun task-related-clause-p (x)
  (and (symbolp x)
       (or (member x +possible-task-states+)
	   (eql x 'state))))

;; we 
(defun condition-tag-variables (condition-clause)
  (cond
   ((variable-p condition-clause) (list condition-clause))
   ((and (consp condition-clause)
	 (complex-episode-operator-p (car condition-clause)))
    (loop for clause in (cdr condition-clause)
			appending (condition-tag-variables clause)))
   ((and (consp condition-clause)
	 (task-related-clause-p (car condition-clause)))
    (list (second condition-clause)))
   
   (t (list))))

(defmethod collected-waitfor-variables ((step pstep))
  (loop for condition-clause in (condition-clauses step)
      appending (condition-tag-variables condition-clause)))
	
(defmethod collected-waitfor-tags (x)
  (mapcar #'variable-name 
	  (collected-waitfor-variables x)))


(defmethod collected-step-tags ((procedure procedure))
  (mapcar #'tag (steps procedure)))

(defmethod pdl-postprocess ((procedure non-primitive-procedure))
  (case (proctype procedure)
    (sequential 
     (add-termination-step-maybe procedure)
     (add-sequential-enablement-conditions procedure)
     )
    (ranked
     (add-rankings procedure)
     (let ((termstep (add-termination-step-maybe procedure)))
       (dolist (step (butlast (steps procedure)))
	 (add-enablement-condition procedure step termstep)))))

  ;; add steps to implement terminate
  (add-step-from-terminate-maybe procedure)
  ;; add step-level terminate, restart, suspend,
  (dolist (step (copy-list (steps procedure))) ;; we modify steps in place..
    (add-step-from-step-terminate-maybe procedure step)
    (add-step-from-step-restart-maybe procedure step)
    (add-step-from-step-resume-maybe procedure step)
    (add-steps-from-step-suspend-maybe procedure step))
  ;; make it 'normal'
  (setf (proctype procedure) 'normal)
  ;; force tag names if not present ...
  (dolist (step (steps procedure))
    (setf (tag step) (force-tag-name procedure step))
    (convert-multiple-waitfors-into-conjunction-maybe procedure step))
  ;; check that all tag names are, in fact, names of steps
  (let ((badnames (set-difference (collected-waitfor-tags procedure)
			(collected-step-tags procedure))))
    (when badnames
      (error "In procedure ~a, the following step names are not defined, but are used as condition variables: ~A"
	     (index procedure) (mapcar #'make-variable badnames))))
  procedure)

	
;;; --
;;; PROCEDURE user syntax to canonical syntax to procedure structure.
;;; --

;; procedure type might be keyword in first place

		    
(defun make-canonical-procedure (specs)
  `(procedure ,@(cleanup-procedure-specs1 specs)))

(defun make-procedure (specs)
  (let* ((canon (make-canonical-procedure specs))
	 (proc (make-pdl-object canon)))
    (setf (pdl proc) `(procedure ,@specs))
    (setf (canonical-pdl proc) canon)
    proc))

(defmacro procedure (&rest specs)
  (let ((pvar (gensym)))
  `(let ((,pvar (make-procedure ',specs)))
     (install-procedure-in-library ,pvar)
     ,pvar)))

;;;
;;; Conversion code from user syntax (both current and legacy user syntax) 
;;; to canonicial form.
;;;

(defconstant +old-seq-ranked-keywords+ 
    '(:seq :sequential
      :ranked :soft-seq :soft-sequential))

;;; supporting old syntax ...
(defun old-sequential-or-ranked-syntax-p (specs)
  (and (keywordp (car specs))
       (member (car specs) +old-seq-ranked-keywords+ )
       (consp (cadr specs))
       (eq (car (cadr specs)) 'index)
       (every (lambda (step-like-thing)
		(and (consp step-like-thing)
		     (not (eql (car step-like-thing) 'step))))
	      (cddr specs))))

(defun convert-old-seq-ranked-specs (specs)
  (flet ((convert-old-spec (spec)
	   (case (car spec)
	     ((index step profile duration log) spec)
	     (t `(step ,spec)))))
    (loop for spec in specs collecting
	  (convert-old-spec spec))))

(defun cleanup-procedure-specs1 (specs)
  (cond 
   ((old-sequential-or-ranked-syntax-p specs)
    (cons `(proctype ,(cleanup-proctype (car specs)))
	  (cleanup-procedure-specs2 
	   (convert-old-seq-ranked-specs (cdr specs)))))
   ((keywordp (car specs))
    (cons `(proctype ,(cleanup-proctype (car specs)))
	  (cleanup-procedure-specs2 (cdr specs))))
   ((assoc 'proctype specs) (cleanup-procedure-specs2 specs))
   (t (cons `(proctype normal)
	    (cleanup-procedure-specs2 specs)))))

;;; allow INDEX clause to be missing -- if its the first clause (after proctype).

(defun cleanup-procedure-specs2 (specs)
  (cleanup-procedure-specs3
   (canonicalize-index specs 'procedure)))

(defun canonicalize-index (specs name-of-form) ; list * symbol -> list
  (cond
   ((assoc 'index specs) specs)
   ((not (assoc (car specs)
                (ast-feature-requirements name-of-form)))
    `((index ,(car specs)) ,@(cdr specs)))
   (t specs)))


(defun cleanup-simple-task-clause (spec) 
  (if (and 
       (member (car spec) '(terminate resume restart))
       (pat-match '(? (when ? . ?)) spec))
    spec
    (ecase (car spec)
      ((terminate termiante)
       `(terminate (when ,@(cdr spec))))
      ((resume-when resume)
       `(resume (when ,@(cdr spec))))
      ((restart-when restart)
       `(restart (when ,@(cdr spec)))))))

(defun cleanup-suspend-clause (spec)
  (let* ((until-part (cdr (member 'until spec)))
	 (when-part (butlast (cdr (ldiff spec until-part)))))
    `(suspend (when ,@when-part) (until ,@until-part))))

;;; cleanup other clauses 

(defun cleanup-procedure-specs3 (specs)
  (loop for spec in specs appending
	(cond 
	 ((eql (car spec) 'profile)
	  (cleanup-profile-parameters (cdr spec)))
	 ((eql (car spec) 'log)
	  (cleanup-logging-policies (cdr spec)))
	 ;;((eql (car spec) 'assume)
	 ;; (cleanup-assume-parameters (cdr spec)))
	 ((eql (car spec) 'step)
	  (cleanup-step-parameters (cdr spec)))
	 ((member (car spec) '(terminate termiante resume resume-when restart restart-when))
	  (list (cleanup-simple-task-clause spec)))
	 ((member (car spec) '(suspend-when))
	  (list (cleanup-suspend-clause spec)))
	 (t (list spec)))))

;;;=====================================================================
;;; PROCEDURE AST
;;;=====================================================================


(defvar *procedure-ast-feature-requirements* '()
  "AST feature requirements for Procedures")

(defmacro define-procedure-requirement (requirement)
  `(define-assoc-in-global *procedure-ast-feature-requirements* ',requirement))

(defmethod ast-feature-requirements ((type (eql 'procedure)))
  *procedure-ast-feature-requirements*)

(defmethod initialize-pdl-object ((type (eql 'procedure)))
  (make-instance 'non-primitive-procedure))

;;;---------------------------------------------------------------------
;;; Procedure-level clause: INDEX 
;;;---------------------------------------------------------------------
;;; User canonical form: 
;;;  (INDEX (<name> . <parameters>)) | (<name> . <parameters>)
;;; Syntax canonical form:
;;;  (INDEX (<name> . <parameters>)) | (<name> . <parameters>)
;;;

(define-procedure-requirement
    (index (? . ?) :unique))

(defmethod define-pdl-feature ((procedure procedure) 
				 (attribute (eql 'index)) 
				 (index list))
  (u-setf (index procedure) (car index)))


;;;---------------------------------------------------------------------
;;; Procedure-level clause: PROCTYPE
;;;---------------------------------------------------------------------



;;; note -- although PROCTYPE is a user syntax construct, and used in
;;; the underlying procedure class, the use is actually different. For
;;; user syntax, it signals whether to added additional constraints on
;;; steps (for sequential and ranked). In the PDL post-processing of
;;; procedures, the PROCTYPE is defined as 'normal' (as opposed to
;;; 'special,' which have been replaced by primtive procedures).
;;;

(defun cleanup-proctype (proctype)
   (case proctype
      ((:conc :concurrent normal) 
       'normal)
      ((:seq :sequential seq sequential) 
       'sequential)
      ((:ranked :soft-seq :soft-sequential ranked soft-seq soft-sequential) 
       'ranked)
      ((:spcl :special) 
       (error "SPECIAL procedures are no longer supported. Use
   PRIMITIVE instead."))
      (otherwise (error "Unknown procedure type ~a" proctype))))

(define-procedure-requirement 
    (proctype
     ((?or  normal
	   sequential
	   ranked
	   ))
     :unique))


(defmethod define-pdl-feature ((procedure non-primitive-procedure) 
				 (attribute (eql 'proctype)) 
				 (proctype list))
  (setf (proctype procedure) (car proctype)))

;;;---------------------------------------------------------------------
;;; Procedure-level clause: PROFILE
;;;---------------------------------------------------------------------
;;; user canonical form:
;;; (PROFILE (<resource> <duration> <continuity>)+ )
;;; syntax canonical form: for each resource:
;;; (PROFILE (resource <resource>) (duration <duration>) (continuity <continuity>))
;;; for each resource.
;;; 

(defun cleanup-one-profile (object)
  (cond
   ((symbolp object) `(,object 10 10))
   ((and (consp object)
	 (null (cdr object)))
    `(,(car object) 10 10))
   ((pat-match '(? ? ?) object) object)
   (t (error "Invalid profile clause: ~A; should be (<resource> <duration> <continuity>)"))))

(defun cleanup-profile-parameters (parameters)
  (cond
   ((every 'canonical-ast-p parameters)
    `((profile ,@parameters)))
   (t
    (loop for (resource duration continuity) in
	  (loop for parameter in parameters collecting
		(cleanup-one-profile parameter))
	collecting
	  `(profile (resource ,resource)
		    (duration ,duration)
		    (continuity ,continuity))))))

(define-procedure-requirement
    (profile ((resource ?)
              (duration ?)
              (continuity ?))
             :kleene-star))

(defmethod define-pdl-feature ((procedure procedure)
			       (attribute (eql 'profile))
			       (clauses list))
  (let ((r (second (first clauses)))
	(d (second (second clauses)))
	(c (second (third clauses))))
    (unless (and r d c)
      (error "Invalid profile clause: (profile ~S)" clauses))
    (enqueuef (profile procedure)
	      `(,r ,d ,c))))

;;;---------------------------------------------------------------------
;;; Procedure-level clause: LOG -> LOG-POLICY
;;;---------------------------------------------------------------------
;;;
;;; User canonical form:
;;; (LOG policy+)
;;;  policy: 
;;;   (attr obj)
;;;   ((attr obj) [:frequency-limit dur] [:time-limit dur] [:count-limit n])
;;;
;;; Syntax canonical form: for each policy:
;;; (LOG-POLICY (state-variable (<attr> <obj>))
;;;             [(:frequency-limit <dur>)]
;;;             [(:time-limit <dur>)]
;;;             [(:count-limit <n>)] )
;;; at least one policy must be specfied. No variables in <attr> 
;;; (at policy execution time). STATE-VARIABLE must occur first in list.
;;;



(defun cleanup-one-logging-policy (policy)
  (cond
   ((pat-match '((?is ?attr symbolp)
		 (?is ?obj symbolp) )
	       policy)
    `((state-variable ,policy) (:count-limit ,most-positive-fixnum)))
   ((pat-match '(((?is ?attr symbolp)
		  (?is ?obj symbolp)) . ?rest)
	       policy)
    (let ((features 
	   (or 
	    (loop for attr in (cdr policy) by #'cddr
		for val in  (cddr policy) by #'cddr
		collecting
		  (list attr
			val))
	    `((:count-limit ,most-positive-fixnum)))))
      `((state-variable ,(car policy)) ,@features)))
   (t (error "Invalid LOG specification: ~S" policy))))

(defun cleanup-logging-policies (policies)
  (mapcar (lambda (policy)
	    `(LOG-POLICY ,@(cleanup-one-logging-policy policy)))
	  policies))

(define-procedure-requirement 
    (log-policy ((state-variable (? ?)) 
			    ((?or :time-limit
				  :count-limit 
				  :frequency-limit
				  (?is ? variable-p)) ?) . ?)  ;; i.e., at least one parameter 
		:kleene-star))


(defmethod define-pdl-feature ((procedure non-primitive-procedure)
			       (attribute (eql 'log-policy))
			       (clauses list))
  (let ((sva (assoc 'state-variable clauses)))
    (let ((policy (cons (cadr sva) (flatten (filter (lambda (assoc)
						      (not (eq assoc sva)))
					    clauses)))))
      (enqueuef (logging-policies procedure) policy))))

;;;---------------------------------------------------------------------
;;; Procedure-level clause: DURATION
;;;---------------------------------------------------------------------

(define-procedure-requirement 
    (duration
     ( ?)                      
     :optional))

(defmethod define-pdl-feature ((procedure non-primitive-procedure)
			       (attribute (eql 'duration))
			       time-expression)
  (u-setf (duration procedure) (car time-expression)))

;;;---------------------------------------------------------------------
;;; Procedure-level clause: EXPECTED-DURATION
;;;---------------------------------------------------------------------

(define-procedure-requirement 
    (expected-duration
     ( ?)                      
     :optional))

(defmethod define-pdl-feature ((procedure non-primitive-procedure)
			       (attribute (eql 'expected-duration))
			       time-expression)
  (u-setf (expected-duration procedure) (car time-expression)))

;;;---------------------------------------------------------------------
;;; Procedure-level clause: INTERRUPT-COST
;;;---------------------------------------------------------------------

(define-procedure-requirement 
    (interrupt-cost
     ( ?)                      
     :optional))

(defmethod define-pdl-feature ((procedure non-primitive-procedure)
			       (attribute (eql 'interrupt-cost))
			       cost-expression)
  ;; can't be u-setf  because there is a default.
  (setf (interrupt-cost procedure) (car cost-expression)))


;;;---------------------------------------------------------------------
;;; Procedure-level clause: TERMINATE -> (TERMINATE (WHEN ..))
;;;---------------------------------------------------------------------

(define-procedure-requirement 
    (terminate ((when ? . ?)) :optional))

(defmethod define-pdl-feature ((procedure non-primitive-procedure)
			       (attribute (eql 'terminate))
			       conditions)
  ;; ((when a b)) -> (a b)
  (u-setf (termination-conditions procedure) (cdr (car conditions))))

;;;
;;; If we ever reinstate ASSUME clauses ...
;;;---------------------------------------------------------------------
;;; Procedure-level clause: ASSUME
;;;---------------------------------------------------------------------
;;; user canonical form:
;;; (ASSUME (<variable> <pattern> <duration>)+ )
;;; syntax canonical form: for each resource:
;;; (ASSUME (variable <variable>) (pattern <pattern>) (duration <duration>))
;;; for each assumption.
;;; Internal form (assumes slot):
;;; (<variable> <pattern> <duration>)+

;;;(define-procedure-requirement 
;;;    (assume (assume 
;;;	      (variable (?is ?x variable-p))
;;;	      (pattern ?)
;;;	      (duration ?))
;;;	     :kleene-star))


;;;(defun cleanup-one-assume (object)
;;;  (cond
;;;   ((pat-match '(? ? ?) object) object)
;;;   (t (error "Invalid assume clause: ~A; should be (<variable>
;;;  <pattern> <duration>)" object))))
;;;
;;;(defun cleanup-assume-parameters (parameters)
;;;  (cond
;;;   ((canonical-ast-p `(assume ,@parameters))
;;;    `((assume ,@parameters)))
;;;   (t 
;;;    (loop for (variable pattern duration) in
;;;	  (loop for parameter in parameters collecting
;;;		(cleanup-one-assume parameter))
;;;	collecting
;;;	  `(assume (variable ,variable)
;;;		   (pattern ,pattern)
;;;		   (duration ,duration))))))

;;;(defmethod define-pdl-feature ((procedure procedure)
;;;				 (attribute (eql 'assume))
;;;				 (clauses list))
;;;  (let ((bs (pat-match '((variable ?b)
;;;			 (pattern ?p)
;;;			 (duration ?d))
;;;		       clauses)))
;;;    (unless bs (error "Invalid internal ASSUME clause: ~a" clauses))
;;;    (enqueuef (assumes procedure)
;;;	      `(,(cadr (car clauses)) ;; it's a variable.
;;;		,(value-in '?p bs)
;;;		,(value-in '?d bs)))))


;;;---------------------------------------------------------------------
;;; Procedure-level clause: STEP
;;;---------------------------------------------------------------------

(define-procedure-requirement 
    (step    
     (? . ?)                      
     :kleene-plus))

(defmethod define-pdl-feature ((procedure non-primitive-procedure)
                               (attribute (eql 'step))
                               (clauses list))
  (enqueuef (steps procedure)
	 (make-pdl-object `(step ,@clauses))))
	  

;;; see STEP AST below ...

;;;=====================================================================
;;; STEP AST
;;;=====================================================================
;;; --------------------------------------------------------------------------
;;; ---- Steps

;;; Each PDL step has the form:
;;; 
;;; (STEP <step-tag> <pattern> [stepclause]*)
;;;
;;; where step-tag is any sumbol (a variable based on the tag is generated 
;;; allowing steps to reference one another).  Pattern corresponds to a 
;;; procedure index or primitive action type.  

;;; --- Step-level clauses

;;; Steps can reference other procedures in the procedure library, specify
;;; primitive actions (e.g. terminate, start-activity), or callout to Lisp
;;; directly.  When a procedure is retrieved, any variables listed in its
;;; activity-description are bound and used to specify variables inside steps.
;;; These steps are then instantiated as tasks --- i.e. task structures are
;;; created for each step and placed on the agent's agenda; conditions
;;; associated with individual steps are placed on the agent monitors-list,
;;; etc..

;;; Valid step-level clauses include:
;;; WAITFOR:   (waitfor <pattern>+)
;;; PRIORITY:  (priority <expr> {integer|:urgency <val> :importance <val>})
;;; INTERRUPT-COST:   (interrupt-cost <val>)
;;; SELECT: (select <var> <evaluable-expression>)
;;; REPEATING (see docs)
;;; RESPONDING (see docs)
;;; FORALL: (forall <variable> <listval>)
;;; RANK: (rank <expr>)

;;; By default, step-specifed activities are assumed to run concurrently with
;;; all other activities.  Waitfors are most often used to sequence steps of a
;;; procedure.  In particular, if a task resulting from the instantiation of step
;;; s2 should be suspended until task s1 terminates, then s2 will be notated
;;; with a clause such as (waitfor (terminated ?s1)).  This can be abbreviated to
;;; just (waitfor ?s1).

;;; If there is a test associated with :recurrent, the parameter is evaluated;
;;; repetition only occurs if it is non-nil.  If the :recovery parameter is
;;; present in a period clause, repeats (subsequent instance of a task) are
;;; reduced in priority in proportion to how much of the specified recovery time
;;; has passed.  This does not inhibit a task, as is needed to prevent premature
;;; action.  If the :enabled keyword is present, repeated tasks are created with
;;; all waitfor conditions satisfied.  The :reftime parameter determined when 
;;; a new task is created (default = when the old instance is terminated)


(defvar *step-ast-feature-requirements* '()
  "AST feature requirements for Steps")

(defmacro define-step-requirement (requirement)
  `(define-assoc-in-global *step-ast-feature-requirements* ',requirement))

(defmethod ast-feature-requirements ((type (eql 'step)))
  *step-ast-feature-requirements*)


;;
;; Cleaning up user forms to AST forms.
;;

(defun cleanup-step-parameters (parameters)
  `((step ,@(cleanup-step-parameters1 parameters))))

(defun cleanup-step-parameters1 (parameters)
  (if (symbolp (car parameters)) ;; i.e., a tag
    (cons `(tag ,(car parameters)) 
	  (cleanup-step-parameters2 (cdr parameters)))
    (cleanup-step-parameters2 parameters)))

;; add ACTIVITY attribute, maybe -- and call any special cleanups on 
;; the activity.
(defun cleanup-step-parameters2 (parameters)
  (let ((activity-param (car parameters)))
    (if (and (consp activity-param)
	     (eql (car activity-param) 'activity))
      (cons activity-param ;; assume it and rval are ok.
	    (cleanup-step-parameters3 (cdr parameters)))
      (append (cleanup-activity activity-param)
	      (cleanup-step-parameters3 (cdr parameters))))))

(defmethod cleanup-activity ((activity list))
  (cleanup-activity-of-type (car activity) activity))

;;; (do-it ?x => ?y)
;;; generally, leave things alone, but check for return value
(defmethod cleanup-activity-of-type ((type T) activity)
  (let ((rval (second (member '=> activity))))
    (if rval
      `((activity ,(butlast activity 2))
	(return-value ,rval))
      `((activity ,activity)))))

;;; termination successful unless otherwise spec'd
(defun no-outcome-val () 'success) 

(defun no-return-val () nil)  ;;default return val is nil


;; (terminate [var] [outcome] [[>>|=>] result] )
;; should this also have a return variable?
(defmethod cleanup-activity-of-type ((type (eql 'terminate)) activity)
  (let ((rval (second (member '>> activity))))
    (when rval (setq activity (butlast activity 2)))
    `((activity (terminate 
		 ,(or (second activity) '+this-task+)
		 ,(or (third activity) (no-outcome-val))
		 >>
		 ,(or rval
		      (no-return-val)))))))

;;; the type
(defmethod cleanup-activity-of-type ((type (eql 'termiante)) activity)
  (cleanup-activity-of-type 'terminate activity))

;;; note -- other forms can be added above if necessary.

;;; (blah blah => rval blah blah) to
;;; (blah blah (returnval rval) blah blah) 
(defun cleanup-step-parameters3 (parameters) 
  (loop for spec in parameters appending
	(cond 
	 ((eql (car spec) 'waitfor)
	  (cleanup-waitfor-parameters (cdr spec)))
	 ((eql (car spec) 'priority)
	  (cleanup-priority-parameters (cdr spec)))
	 ((eql (car spec) 'period)
	  (cleanup-period-parameters (cdr spec)))
         ((eql (car spec) 'repeating)
	  (cleanup-repeating-parameters (cdr spec)))
         ((eql (car spec) 'responding)
	  (cleanup-responding-parameters (cdr spec)))
	 ((eql (car spec) 'select)
	  (cleanup-select-parameters (cdr spec)))
	 ((eql (car spec) 'forall)
	  (cleanup-forall-parameters (cdr spec)))
	 ((member (car spec) '(terminate termiante resume resume-when restart restart-when))
	  (list (cleanup-simple-task-clause spec)))
	 ((member (car spec) '(suspend-when))
	  (list (cleanup-suspend-clause spec)))
	 (t (list spec)))))

;;;(defmethod ast-feature-requirements ((type (eql 'step)))
;;;  '((activity   (activity (?is ?name listp)) :unique)
;;;    (tag      (tag (?is ?name symbolp)) :optional)
;;;    ))

(defmethod initialize-pdl-object ((type (eql 'step)))
  (make-instance 'pstep))

;;;---------------------------------------------------------------------
;;; Step-level clause: TAG
;;;---------------------------------------------------------------------
;;; User syntax: just a symbol (optional)
;;; Canonical syntax: (tag <name>) 

(define-step-requirement (tag      ((?is ?name symbolp)) :optional))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'tag)) 
			       (tag list))
  (setf (tag step) (car tag)))

;;;---------------------------------------------------------------------
;;; Step-level clause: ACTIVITY
;;;---------------------------------------------------------------------
;;; User syntax: <form> to execute (required)
;;; Canonical syntax: (activity <form>)
(define-step-requirement (activity   ((?is ?name listp)) :unique))


(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'activity)) 
			       (activity list))
  (setf (activity step) (car activity)))

;;;---------------------------------------------------------------------
;;; Step-level clause: RETURN-VALUE
;;;---------------------------------------------------------------------
;;; User syntax: (do-x => var) 
;;; Canonical syntax: (return-value <form>)
(define-step-requirement (return-value  ( ? ) :unique))


(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'return-value)) 
			       (returnvar list))
  (setf (returnvar step) (car returnvar)))

;;;---------------------------------------------------------------------
;;; Step-level clause: WAITFOR
;;;---------------------------------------------------------------------
;;; User syntax:
;;; (waitfor <condition>+)
;;; condition: 
;;;   ?var | <monitor-form>
;;;
;;; Canonical syntax:
;;; (waitfor <monitor-form>+)
;;; 
;;; Internal: waitfors: <monitor-form>+
;;; 
;;; The syntax for monitor forms can be found in the various
;;; MONITOR-x.lisp files. 

;;; I have a fondness for the silly code here that protects against some
;;; typos ... so I left it in.
;;; 
;;;(defun cleanup-one-monitor-condition (spec)
;;;  (cond
;;;   ((and (consp spec)
;;;	 (complex-episode-operator-p (car spec))) ;; defined in monitors-complex-episode.lisp
;;;    `(,(car spec) ,@(mapcar 'cleanup-one-monitor-condition (cdr spec))))
;;;   ((and (consp spec) (equal 2 (length spec)) (equal 'terminate (first spec)))
;;;    `(terminated ,(second spec) ?))
;;;   ((and (consp spec) (equal 2 (length spec)) (equal 'termiante (first spec))) ;typo
;;;    `(terminated ,(second spec) ?))
;;;   ((variable-p spec)
;;;    `(terminated ,(convert-variable spec :bound) ?))
;;;   ((measurement-form-p spec) ;; defined in monitors.lisp
;;;    `(:measurement ,spec))
;;;   (t spec)))

(defun cleanup-one-monitor-condition (spec)
  (cond
   ((and (consp spec)
	 (complex-episode-operator-p (car spec))) ;; defined in monitors-complex-episode.lisp
    `(,(car spec) ,@(mapcar 'cleanup-one-monitor-condition (cdr spec))))
   ((and (consp spec) (equal 2 (length spec)) (equal 'terminate (first spec)))
    `(terminated ,(second spec) ?))
   ((and (consp spec) (equal 2 (length spec)) (equal 'termiante (first spec))) ;typo
    `(terminated ,(second spec) ?))
   ((variable-p spec)
    `(state ,(convert-variable spec :bound) = terminated))
   ((valid-measurement-form-p spec) ;; defined in monitors.lisp
    `(:measurement ,spec))
   (t spec)))

(defun cleanup-waitfor-parameters (specs)
  `((waitfor ,@ (mapcar 'cleanup-one-monitor-condition specs))))

(define-step-requirement (waitfor (? . ?) :optional))

;; (waitfor w1 w2 w3 ... wn)
(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'waitfor)) 
			       (conditions list))
  (u-setf  (waitfors step) (list conditions)))

;;;---------------------------------------------------------------------
;;; Step-level clause: INTERRUPT-COST
;;;---------------------------------------------------------------------

(define-step-requirement 
    (interrupt-cost
     (?)                      
     :optional))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'interrupt-cost))
			       cost-expression)
  ;; can't be u-setf  because there is a default.
  (setf (interrupt-cost step) (car cost-expression)))

;;;---------------------------------------------------------------------
;;; Step-level clause: PRIORITY
;;;---------------------------------------------------------------------

(define-step-requirement 
    (priority
     (? . ?)                      
     :optional))

;;; when I understand priorities better, fix here.
(defun cleanup-priority-parameters (specs)
  `((priority ,specs)))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'priority))
			       priority)
  (setf (criteria step) (cons (car priority) (criteria step)))) 

;;;---------------------------------------------------------------------
;;; Step-level clause: RANK
;;;---------------------------------------------------------------------

(define-step-requirement 
    (rank
     (?)                      
     :optional))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'rank))
			       rank-expression)
  (u-setf (rank step) (car rank-expression)))

;;;---------------------------------------------------------------------
;;; Step-level clause: PERIOD
;;;---------------------------------------------------------------------

(define-step-requirement 
    (period
     (? . ?)                      
     :optional))

(defun cleanup-period-parameters (specs)
  `((period ,specs)))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'period))
			       period)
  (u-setf (period step) (car period)))

;;;---------------------------------------------------------------------
;;; Step-level clause: REPEATING
;;;---------------------------------------------------------------------

(define-step-requirement 
    (repeating
     (? . ?)                      
     :optional))

(defun cleanup-repeating-parameters (specs)
  `((repeating ,specs)))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'repeating))
			       repeating)
  (u-setf (repitition step) (or (car repeating) t)))


;;;---------------------------------------------------------------------
;;; Step-level clause: RESPONDING
;;;---------------------------------------------------------------------

(define-step-requirement 
    (responding
     (? . ?)                      
     :optional))

(defun cleanup-responding-parameters (specs)
  `((responding ,specs)))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'responding))
			       responding)
  (u-setf (response-policy step) (or (car responding) t)))

;;;---------------------------------------------------------------------
;;; Step-level clause: SELECT
;;;---------------------------------------------------------------------

(define-step-requirement 
    (select
     (? . ?)                      
     :optional))

(defun cleanup-select-parameters (specs)
  `((select ,specs)))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'select))
			       select)
  (u-setf (select step) (car select)))

;;;---------------------------------------------------------------------
;;; Step-level clause: FORALL
;;;---------------------------------------------------------------------

(define-step-requirement 
    (forall
     (? . ?)                      
     :optional))

(defun cleanup-forall-parameters (specs)
  `((forall ,specs)))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'forall))
			       forall)
  (u-setf (forall step) (car forall)))

;;;---------------------------------------------------------------------
;;; Step-level clause: ON-START
;;;---------------------------------------------------------------------

(define-step-requirement 
    (on-start
     (?  . ?)                      
     :kleene-star))


(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'on-start))
			       on-start)
  (appendf (on-start step) on-start))
	

;;;---------------------------------------------------------------------
;;; Step-level clause: ON-END
;;;---------------------------------------------------------------------

(define-step-requirement 
    (on-end
     (? . ?)                      
     :kleene-star))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'on-end))
			       on-end)
  (appendf (on-end step) on-end))

;;;---------------------------------------------------------------------
;;; Step-level clause: TERMINATE -> (TERMINATE (WHEN ..))
;;;---------------------------------------------------------------------

(define-step-requirement 
    (terminate ((when ? . ?)) :optional))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'terminate))
			       conditions)
  ;; ((when a b)) -> (a b)
  (u-setf (termination-conditions step) (cdr (car conditions))))

;;;---------------------------------------------------------------------
;;; Step-level clause: RESTART-WHEN -> (RESTART (WHEN ...))
;;;---------------------------------------------------------------------

(define-step-requirement 
    (restart ((when ? . ?)) :optional))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'restart))
			       conditions)
  (u-setf (restart-conditions step) (cdr (car conditions))))

;;;---------------------------------------------------------------------
;;; Step-level clause: RESUME-WHEN -> (RESUME (WHEN ...))
;;;---------------------------------------------------------------------

(define-step-requirement 
    (resume ((when ? . ?)) :optional))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'resume))
			       conditions)
  (u-setf (resumption-conditions step) (cdr (car conditions))))

;;;---------------------------------------------------------------------
;;; Step-level clause: SUSPEND-WHEN -> (SUSPEND (WHEN .) (UNTIL .))
;;;---------------------------------------------------------------------
;;((when ?)     (until ?))
;;((when (o c)) (until (d e)))
(define-step-requirement 
    (suspend ((when ? . ?) (until ? . ?)) :optional))

(defmethod define-pdl-feature ((step pstep)
			       (attribute (eql 'suspend))
			       conditions)
  (u-setf (interruption-conditions step) conditions))


;;;=====================================================================
;;; Some useful functions for describing procedures, steps
;;;=====================================================================


(defun dp-ntabs (n) (coerce (make-array n :initial-element #\Tab) 'string))

(defun describe-procedures (&optional (stream *standard-output*) (sort-p t))
  (dolist (proc (if sort-p (sort (copy-list *proclib*)
				 #'string<
				 :key #'(lambda (x) (format nil "~a" (index x))))
		    *proclib*))
    (describe-deeply proc stream sort-p)))
  
(defmethod describe-deeply ((proc non-primitive-procedure)
                            &optional (stream *standard-output*) (sort-steps-p nil))
  (format stream "PROCEDURE ~s~%" (index proc))
  (format stream "~a:proctype ~s~%" (dp-ntabs 1) (proctype proc))
  (format stream "~a:profile ~s~%" (dp-ntabs 1) (profile proc))
  (format stream "~a:duration ~s~%" (dp-ntabs 1) (duration proc))
  (format stream "~a:bundle ~s~%" (dp-ntabs 1) (bundle proc))
  (format stream "~a:log ~s~%" (dp-ntabs 1) (logging-policies proc))
  (format stream "~a:interrupt-cost ~s~%" (dp-ntabs 1) (interrupt-cost proc))
  (format stream "~a:steps~%" (dp-ntabs 1))
  (dolist (step (if sort-steps-p
		  (sort (copy-list (steps proc))
		      #'string<
		      :key #'(lambda (x) (format nil "~a" (if (typep x 'pstep) (activity x) x))))
		  (steps proc)))
    (describe-step step stream 2))
  (terpri stream))

(defun describe-step (step stream n)
  (if (typep step 'pstep)
    (progn
      (format stream "~aSTEP ~s~%" (dp-ntabs n) (activity step))
      (format stream "~a:tag ~s~%" (dp-ntabs (1+ n)) (tag step))
      (format stream "~a:returnvar ~s~%" (dp-ntabs (1+ n)) (returnvar step))
      (format stream "~a:select ~s~%" (dp-ntabs (1+ n)) (select step))
      (format stream "~a:forall ~s~%" (dp-ntabs (1+ n)) (forall step))  
      (format stream "~a:period ~s~%" (dp-ntabs (1+ n)) (period step))  
      (format stream "~a:repitition ~s~%" (dp-ntabs (1+ n)) (repitition step))  
      (format stream "~a:response-policy ~s~%" (dp-ntabs (1+ n)) (response-policy step))  
      (format stream "~a:interrupt-cost ~s~%" (dp-ntabs (1+ n)) (interrupt-cost step))  
      (format stream "~a:rank ~s~%" (dp-ntabs (1+ n)) (rank step))  
      (format stream "~a:criteria ~s~%" (dp-ntabs (1+ n)) (criteria step))  
      (format stream "~a:on-start ~s~%" (dp-ntabs (1+ n)) (on-start step))  
      (format stream "~a:on-end ~s~%" (dp-ntabs (1+ n)) (on-end step))  
      (format stream "~a:waitfors ~s~%" (dp-ntabs (1+ n)) (waitfors step)))
    (format stream "~aSTEP ~s~%" (dp-ntabs n) step))
  )


