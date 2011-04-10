;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors-complex-episode.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors-complex-episode.lisp,v 1.13 2006/01/15 03:43:00 dalal Exp $
;;; Created:        November. 2004

;;; --- 
;;; complex episode monitors.
;;; ----

(in-package :cl-user)

(defclass complex-episode-monitor (monitor)
  ((general-constraints 
    :initarg :general-constraints 
    :initform (list) 
    :accessor monitor-general-constraints)
   (submonitors
    :initarg :submonitor
    :initform (list)
    :accessor submonitors)))


(defclass allen/and-monitor (complex-episode-monitor)
  ((interval-comparator
    :initarg :interval-comparator
    :initform nil
    :accessor interval-comparator
    :documentation "how to compare two intervals")
   ))

(defclass allen-monitor (allen/and-monitor) ())



(defmacro define-allen-monitor-class (name comparator keyword)
  `(progn
     (defclass ,name (allen-monitor)
       ()
       (:default-initargs :interval-comparator ',comparator))
     (defmethod c-episode-monitor-type ((type (eql ',keyword)))
       ',name)
     (defmethod create-monitor-from-pattern ((type (eql ',keyword)) expr parameters pattern)
       (declare (ignore expr))
       (create-complex-monitor-from-pattern type parameters pattern))))

(defvar *complex-episode-operators*
    '(:and :or :not :in-order
      :contains 
      :finishes 
      :starts 
      :before 
      :meets 
      :overlaps 
      :cotemporal 
      :during 
      :finished-by 
      :started-by 
      :after 
      :met-by 
      :overlapped-by))

(defun complex-episode-operator-p (x)
  (and (member x *complex-episode-operators*)
       t))

;;;
;;; -- Create the Allen patterns
;;; 

(define-allen-monitor-class contains-monitor contains-p :contains)
(define-allen-monitor-class finishes-monitor finishes-p :finishes)
(define-allen-monitor-class starts-monitor starts-p :starts)
(define-allen-monitor-class before-monitor before-p :before)
(define-allen-monitor-class meets-monitor meets-p :meets)
(define-allen-monitor-class overlaps-monitor overlaps-p :overlaps)
(define-allen-monitor-class cotemporal-monitor cotemporal-p :cotemporal)
(define-allen-monitor-class during-monitor during-p :during)
(define-allen-monitor-class finished-by-monitor finished-by-p :finished-by)
(define-allen-monitor-class started-by-monitor started-by-p :started-by)
(define-allen-monitor-class after-monitor after-p :after)
(define-allen-monitor-class met-by-monitor met-by-p :met-by)
(define-allen-monitor-class overlapped-by-monitor overlapped-by-p :overlapped-by)
(define-allen-monitor-class in-order-monitor before-p :in-order)
  
(defmethod create-complex-monitor-from-pattern (type parameters pattern)
  (let ((mon (make-instance (c-episode-monitor-type type)))
	(patpart (if (symbolp (cadr pattern))
		   (cddr pattern)
		   (cdr pattern))))
    (let ((submon-patterns (loop for x in patpart 
			       until (eql x (car parameters))
			       collect x)))
      (let ((submonitors (collect-monitors type submon-patterns pattern mon)))
	(setf (relevant-types mon)
	  (remove-duplicates 
	   (flatten (mapcar #'relevant-types submonitors))))
	(setf (submonitors mon) submonitors)
	mon))))

(defmethod compile-monitor-post-process ((monitor complex-episode-monitor))
  (setf (expr monitor) (pattern monitor))
  (unless (submonitors monitor)
    (error "No submonitors specified: ~S" (pattern monitor))))



(defmethod add-monitor-constraint ((type (eql :constraints)) (monitor complex-episode-monitor) value)
  (setf (monitor-general-constraints monitor)
    (append (monitor-general-constraints monitor)
	    (list value))))

(defun delay-pattern-p (pattern)
  (and (consp pattern) (eq (car pattern) :delay)))


(defun check-no-delays (mons-delays pattern)
  (when (find-if #'delay-pattern-p 
		 mons-delays)
    (error "DELAY forms are only allowed in :AND and :OR patterns, or by themselves: ~s"
	   pattern)))


(defun just-collect-monitors (submon-pats pat parent)
  (check-no-delays submon-pats pat)
  (let ((mons (mapcar #'compile-monitor submon-pats)))
    (dolist (mon mons)
      (setf (parent-monitor mon) parent))
    mons))

(defun collect-monitors-keeping-delays (submon-pats pat parent)
  (declare (ignore pat))
  (let ((mons (mapcar #'compile-monitor submon-pats)))
    (dolist (mon mons)
      (setf (parent-monitor mon) parent))
    mons))
      
(defun collect-monitors (type submon-pats pat parent)
  (cond
   ((member type '(:and :or))
    (collect-monitors-keeping-delays submon-pats pat parent))
   (t
    (just-collect-monitors submon-pats pat parent))))

;;;
;;; Signalling ...
;;;

(defmethod general-constraints-met-p ((monitor complex-episode-monitor) (result monitor-result))
  (let ((bindings (binding-set result)))
    (let ((globals (if (parent (task monitor))
		     (vars (parent (task monitor)))
		     (vars (task monitor)))))
      (let ((constraints (replace-vars (monitor-general-constraints monitor) bindings globals)))
	(if (contains-variable-p constraints)
	  nil
      (every (lambda (constraint)
	       (eval (convert-duration-specs constraint)))
	     constraints))))))

(defmethod interval-union ((i_1 interval) (i_2 interval))
  (make-interval (min (interval-start i_1)
		      (interval-start i_2))
		 (max (interval-end i_1)
		      (interval-end i_2))))


(defmethod signal-monitor-accum ((monitor allen/and-monitor) (task task) event submonitors pipe)
  (flet ((bs2self (result)
	   ;; (pvs! "extending" (monitor result) (binding-set result) (interval result))
	   (extend-bindings (make-variable (tag (monitor result)))
			      result
			      (binding-set result))))
    (if (null submonitors)
      (map-pipe (lambda (result)
		  (setf (binding-set result)  (bs2self result))
		  result)
		pipe)
      (let ((next (car submonitors))
	    (remaining (cdr submonitors)))
      ;;; ok. we have a pipe of results. we need to get a new pipe:
      ;;;  for each result in pipe, appending the results of signaling NEXT
      ;;;  with the added bindings from result.
	(let ((new-pipe
	       (mappend-pipe-filtering
		(lambda (prev-result)
		  ;; (pvs! prev-result)
		  (let ((bindings (bs2self prev-result))
			(i_1 (interval prev-result))
			(partial_1 (partial-interval prev-result)))
		  (map-pipe-filtering
		   (lambda (next-result)
		     ;; (pvs! next-result)
		     (let ((i_2 (interval next-result)))
		       (if (or (null (interval-comparator monitor))
			       (funcall (interval-comparator monitor)
					i_1 i_2))
			 (make-monitor-result
			  (extend-bindings (make-variable (tag (monitor prev-result)))
					   prev-result
					   (binding-set-union bindings (binding-set next-result)))
			  i_2
			  next
			  (interval-union partial_1 i_2))
			 nil)))
		   (signal-monitor next task event bindings))))
	      pipe)))
	(if (empty-pipe-p new-pipe) ;; no results from next
	  ;; set to partially satisfied, and return empty pipe
	  (progn
	    (set-monitor-status monitor :partially-satisfied nil nil)
	    empty-pipe
	    )
	  ;;; otherwise, recurse
	  (signal-monitor-accum monitor task event remaining new-pipe)))))))

(defmethod signal-monitor-iter ((monitor allen/and-monitor) (task task) event added-bindings)
  ;;; get the pipe of results from signaling the first submonitor.
  (let ((submonitors (submonitors monitor)))
    ;; for debugging, make submonitor status unknown.
    (dolist (subm submonitors)
      (set-monitor-status subm :unknown nil nil))
    (let ((pipe (signal-monitor (car submonitors) task event added-bindings)))
      ;;(setq pipe (enumerate-pipe pipe))
      ;;(pvs! ";;; first pipe:" pipe :newline ";;; " monitor)
      (if (empty-pipe-p pipe)
	(progn ;; quit now
	  (set-monitor-status monitor :unsatisfied nil nil)
	  empty-pipe)
	(signal-monitor-accum monitor task event (cdr submonitors) 
			      (map-pipe (lambda (result)
					  ;; (pvs! result)
					  ;; hook up to monitor
					  (setf (monitor result) 
					    (car submonitors))
					  ;; make partial interval the same as interval
					  (setf (partial-interval result)
					    (interval result))
					  result)
				       
					pipe))))))

  
;;;(defmethod signal-monitor-iter ((monitor allen/and-monitor) (task task) event added-bindings)
;;;  ;;; get the pipe of results from signaling the first submonitor.
;;;  (let ((submonitors (submonitors monitor)))
;;;    ;; for debugging, make submonitor status unknown.
;;;    (dolist (subm submonitors)
;;;      (set-monitor-status subm :unknown nil nil))
;;;    (let ((pipe (signal-monitor (car submonitors) task event added-bindings)))
;;;      (when (empty-pipe-p pipe)
;;;	(set-monitor-status monitor :unsatisfied nil nil)
;;;	(return-from signal-monitor-iter empty-pipe))
;;;    ;;; collect data we need to pass along ...
;;;    ;;; each result's 1. binding-set (which will expand as we cross submonitors)
;;;    ;;;               2. the result itself (for debugging)
;;;    ;;;               3. The initial interval
;;;    ;;;               4. the 'whole event' interval, which gets expanded across
;;;    ;;;               5. a list of monitors which fire (debugging, only now, I think)
;;;      (setq pipe
;;;	(map-pipe (lambda (result)
;;;		    (list (binding-set result)
;;;			  result
;;;			  (interval result)
;;;			  (interval result)
;;;			  (list (monitor result))))
;;;		  pipe))
;;;      ;; well, at least it's partially satisfied ...
;;;      (set-monitor-status  monitor 
;;;			   :partially-satisfied 
;;;			   (car (pipe-head pipe))
;;;			   (third (pipe-head pipe)))
;;;      ;; (setq pipe (enumerate-pipe pipe))
;;;      ;; (pvs! "After first" pipe)
;;;    ;;; ok, let signal the other submontiors ...
;;;      (dolist (subm (cdr submonitors))
;;;	;; (pvs! subm)
;;;	(setq pipe 
;;;	  (mappend-pipe-filtering 
;;;	   (lambda (pipe-result)
;;;	     (let ((bindings (car pipe-result))
;;;		 (prev-interval (interval (cadr pipe-result)))
;;;		 (prev-monitor (car (fifth pipe-result))))
;;;	     (when prev-monitor
;;;	       ;; (pvs! "Setting recognition interval" :newline prev-monitor prev-interval)
;;;	       (setf (recognition-interval prev-monitor)
;;;		 prev-interval))
;;;	     ;; (pvs! :newline "Comparing" prev-monitor :newline subm)
;;;	     (map-pipe-filtering 
;;;	      (lambda (signal-result)
;;;		;; (pvs! event :newline signal-result)
;;;;;;		(pvs! "Comparing" prev-interval (interval signal-result)
;;;;;;		      :newline (monitor signal-result) 
;;;;;;		      :newline (fifth pipe-result)
;;;;;;		      :newline "Last " (car (fifth pipe-result)))
;;;		(if (or (null (interval-comparator monitor))
;;;			(funcall (interval-comparator monitor)
;;;				 prev-interval
;;;				 (interval signal-result)))
;;;		  (let ((result-list
;;;			 (list (binding-set-union 
;;;				bindings (binding-set signal-result))
;;;			       signal-result
;;;			       (third pipe-result)
;;;			       (interval signal-result)
;;;			       (cons (monitor signal-result) (fifth pipe-result)))))
;;;		    ;; (pvs! result-list)
;;;		    (set-monitor-status monitor
;;;					:partially-satisfied
;;;					(car result-list)
;;;					(make-interval (min (interval-start (interval (monitor-status monitor)))
;;;							    (interval-start (interval signal-result)))
;;;						       (max (interval-end (interval (monitor-status monitor)))
;;;							    (interval-end (interval signal-result)))))
;;;								       
;;;		    result-list)
;;;		  (progn
;;;		    ;; (pvs! "Submonitor failed")
;;;		    nil))) ;; i.e. fail
;;;	      (signal-monitor subm task event bindings))))
;;;	   pipe))
;;;	;; (pvs! "About to enumerate pipe...")
;;;	(setq pipe (enumerate-pipe pipe))
;;;	;; if we ever get an empty pipe, quit immediately.
;;;	(when (empty-pipe-p pipe)
;;;	  (return-from signal-monitor-iter empty-pipe)))
;;;      ;; ok, now we have a flat list of 'pipe results' which we can turn into
;;;      ;; results from the monitor.
;;;      (let ((newpipe 
;;;	     (map-pipe (lambda (pipe-result)
;;;			 (let ((start-interval (third pipe-result))
;;;			       (end-interval (fourth pipe-result))
;;;			       (bindings (first pipe-result)))
;;;			   (make-monitor-result 
;;;			    bindings ;; that we collected across submonitors 
;;;			    (make-interval  ;; created from the combination of start and end
;;;			     (min (interval-start start-interval)
;;;				  (interval-start end-interval))
;;;			     (max (interval-end start-interval)
;;;				  (interval-end end-interval)))
;;;			    monitor))) ;; and the monitor itself, naturally
;;;		       pipe)))
;;;	(set-monitor-status monitor :satisfied 
;;;			    (binding-set (pipe-head newpipe))
;;;			    (interval (pipe-head newpipe)))
;;;	newpipe))))


(defmethod signal-monitor ((monitor allen/and-monitor) (task task) event &optional (added-bindings no-bindings))
  (signal-recording-pipe-result
   monitor
   (unless (eq event :creation)
     (when (not (relevant-event-type-p monitor (first (content event))))
       ;; (let ((type (first (content event)))) (pvs! ";;; not relevant: " type))
       (return-from signal-monitor empty-pipe)))
   (let ((pipe (signal-monitor-iter monitor task event added-bindings)))
     ;; (pvs! event (id monitor) (enumerate-pipe pipe))
     (filter-pipe (lambda (result)
		    (if (general-constraints-met-p monitor result)
		      (progn
			;; set the interval to the accumulated (no longer) partial result
			(setf (interval result) (partial-interval result))
			result)
		      nil))
		  pipe))))



;;;
;;; Oh, aren't we clever -- and monitors are the same as Allen monitors, but without
;;; constraints on their ordering...
;;;

(defclass and-monitor (allen/and-monitor) () )

(defmethod c-episode-monitor-type ((type (eql :and)))
  'and-monitor)

(defmethod create-monitor-from-pattern ((type (eql :and)) expr parameters pattern)
  (declare (ignore expr))
  (create-complex-monitor-from-pattern type parameters pattern))

;;;
;;; OR monitors just return the appended pipes of their submonitors.
;;;

(defclass or-monitor (complex-episode-monitor) () )

(defmethod c-episode-monitor-type ((type (eql :or)))
  'or-monitor)

(defmethod create-monitor-from-pattern ((type (eql :or)) expr parameters pattern)
  (declare (ignore expr))
  (create-complex-monitor-from-pattern type parameters pattern))

(defmethod signal-monitor ((monitor or-monitor) (task task) event &optional (added-bindings no-bindings))
  (dolist (subm (submonitors monitor))
    (set-monitor-status subm :unknown nil nil))
  (filter-pipe 
   (lambda (result)
     (general-constraints-met-p monitor result))
   (signal-recording-pipe-result monitor
				 (signal-monitor/1 monitor task event added-bindings))))

(defmethod signal-monitor/1 ((monitor or-monitor) (task task) event &optional (added-bindings no-bindings))
  (unless (eq event :creation)
    (when (not (relevant-event-type-p monitor (first (content event))))
      (return-from signal-monitor/1 empty-pipe)))
  (mappend-pipe-filtering
   (lambda (submonitor)
     (signal-monitor submonitor task event added-bindings))
   (submonitors monitor)))






;;;(defmethod constrain-by-previous ((monitor monitor) (prev monitor) (next monitor))
;;;  ;; (warn "Cannot constrain ~a by ~a" prev next)
;;;  )
;;;
;;;;;;(define-allen-monitor-class contains-monitor contains-p :contains)
;;;
;;;(defmethod constrain-by-previous ((monitor contains-monitor) (prev timepoint-monitor) (next monitor))
;;;  (error "~a monitors for a point; it cannot contain ~a" prev next))
;;;
;;;(defmethod constrain-by-previous ((monitor contains-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(> (start-of ,prev)))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(< (end-of ,prev))))
;;;  
;;;
;;;(defmethod constrain-by-previous ((monitor contains-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (> (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (< (end-of ,prev)))))
;;;
;;;
;;;;;;(define-allen-monitor-class finishes-monitor finishes-p :finishes)
;;;
;;;(defmethod constrain-by-previous ((monitor finishes-monitor) (prev monitor) (next timepoint-monitor))
;;;  (error "~a monitors for a point; ~a cannot finish it" next prev))
;;;
;;;
;;;(defmethod constrain-by-previous ((monitor finishes-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (< (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (= (end-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class starts-monitor starts-p :starts)
;;;
;;;(defmethod constrain-by-previous ((monitor starts-monitor) (prev monitor) (next timepoint-monitor))
;;;  (error "~a monitors for a point; ~a cannot start it" next prev))
;;;
;;;(defmethod constrain-by-previous ((monitor starts-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (= (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (> (end-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class before-monitor before-p :before)
;;;
;;;(defmethod constrain-by-previous ((monitor before-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(> (end-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor before-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (> (end-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class meets-monitor meets-p :meets)
;;;
;;;(defmethod constrain-by-previous ((monitor meets-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(= (end-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor meets-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (= (end-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class overlaps-monitor overlaps-p :overlaps)
;;;
;;;(defmethod constrain-by-previous ((monitor overlaps-monitor) (prev timepoint-monitor) (next monitor))
;;;  (error "~s is a point monitor; it cannot overlap ~s" prev next))
;;;
;;;(defmethod constrain-by-previous ((monitor overlaps-monitor) (prev monitor) (next timepoint-monitor))
;;;  (error "~s is a point monitor; it cannot be overlapped by ~s" next prev))
;;;
;;;(defmethod constrain-by-previous ((monitor overlaps-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (> (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (< (end-of ,prev))))  
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (> (end-of ,prev)))))
;;;
;;;
;;;;;;(define-allen-monitor-class cotemporal-monitor cotemporal-p :cotemporal)
;;;
;;;(defmethod constrain-by-previous ((monitor cotemporal-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(= (start-of ,prev)))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(= (end-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor cotemporal-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (= (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (= (end-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class during-monitor during-p :during)
;;;
;;;(defmethod constrain-by-previous ((monitor during-monitor) (prev monitor) (next timepoint-monitor))
;;;  (error "~a monitors for a point in time; ~a cannot be during it" next prev))
;;;
;;;(defmethod constrain-by-previous ((monitor during-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (< (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (> (end-of ,prev)))))
;;;
;;;
;;;;;;(define-allen-monitor-class finished-by-monitor finished-by-p :finished-by)
;;;
;;;(defmethod constrain-by-previous ((monitor finished-by-monitor) (prev timepoint-monitor) (next monitor))
;;;  (error "~s monitors for a point in time; cannot be finished by ~s" prev next))
;;;  
;;;(defmethod constrain-by-previous ((monitor finished-by-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(= (end-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor finished-by-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (> (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (= (end-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class started-by-monitor started-by-p :started-by)
;;;
;;;(defmethod constrain-by-previous ((monitor started-by-monitor) (prev timepoint-monitor) (next monitor))
;;;  (error "~s monitors for a point in time; cannot be started by ~s" prev next))
;;;
;;;(defmethod constrain-by-previous ((monitor started-by-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(= (start-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor started-by-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (= (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (< (end-of ,prev)))))
;;;
;;;
;;;;;;(define-allen-monitor-class after-monitor after-p :after)
;;;
;;;(defmethod constrain-by-previous ((monitor after-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(< (start-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor after-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (< (start-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class met-by-monitor met-by-p :met-by)
;;;
;;;(defmethod constrain-by-previous ((monitor met-by-monitor) (prev timepoint-monitor) (next monitor))
;;;  (error "~s monitors for a point in time; cannot be met by ~s" prev next))
;;;
;;;(defmethod constrain-by-previous ((monitor met-by-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(= (start-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor met-by-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (= (start-of ,prev)))))
;;;
;;;;;;(define-allen-monitor-class overlapped-by-monitor overlapped-by-p :overlapped-by)
;;;
;;;(defmethod constrain-by-previous ((monitor overlapped-by-monitor) (prev timepoint-monitor) (next monitor))
;;;  (error "~s monitors for a point in time; cannot be overlapped by ~s" prev next))
;;;
;;;(defmethod constrain-by-previous ((monitor overlapped-by-monitor) (prev monitor) (next timepoint-monitor))
;;;  (error "~a monitors for a point in time; can overlap ~a") next prev)
;;;
;;;(defmethod constrain-by-previous ((monitor overlapped-by-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (< (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (> (start-of ,prev))))
;;;  (add-monitor-constraint :timing next
;;;			  `(:end (< (end-of ,prev)))))
;;;
;;;
;;;;;;(define-allen-monitor-class in-order-monitor before-p :in-order)
;;;
;;;(defmethod constrain-by-previous ((monitor in-order-monitor) (prev monitor) (next timepoint-monitor))
;;;  (add-monitor-constraint :timestamp next
;;;			  `(> (end-of ,prev))))
;;;
;;;(defmethod constrain-by-previous ((monitor in-order-monitor) (prev monitor) (next episode-monitor))
;;;  (add-monitor-constraint :timing next
;;;			  `(:start (> (end-of ,prev)))))




;;; -- NOT monitors

(defclass not-monitor (complex-episode-monitor) () )

(defmethod c-episode-monitor-type ((type (eql :not)))
  'not-monitor)

;;; NOT should get all events
(defmethod immediately-relevant-p ((monitor not-monitor) (event-type symbol))
  t)

(defmethod create-monitor-from-pattern ((type (eql :not)) expr parameters pattern)
  (declare (ignore expr))
  (let ((mon (create-complex-monitor-from-pattern type parameters pattern)))
    (dolist (subm (submonitors mon))
      (setf (parent-monitor  subm) mon))
    (setf (relevant-types mon) (cons :any (relevant-types mon)))
    mon))

(defmethod signal-monitor ((monitor not-monitor) (task task) event &optional (added-bindings no-bindings))
  (dolist (subm (submonitors monitor))
    (set-monitor-status subm :unknown nil nil))
  (filter-pipe 
   (lambda (result)
     (general-constraints-met-p monitor result))
   (signal-recording-pipe-result monitor
				 (signal-monitor/1 monitor task event added-bindings))))

(defmethod signal-monitor/1 ((monitor not-monitor) (task task) event &optional (added-bindings no-bindings))
  (unless (eq event :creation)
    (when (not (relevant-event-type-p monitor (first (content event))))
      (return-from signal-monitor/1 empty-pipe)))
  (if (not (eq empty-pipe
	       (mappend-pipe-filtering
		(lambda (submonitor)
		  (signal-monitor submonitor task event added-bindings))
		(submonitors monitor))))
    empty-pipe
    (list (make-monitor-result 
	   no-bindings
	   (make-interval (current-time))
	   monitor))))
    





