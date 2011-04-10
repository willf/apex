;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors-simple-episode.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors-simple-episode.lisp,v 1.16 2006/01/15 03:43:00 dalal Exp $
;;; Created:        September. 2004

;;; --- 
;;; episode monitors.
;;; 'episodes' are intervals of time over which SV values are examined and used for monitoring.
;;;
;;; ----

(in-package :cl-user)

(defclass episode-monitor (non-complex-monitor)
  ((start-time-constraints :initarg :start-time-constraints :initform NIL :accessor monitor-start-time-constraints)
   (end-time-constraints :initarg :end-time-constraints :initform NIL :accessor monitor-end-time-constraints)   
  ))


;;;
;;; A very important subsclass of these are 'atomic episodes' which are (just) lisp CONSes that
;;; get signalled from internal or external sources.  One class of _these_ are the forms that 
;;; get signalled from Apex internally - (enabled task_i) etc.
;;;

(defclass atomic-episode-monitor (episode-monitor)
  ())

;;;
;;; It's possible to define something as an atomic episode, but, in general, anything that doesn't
;;; look like a measurement form gets turned into an atomic episode monitor.
;;; 
(defmethod create-monitor-from-pattern ((type (eql :atomic-episode)) expr parameters pattern)
  (assert (consp expr) nil
      "Atomic patterns must be lists: ~a " expr)
  ;;(assert (not (variable-p (car expr))) nil
  ;;    "Atomic patterns must not have a variable as their first item: ~a" expr)
  (make-instance 'atomic-episode-monitor 
    :pattern pattern
    :expr expr
    :parameters parameters
    :relevant-types (list (first expr))))


(defmethod relevant-event-type-p ((monitor atomic-episode-monitor) type)
  (eql type (car (expr monitor))))

(defmethod meets-start-time-constraints
    ((monitor episode-monitor)
     (timestamp real)
     bindings/s)
  (let ((constraints (monitor-start-time-constraints monitor)))
    (if (not constraints)
      T
      (value-constraints-met-p 
       monitor 
       constraints
       timestamp
       bindings/s))))

(defmethod meets-end-time-constraints
    ((monitor episode-monitor)
     (timestamp real)
     bindings/s)
  (let ((constraints (monitor-end-time-constraints monitor)))
    (if (not constraints)
      T
      (value-constraints-met-p 
       monitor 
       constraints
       timestamp
       bindings/s))))

;;; !! do we need to check for timing constraints, too?
;;;(defmethod signal-monitor/1 ((monitor atomic-episode-monitor) (task task) (cogevent cogevent) &optional (added-bindings no-bindings))
;;;  (if  (not (relevant-event-type-p monitor (first (content cogevent))))
;;;    empty-pipe
;;;    (let* ((eventspec (content cogevent))
;;;	   (bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
;;;	   (waitspec (substitute-bindings-stack (expr monitor) bindings/s))
;;;	   (timestamp (timestamp cogevent)))
;;;      (let ((bindings (pat-match waitspec eventspec)))
;;;	(if (and bindings 
;;;		 (progn (push-bindings bindings bindings/s) t) ;; need it on stack to match 
;;;		 (meets-start-time-constraints monitor timestamp bindings/s)
;;;		 (meets-end-time-constraints monitor timestamp bindings/s))
;;;	  (list (make-monitor-result bindings (make-interval (timestamp cogevent)) monitor))
;;;	  empty-pipe)))))


(defmethod results-from-query ((monitor atomic-episode-monitor) (task task) waitspec timestamp bindings/s)
  (let ((start (earliest-start-of task))
	(end timestamp)) ;; has to be timestamp for the case of Complex monitors ... 
    (if (< end start)
      empty-pipe
      (let ((interval (make-interval start end)))
	(map-pipe 
	 (lambda (l)
	   (let ((new-bindings (car l))
		 (timepoint (second l)))
	     (make-monitor-result 
	      new-bindings 
	      (make-interval timepoint)
	      monitor)))
	 (query-atomic-episode-memory (agent-ae-memory (agent task))
				      interval
				      waitspec
				      bindings/s))))))

;;; 
(defmethod signal-monitor/1 ((monitor atomic-episode-monitor) (task task) (event (eql :creation)) &optional added-bindings)
  (let* ((bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
	 (waitspec (substitute-bindings-stack (monitor-expr monitor) bindings/s))
	 (ts (current-time)))
    (if (parent-monitor monitor)
      (results-from-query monitor task waitspec ts bindings/s)
      empty-pipe)))

;;;(defmethod relevant-to-self-or-parent ((monitor monitor) type)
;;;  (if (not (parent-monitor monitor))
;;;    (relevant-event-type-p monitor type)
;;;    (or (relevant-event-type-p monitor type)
;;;	(relevant-event-type-p (parent-monitor monitor) type))))

;;;(defmethod signal-monitor/1 ((monitor atomic-episode-monitor) (task task) (cogevent cogevent) &optional (added-bindings no-bindings))
;;;  (if (not (relevant-to-self-or-parent-p monitor (first (content cogevent))))
;;;    empty-pipe
;;;    (let* ((eventspec (content cogevent))
;;;	   (bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
;;;	   (waitspec (substitute-bindings-stack (monitor-expr monitor) bindings/s))
;;;	   (ts (timestamp cogevent)))
;;;      (append-pipes 
;;;       ;;; all query-based monitors,
;;;       (if (parent-monitor monitor)
;;;	 (results-from-query monitor task waitspec ts bindings/s)
;;;	 empty-pipe)
;;;       ;; with a current match
;;;       (let* ((bs (pat-match waitspec eventspec)))
;;;;;;	 (let ((start-constraints-ok (meets-start-time-constraints monitor ts bindings/s))
;;;;;;	       (end-constraints-ok (meets-end-time-constraints monitor ts bindings/s)))
;;;;;;	   (pvs! ts (monitor-start-time-constraints monitor) :newline  start-constraints-ok end-constraints-ok))
;;;	 (if bs
;;;	   (progn
;;;	     (push-bindings bs bindings/s)
;;;	     (if (and 
;;;		  (meets-start-time-constraints monitor ts bindings/s)
;;;		  (meets-end-time-constraints monitor ts bindings/s))
;;;	       (progn
;;;		 (list (make-monitor-result 
;;;			bs
;;;			(make-interval ts ts)
;;;			monitor)))
;;;	       empty-pipe))
;;;	   empty-pipe))))))

;;;(defmethod signal-monitor/1 ((monitor atomic-episode-monitor) (task task) (cogevent cogevent) &optional (added-bindings no-bindings))
;;;  (if (not (relevant-to-self-or-parent-p monitor (first (content cogevent))))
;;;    empty-pipe
;;;    (let* ((eventspec (content cogevent))
;;;	   (bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
;;;	   (waitspec (substitute-bindings-stack (monitor-expr monitor) bindings/s))
;;;	   (ts (timestamp cogevent)))
;;;      ;; (pvs! waitspec :newline monitor :newline task :newline added-bindings :newline (get-local-context task) :newline (task-globals task))
;;;	    ;;; all query-based monitors,
;;;       (if (parent-monitor monitor)
;;;	 (results-from-query monitor task waitspec ts bindings/s)
;;;	 ;; with a current match
;;;	 (let* ((bs (pat-match waitspec eventspec)))
;;;	   (if bs
;;;	     (progn
;;;	       (push-bindings bs bindings/s)
;;;	       (if (and 
;;;		    (meets-start-time-constraints monitor ts bindings/s)
;;;		    (meets-end-time-constraints monitor ts bindings/s))
;;;		 (progn
;;;		   (list (make-monitor-result 
;;;			  bs
;;;			  (make-interval ts ts)
;;;			  monitor)))
;;;		 empty-pipe))))))))

(defmethod signal-monitor/1 ((monitor atomic-episode-monitor) (task task) (cogevent cogevent) &optional (added-bindings no-bindings))
  (if (not (relevant-to-self-or-parent-p monitor (first (content cogevent))))
    empty-pipe
    (let* (;; (eventspec (content cogevent))
	   (bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
	   (waitspec (substitute-bindings-stack (monitor-expr monitor) bindings/s))
	   (ts (timestamp cogevent)))
      (results-from-query monitor task waitspec ts bindings/s))))
;;;
;;; Simple episodes look for one kind of thing over the interval. 
;;;


(defclass simple-episode-monitor (episode-monitor probe-mixin) 
  (
      (duration-constraints :initarg :duration-constraints :initform NIL :accessor monitor-duration-constraints)
   
   (difference-constraints :initarg :difference-constraints :initform nil :accessor monitor-difference-constraints)  
   (stddev-constraints :initarg :stddev-constraints :initform NIL :accessor monitor-stddev-constraints)
   (variance-constraints :initarg :variance-constraints :initform NIL :accessor monitor-variance-constraints)
   (count-constraints :initarg :count-constraints :initform NIL :accessor monitor-count-constraints)
   (sum-constraints :initarg :sum-constraints :initform NIL :accessor monitor-sum-constraints)
   (mean-constraints :initarg :mean-constraints :initform NIL :accessor monitor-mean-constraints)
   (rate-constraints :initarg :rate-constraints :initform NIL :accessor monitor-rate-constraints)
   (first-value-constraints :initarg :first-value-constraints :initform NIL :accessor monitor-first-value-constraints)   
   (last-value-constraints :initarg :last-value-constraints :initform NIL :accessor monitor-last-value-constraints)      
   (quality-constraints :initarg :quality-constraints :initform NIL :accessor monitor-quality-constraints)
   (trend-constraint-methods 
    :initarg :trend-constraint-mehthods
    :initform NIL
    :accessor monitor-trend-constraint-methods))
  )


;;; :stat (:mean (> 10) (< 20)) ... 
(defmethod add-monitor-constraints-level-2 ((mon episode-monitor) kind (keyword-parameters list) current-keyword)
  (when (not (null keyword-parameters))
    (let ((kp (car keyword-parameters))
	  (kps (cdr keyword-parameters)))
      (if (keywordp kp)
	(add-monitor-constraints-level-2 mon kind kps kp)
	(progn
	  (dolist (constraint (convert-constraints kp))
	    (add-monitor-constraint/2 kind current-keyword mon constraint))
	  (add-monitor-constraints-level-2 mon kind kps current-keyword))))))

(defmethod add-monitor-constraints-arity2 ((monitor episode-monitor) kind value)
  (assert (consp value) nil
    "List of  constraints are not provided: ~s" value)
  (add-monitor-constraints-level-2 monitor kind (cdr value) (car value)))

(defmethod add-monitor-constraint/2 (kind kind2 monitor constraint)
  (error "Don't know how to add a ~s ~s ~s for ~s monitors"
	 kind kind2 constraint (type-of monitor)))

(defmethod add-monitor-constraint ((type (eql :first-value)) (monitor simple-episode-monitor) value)
  (setf (monitor-first-value-constraints monitor)
    (append (monitor-first-value-constraints monitor)
	    (list value))))

(defmethod add-monitor-constraint ((type (eql :last-value)) (monitor simple-episode-monitor) value)
  (setf (monitor-last-value-constraints monitor)
    (append (monitor-last-value-constraints monitor)
	    (list value))))
  
(defmethod add-monitor-constraint ((type (eql :stats)) (monitor simple-episode-monitor) value)
  (add-monitor-constraints-arity2 monitor type value))

(defmethod add-monitor-constraint ((type (eql :timing)) (monitor episode-monitor) value)
  (add-monitor-constraints-arity2 monitor type value))

;;; start/end constraints

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :start)) (monitor episode-monitor) value)
  (setf (monitor-start-time-constraints monitor)
    (append (monitor-start-time-constraints monitor)
	    (list value))))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :end)) (monitor episode-monitor) value)
  (setf (monitor-end-time-constraints monitor)
    (append (monitor-end-time-constraints monitor)
	    (list value))))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :earliest-start)) (monitor episode-monitor) value)
  (setf (monitor-start-time-constraints monitor)
    (append (monitor-start-time-constraints monitor)
	    `((>= ,value)))))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :earliest-end)) (monitor episode-monitor) value)
  (setf (monitor-end-time-constraints monitor)
    (append (monitor-end-time-constraints monitor)
	    `((>= ,value)))))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :latest-start)) (monitor episode-monitor) value)
  (setf (monitor-start-time-constraints monitor)
    (append (monitor-start-time-constraints monitor)
	    `((<= ,value)))))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :latest-end)) (monitor episode-monitor) value)
  (setf (monitor-end-time-constraints monitor)
    (append (monitor-end-time-constraints monitor)
	    `((<= ,value)))))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :es)) (monitor episode-monitor) value)
  (add-monitor-constraint/2 :timing :earliest-start monitor value))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :ee)) (monitor episode-monitor) value)
  (add-monitor-constraint/2 :timing :earliest-end monitor value))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :ls)) (monitor episode-monitor) value)
  (add-monitor-constraint/2 :timing :latest-start monitor value))

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :le)) (monitor episode-monitor) value)
  (add-monitor-constraint/2 :timing :latest-end monitor value))

;;; -- duration constraints 

(defmethod add-monitor-constraint/2 ((kind (eql :timing)) (type (eql :duration)) (monitor simple-episode-monitor) value)
  (setf (monitor-duration-constraints monitor)
    (append (monitor-duration-constraints monitor)
	    (list value))))

  
;;; -- difference constraints 

(defmethod add-monitor-constraint/2 ((kind (eql :stats)) (type (eql :difference)) (monitor simple-episode-monitor) value)
  (setf (monitor-difference-constraints monitor)
    (append (monitor-difference-constraints monitor)
	   (list value))))


;;; -- stddev constraints 

(defmethod add-monitor-constraint/2 ((kind (eql :stats)) (type (eql :stddev)) (monitor simple-episode-monitor) value)
  (setf (monitor-stddev-constraints monitor)
    (append (monitor-stddev-constraints monitor)
	   (list value))))


;;; -- variance constraints 

(defmethod add-monitor-constraint/2 ((kind (eql :stats)) (type (eql :variance)) (monitor simple-episode-monitor) value)
  (setf (monitor-variance-constraints monitor)
    (append (monitor-variance-constraints monitor)
	   (list value))))


;;; -- count constraints 

(defmethod add-monitor-constraint/2 ((kind (eql :stats)) (type (eql :count)) (monitor simple-episode-monitor) value)
  (setf (monitor-count-constraints monitor)
    (append (monitor-count-constraints monitor)
	   (list value))))


;;; -- sum constraints 

(defmethod add-monitor-constraint/2 ((kind (eql :stats)) (type (eql :sum)) (monitor simple-episode-monitor) value)
  (setf (monitor-sum-constraints monitor)
    (append (monitor-sum-constraints monitor)
	   (list value))))


;;; -- mean constraints 

(defmethod add-monitor-constraint/2 ((kind (eql :stats)) (type (eql :mean)) (monitor simple-episode-monitor) value)
  (setf (monitor-mean-constraints monitor)
    (append (monitor-mean-constraints monitor)
	   (list value))))



(defmethod add-monitor-constraint ((type (eql :trend)) (monitor simple-episode-monitor) value)
  (add-monitor-constraints-arity2 
   monitor 
   type
   (convert-trend-constraint value)))

(defun convert-trend-constraint (constraint)
  (case (car constraint)
    (:rate `(:rate ,@(mapcar 'convert-rate-constraint (cdr constraint))))
    (:step `(:step ,@(mapcar 'convert-step-constraint (cdr constraint))))
    (t constraint)))

(defun convert-rate-constraint (constraint)
  (if (symbolp constraint)
    (symbolic-rate-to-list constraint)
    constraint))

(defun convert-step-constraint (constraint)
  (make-step-compare-function constraint))
    

;;; (:trend (:rate (> 10 P1s)))
;;; (:trend (:rate (:range (< 10 P1s) (> 10 P0.1s))))
;;; (:trend (:rate :increasing (> 10)) (:step :decreasing)) 

(defmethod symbolic-rate-to-list ((value (eql :increasing)))
  `(> 0))

(defmethod symbolic-rate-to-list ((value (eql :decreasing)))
  `(< 0))

(defmethod symbolic-rate-to-list ((value (eql :non-increasing)))
  `(<= 0))

(defmethod symbolic-rate-to-list ((value (eql :non-decreasing)))
  `(>= 0))


(defmethod add-monitor-constraint/2 ((kind (eql :trend)) (type (eql :rate)) (monitor simple-episode-monitor) value)
  (assert (consp value) nil "Rate constraint must be a list ~s" value)
  (let ((constraint 
	 (let ((op (car value))
	       (expr (second value)))
	   (if (consp expr)
	     (let ((units (car expr))
		   (dur (cadr expr)))
	       `(,op ($units-per-duration ,units ,dur)))
	     value))))
    (setf (monitor-rate-constraints monitor)
      (append
       (monitor-rate-constraints monitor)
       (list constraint)))))


(defmethod make-step-compare-function ((value (eql :increasing)))
  (lambda (b a)
    (> (- b a) 0)))

(defmethod make-step-compare-function ((value (eql :decreasing)))
  (lambda (b a)
    (< (- b a) 0)))

(defmethod make-step-compare-function ((value (eql :non-increasing)))
  (lambda (b a)
    (<= (- b a) 0)))

(defmethod make-step-compare-function ((value (eql :non-decreasing)))
  (lambda (b a)
    (>= (- b a) 0)))

;; (> .5)
(defmethod make-step-compare-function ((value list))
  (lambda (b a)
    (patmatch-constraint (- b a) value)))


;; by now value should be a function ...
(defmethod add-monitor-constraint/2 ((kind (eql :trend)) (type (eql :step)) (monitor simple-episode-monitor) value)
  (setf (monitor-trend-constraint-methods monitor)
    (append (monitor-trend-constraint-methods monitor)
	    (list (pairwise-constraints-met-function monitor value)))))

;;; (:trend (:custom trend-is-ok))
;;; (:trend (:custom #'trend-is-ok))
;;; (:trend (:custom (lambda (history interval) (trend-is-ok history interval))))
(defmethod add-monitor-constraint/2 ((kind (eql :trend)) (type (eql :custom)) (monitor simple-episode-monitor) value)
  (cond
   ;; (:trend trend-is-ok)
   ((symbolp value)
    (setf (monitor-trend-constraint-methods monitor)
      (append (monitor-trend-constraint-methods monitor)
	      (list value))))
   ;; (:trend #'trend-is-ok)
   ((functionp value)
    (setf (monitor-trend-constraint-methods monitor)
      (append (monitor-trend-constraint-methods monitor)
	      (list value))))
   ;; (:trend (lambda (history interval) ...))
   ((and (consp value)
	 (eq (car value) 'lambda))
    (setf (monitor-trend-constraint-methods monitor)
      (append (monitor-trend-constraint-methods monitor)
	      (list value))))
   (t (error "Unknown custom trend constraint type: ~s" value))))



(defmethod create-monitor-from-pattern ((type (eql :episode)) expr parameters pattern)
  (let ((monitor (make-instance 'simple-episode-monitor)))
    (unless (valid-sv-form-p expr)
      (error "Invalid state variable form in ~s" pattern))
    (setf (relevant-types monitor)
      (list (car expr)))
    (parameter-require monitor ':quality parameters)
    monitor))

(defmethod add-monitor-constraint ((type (eql :quality))
				   (monitor simple-episode-monitor) value)
  (add-monitor-constraints-arity2 monitor type value))

(defmethod add-monitor-constraint/2 ((kind (eql :quality)) (type (eql :no-constraints)) (monitor episode-monitor) value)
  (declare (ignore value))
  (setf (monitor-quality-constraints monitor) nil))

(defun quality-symbol-to-list (x)
  (assert (symbolp x))
    `(<= ,x))

(defun collect-quality-constraints (l)
  (loop for x in l
      collecting
	(if (symbolp x) (quality-symbol-to-list x) x)))

(defmethod add-monitor-constraint/2 ((kind (eql :quality)) (type (eql :minimum-sample-interval)) (monitor episode-monitor) value)
  (setf (monitor-quality-constraints monitor)
    (append (monitor-quality-constraints monitor)
	    (collect-quality-constraints (list value)))))

(defmethod add-monitor-constraint/2 ((kind (eql :quality)) (type (eql :msi)) (monitor episode-monitor) value)
  (setf (monitor-quality-constraints monitor)
    (append (monitor-quality-constraints monitor)
	    (collect-quality-constraints (list value)))))


;;;(defmethod duration-from-spec ((monitor estimation-mixin) bindings default)
;;;  (let* ((durspec (or (parameter-value :with-timeout (monitor-estimation-parameters monitor))
;;;					default))
;;;	 (duration (convert-duration-specs (substitute-bindings durspec bindings))))
;;;  duration))



;;; (100 P3S) -> 100/3000
;;; (100 P1M) -> 100/60000
(defun convert-rate-spec (spec) ;; assume fully bound !
    (/ (car spec) (cadr spec)))

(defun $units-per-duration (units duration)
  (/ units duration))


(defmethod create-episode-monitor-from-pattern ((type (eql :trend)) expr parameters pattern)
  (declare (ignore expr))
  (if (not (member :estimation parameters))
    (error "No estimation method specified in ~A" pattern)
    (make-instance 'trend-monitor)))


;;;
;;; the idea here is to provide a method which does pairwise comparisons across
;;; an interval. i.e., given measurement points at t1, t2, t3, ..., tn-1, tn
;;; an estmation function f(t), and a comparsion function g(x,y),
;;; g(f(t1),f(t2)), g(f(t2), f(t3)), ... g(f(tn-1), f(tn)) are all true.
;;; 
;;;
(defmethod pairwise-constraint-over-interval-met-p
    ((history sv-history)
     (data-interval interval)
     (pairwise-constraint-function function)
     )
  ;; (pvs! "Entering p-c-o-i-m-p" :newline history data-interval)
  (let ((last-value NIL))
    (sv-history-for-each 
     history data-interval
     (lambda (measurement)
       (let ((value (measurement-value measurement)))
	 (when (and last-value
		    (not (funcall pairwise-constraint-function
				  value last-value)))
	   ;; (pvs! "Failed with" value last-value)
	   (return-from pairwise-constraint-over-interval-met-p  nil))
	 (setq last-value value))))
    T ;; i.e., we never failed.
    ))


(defmethod pairwise-constraints-met-function ((monitor monitor) (compare-function function))
  ;; (pvs! "Entering p-w-m-f ccreation" compare-function)
  (lambda (history interval bindings)
    (declare (ignore bindings))
    ;; (pvs! "Entering p-w-m-f call" history :newline interval :newline compare-function)
    (pairwise-constraint-over-interval-met-p history interval compare-function)))

(defmethod quality-constraints-met-p 
    ((monitor simple-episode-monitor)
     (history sv-history)
     (data-interval interval)
     bindings/s)
  (let ((last-ts nil)
	(count 0))
    (sv-history-for-each 
     history data-interval
     (lambda (measurement)
       (let ((ts (measurement-timestamp measurement)))
	 (when last-ts
	   (let ((val (- ts last-ts)))
	     (unless 
		 (value-constraints-met-p
		  monitor
		  (monitor-quality-constraints monitor)
		  val
		  bindings/s)
	       (return-from quality-constraints-met-p nil))))
	 (incf count)
	 (setq last-ts ts))))
    (if (zerop count)
      nil ;; must have *one* anyway
      T) ;; i.e., we never failed.
    ))

;;; return a pipe of intervals created from the timestamps of points
;;; of two intervals.
;;; [ t1 t2 t3 ]  [   t4 t5 t6    ]
;;; [t1 t4], [t1 t5], [t1 t6], [t2 t4], ... , [t3 t6] 
;;;
(defmethod interval-pipe ((monitor simple-episode-monitor)
			  (history sv-history)
			  (start interval)
			  (end interval)
			  bindings/s)
  (declare (ignore bindings/s))
  ;; get pipes of timestamps ...
  (let ((start-pipe (map-pipe #'measurement-timestamp (sv-measurement-pipe history start)))
	(end-pipe   (map-pipe #'measurement-timestamp (sv-measurement-pipe history end))))
    ;;(setq start-pipe (enumerate-pipe start-pipe)) (pvs! start-pipe)
    ;;(setq end-pipe (enumerate-pipe end-pipe)) (pvs! end-pipe)
    ;; filter out values where a pair (t1, t2) has t2 < t1 (a bad interval)
    (filter-pipe 
     #'identity
     (map-pipe
      (lambda (pair)
	(let ((start (car pair))
	      (end (cadr pair)))
	  (if (and start end (<= start end))
	    (make-interval start end)
	    NIL)))
      (pairs-pipe start-pipe end-pipe)))))


(defmethod interval-pipe ((monitor simple-episode-monitor)
			  (history sv-history)
			  (start NULL)
			  (end interval)
			  bindings/s)
  (declare (ignore bindings/s))
  (list end))

(defmethod interval-pipe ((monitor simple-episode-monitor)
			   (history sv-history)
			   (start interval)
			   (end NULL)
			   bindings/s)
  (declare (ignore bindings/s))
  (list start))

(defmethod interval-pipe ((monitor simple-episode-monitor)
			  (history sv-history)
			   (start NULL)
			  (end NULL)
			  bindings/s)
  (declare (ignore bindings/s))
  nil)
  
      

(defmethod every-measurement-meets-value-constraints-p 
    ((monitor simple-episode-monitor)
     (history sv-history)
     (interval interval)
     bindings/s)
  (sv-history-every 
   history interval
   (lambda (obs)
     (value-constraints-met-p
      monitor
      (monitor-value-constraints monitor)
      (measurement-value obs)
      bindings/s))))


(defmethod interval-meets-statistical-constraints-p
    ((monitor simple-episode-monitor)
     (history sv-history)
     (interval interval)
     bindings/s)
  (if (not 
       (or
	(monitor-mean-constraints monitor)
	(monitor-difference-constraints monitor)
	(monitor-stddev-constraints monitor)
	(monitor-variance-constraints monitor)
	(monitor-count-constraints monitor)
	(monitor-sum-constraints monitor)))
    ;; just get out quick ..
    T
    (progn
      (set-view-interval history interval)
      ;; check each in turn ...
      ;; mean
      (when (monitor-mean-constraints monitor)
	(unless 
	    (value-constraints-met-p
	     monitor
	     (monitor-mean-constraints monitor)
	     (view-mean history)
	     bindings/s)
	  (return-from 
	      interval-meets-statistical-constraints-p
	    NIL)))
      ;; difference
      (when (monitor-difference-constraints monitor)
	(unless 
	    (value-constraints-met-p
	     monitor
	     (monitor-difference-constraints monitor)
	     (view-difference history)
	     bindings/s)
	  (return-from 
	      interval-meets-statistical-constraints-p
	    NIL)))  
      ;; stddev
      (when (monitor-stddev-constraints monitor)
	(unless 
	    (value-constraints-met-p
	     monitor
	     (monitor-stddev-constraints monitor)
	     (view-stddev history)
	     bindings/s)
	  (return-from 
	      interval-meets-statistical-constraints-p
	    NIL)))
      ;; variance
      (when (monitor-variance-constraints monitor)
	(unless 
	    (value-constraints-met-p
	     monitor
	     (monitor-variance-constraints monitor)
	     (view-variance history)
	     bindings/s)
	  (return-from 
	      interval-meets-statistical-constraints-p
	    NIL)))      
      ;; count
      (when (monitor-count-constraints monitor)
	(unless 
	    (value-constraints-met-p
	     monitor
	     (monitor-count-constraints monitor)
	     (view-count history)
	     bindings/s)
	  (return-from 
	      interval-meets-statistical-constraints-p
	    NIL)))
      ;; sum
      (when (monitor-sum-constraints monitor)
	(unless 
	    (value-constraints-met-p
	     monitor
	     (monitor-sum-constraints monitor)
	     (view-sum history)
	     bindings/s)
	  (return-from 
	      interval-meets-statistical-constraints-p
	    NIL)))
      ;; ok -- we pass through it all, so it's true
      T)))


(defmethod interval-meets-trend-constraints
    ((monitor simple-episode-monitor)
     (history sv-history)
     (interval interval)
     bindings/s)
  (every (lambda (method)
	   ;; (pvs! "About to call method")
	   (sv-history-funcall history interval method bindings/s))
	 (monitor-trend-constraint-methods monitor)))

(defmethod interval-meets-duration-constraints
    ((monitor simple-episode-monitor)
     (interval interval)
     bindings/s)
  (let ((constraints (monitor-duration-constraints monitor)))
    (if (not constraints)
      T
      (let ((duration (- (interval-end interval) (interval-start interval))))
	(value-constraints-met-p 
	 monitor 
	 constraints
	 duration
	 bindings/s)))))

(defmethod first-value-meets-constraints ((monitor simple-episode-monitor)
					 (history sv-history)
					 (data-interval interval)
					 bindings/s)
  (let ((constraints (monitor-first-value-constraints monitor)))
    (if (null constraints)
      T
      (let ((first-meas (first-value-within history data-interval)))
	(if (null first-meas)
	  t
	  (value-constraints-met-p 
	   monitor
	   constraints
	   (measurement-value first-meas)
	   bindings/s))))))

(defmethod last-value-meets-constraints ((monitor simple-episode-monitor)
					 (history sv-history)
					 (data-interval interval)
					 bindings/s)
  (let ((constraints (monitor-last-value-constraints monitor)))
    (if (null constraints)
      T
      (let ((last-meas (last-value-within history data-interval)))
	(if (null last-meas)
	  t
	  (value-constraints-met-p 
	   monitor
	   constraints
	   (measurement-value last-meas)
	   bindings/s))))))

(defmethod values-meet-rate-constraints ((monitor simple-episode-monitor)
					 (history sv-history)
					 (data-interval interval)
					 bindings/s)
  (let ((constraints (monitor-rate-constraints monitor)))
    (if (null constraints)
      T  
      ;; check that we have a proper interval ...
      (if (= (interval-start data-interval)
	     (interval-end data-interval))
	T
	;; otherwise, try to get the rate (= slope of regression line)
	(let* ((rate (view-slope history)))
	  ;; (pvs! "Checking rate. " history :newline rate )
	  (if (null rate)
	    T
	    (value-constraints-met-p monitor constraints rate bindings/s)))))))


;;; given a *specific* state-variable history, see if something works.
;;; this will return a binding set on sucess 

(defmethod signal-monitor-on-history ((monitor simple-episode-monitor) (task task) (history sv-history) bindings/s)
    (map-pipe-filtering 
     (lambda (interval)
       ;; (pvs! :newline "In pipe, " history :newline interval)
       (set-view-interval history interval)
       (and 
	(quality-constraints-met-p monitor history interval bindings/s)
	(interval-meets-duration-constraints monitor interval bindings/s)
	(every-measurement-meets-value-constraints-p monitor history interval bindings/s)
	(interval-meets-statistical-constraints-p monitor history interval bindings/s)
	(interval-meets-trend-constraints  monitor history interval bindings/s)
	(first-value-meets-constraints  monitor history interval bindings/s)
	(last-value-meets-constraints  monitor history interval bindings/s)	
	(values-meet-rate-constraints monitor history interval bindings/s)
	;; (prog1 t (pvs! "Constraints are met!"  history :newline interval :newline bindings))
	(make-monitor-result (first bindings/s) interval monitor)))
     (multiple-value-bind (startint endint)
	 (intervals-for-pipes monitor bindings/s)
       (let ((pipe 
	      (interval-pipe 
	       monitor
	       history
	       startint
	       endint
	       bindings/s)))
	 pipe))))

(defmethod signal-monitor/1 ((monitor simple-episode-monitor) (task task) (cogevent T) &optional (added-bindings no-bindings))
  (unless (eq cogevent :creation)
    (when (not (relevant-event-type-p monitor (first (content cogevent))))
      (return-from signal-monitor/1 empty-pipe)))
  (let ((bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task))))
    (let ((waitspec (substitute-bindings-stack (monitor-expr monitor) bindings/s))
	  (mem (task-memory task)))
      (if (variable-p (sv-form-object waitspec))
	(mappend-pipe-filtering 
	 (lambda (obj)
	   (let ((history (sv-history mem 
				      (make-state-variable
				       (sv-form-attribute waitspec)
				       obj))))
	     (let ((bpipe (signal-monitor-on-history monitor task history bindings/s)))
	       (if (not (eq bpipe empty-pipe))
		 ;; get all binding sets with value extended
		 (map-pipe-filtering 
		  (lambda (result)
		    (let ((bs (extend-bindings (second waitspec) obj (binding-set result))))
		      (if (value-constraints-met-p 
			   monitor 
			   (monitor-object-constraints monitor)
			   obj
			   (push-bindings bs bindings/s) )
			(progn
			  (setf (binding-set result) bs)
			  result)
			nil)))
		  bpipe)
		 nil ;; empty pipe
		 ))))
	 (sv-memory-objects 
	  (task-memory task) 
	  (sv-form-attribute waitspec)))
	;; not a variable
	(let ((history (sv-history mem 
				   (make-state-variable
				    (sv-form-attribute waitspec)
				    (sv-form-object waitspec)))))
	  (signal-monitor-on-history monitor task history bindings/s))))))


(defun t+ (a b)
  (cond
   ((= a +end-of-time+) 
    +end-of-time+)
   ((= b +pos-infinity+)
    +end-of-time+)
   (t (+ a b))))

(defun t- (a b)
  (cond
   ((and (= a +end-of-time+)
	 (= b +pos-infinity+))
    +beginning-of-time+)
   ((= a +end-of-time+)
    +end-of-time+)
   ((= b +pos-infinity+)
    +beginning-of-time+)
   (t (- a b))))

(defmethod interval-minus-duration ((end interval) (duration interval))
  (make-interval (max (t- (interval-start end)
			  (interval-end duration))
		      0)
		 (max (t- (interval-end end)
			  (interval-start duration))
		      0)))

(defmethod interval-plus-duration ((start interval) (duration interval))
  (make-interval (t+ (interval-start start)
		    (interval-start duration))
		 (t+ (interval-end start)
		    (interval-end duration))))

(defmethod interval-intersection ((a interval) (b interval))
  (let ((beginning (max (interval-start a) (interval-start b)))
	(ending (min (interval-end a) (interval-end b))))
    (if (>= ending beginning)
      (make-interval beginning ending)
      nil)))

;;; -------------
;;; case analysis. assume we might have a closed interval for es/ls and ee/le,
;;; and a closed interval for min/max duration.
;;; 
;;; the default start interval is [(start-of +this-task+), +end-of-time+]
;;; the default end inteval is also [(start-of +this-task+), +end-of-time+]
;;; the default duration interval is [0,+pos=infinity+]
;;;
;;; duration-interval  start-interval end-interval / start sample                   end sample
;;; no                 no             no             S-DEFAULT                      E-DEFAULT
;;; no                 no             yes            start(S-DEFAULT),start(end-i)  end-i
;;; no                 yes            no             start-i                        end(start-i),end(DEFAULT)
;;; no                 yes            yes            start-i                        end-i
;;; yes                no             no             S-DEFAULT ^^ (E-DEFAULT --dur) E-DEFAULT ^^ (S_DEFAULT ++ dur)
;;; yes                no             yes            end-i -- dur                   end-i
;;; yes                yes            no             start-i                        start-i ++ dur
;;; yes                yes            yes            start-i ^^ (end-i -- dur)      end-i ^^ (start-i ++ dur)
;;;
;;; in general: start-sample: start-i ^^ (end-i -- dur); end-sample: end-i ^^ (start-i ++ dur)
;;; 

(defmethod closed-start-interval ((monitor simple-episode-monitor) bindings)
  (let ((task (task monitor)))
    (close-time-interval
     (determine-extreme-values 
      (evaluate-constraints (monitor-start-time-constraints monitor) bindings)
      (t-started task) :closed
      +end-of-time+ :closed))))


(defmethod closed-end-interval ((monitor simple-episode-monitor) bindings)
  (let ((task (task monitor)))
    (close-time-interval
     (determine-extreme-values 
      (evaluate-constraints (monitor-end-time-constraints monitor) bindings)
      (t-started task) :closed
      +end-of-time+ :closed))))


(defmethod closed-duration-interval ((monitor simple-episode-monitor) bindings)
  (close-time-interval
   (determine-extreme-values 
    (evaluate-constraints (monitor-duration-constraints monitor) bindings)
      0 :closed
      +pos-infinity+ :closed)))

(defmethod intervals-for-pipes ((monitor simple-episode-monitor) bindings)
  (let ((startint (closed-start-interval monitor bindings))
	(endint (closed-end-interval monitor bindings))
	(durint (closed-duration-interval monitor bindings)))
    (values
     (interval-intersection startint
			    (interval-minus-duration 
			     endint durint))
     (interval-intersection endint
			    (interval-plus-duration
			     startint durint)))))


;;; (defmethod special-monitor-pattern-p ((tag (eql :delay))) T)

;;; (:delay P2s)
;;; (:delay P2s P4m)
;;; (:delay P2s P4m)
;;; 



