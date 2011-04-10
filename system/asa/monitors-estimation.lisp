;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors-estimation.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors-estimation.lisp,v 1.5 2006/01/15 03:43:00 dalal Exp $
;;; Created:        September. 2004

;;; estimation monitors

;;;
;;; Measurement monitors are monitors based on specific individual values in the state-value
;;; history *or* values arriving as signals.
;;;
;;; Syntax
;;; 
;;; measurement-monitor-pattern:   (:measurement [<tag>] (<attr> <obj/v> = <val/v>)
;;;                                     :estimation <estimator>
;;;                                     [:timestamp <constraint>*:(:range (start-of +monitor+) +end-of-time+)]
;;;                                     [:value     <constraint>*]
;;;                                     [:object    <constraint>*]
;;;                                )
;;; tag:   symbol | string
;;; attr:  symbol
;;; obj/v: symbol | variable
;;; val/v: atom | variable
;;;
;;; estimator: persist-estimator | linear-regression-estimator
;;;
;;; persist-estimator: (:persist [:with-timeout <duration>:infinity])
;;;
;;; linear-regression-estimator: (:linear-regression 
;;;                                 [:minimum-points <int>:2]
;;;                                 [:maximum-error <real>:infinity]
;;;                                 [:maximum-difference <real>:infinity]
;;;                                 [:minimum-frequency <real>:infinity]
;;;                                 [:start <time-point>:0]
;;;                                 [:end <time-point>:+current-time+]
;;;                               )
;;;
;;; The *start* and *end* parameters for :linear-regression describe the interval
;;; from which the linear regression equation is generated.
;;
;;; Note: the presence of an :estimation form indicates an 'estimation' monitor. Without
;;; it, it is an measurement monitor. See monitors-measurement.lisp
;;;
;;;

(in-package :cl-user)

(defclass estimation-monitor (measurement-monitor estimation-mixin) ())

(defmethod declare-estimation-method ((type (eql :persist)) (monitor estimation-monitor) parameters)
  (parameter-check type monitor (cdr parameters))
  (setf (monitor-estimation-type monitor) :persist)
  (setf (monitor-estimation-parameters monitor) (cdr parameters)))

;; (:linear-regression :minimum-points 10 ..)
(defmethod declare-estimation-method ((type (eql :linear-regression)) (monitor estimation-monitor) parameters)
  (parameter-check type monitor (cdr parameters))  
  (setf (monitor-estimation-type monitor) type)
  (setf (monitor-estimation-parameters monitor) (cdr parameters)))


(defmethod signal-monitor/1 ((monitor estimation-monitor) (task task) (event (eql :creation)) &optional (added-bindings no-bindings))
  (results-from-estimate monitor task added-bindings))

(defmethod signal-monitor/1 ((monitor estimation-monitor) (task task) (cogevent cogevent)  &optional (added-bindings no-bindings))
  (if (not (relevant-event-type-p monitor (first (content cogevent))))
    empty-pipe
    (results-from-estimate monitor task added-bindings)))


(defmethod results-from-estimate ((monitor estimation-monitor) (task task) added-bindings)
  (let* ((bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
	 (waitspec (substitute-bindings (monitor-expr monitor) bindings/s))
	 (attr (measurement-form-attribute waitspec))
	 (obj  (measurement-form-object waitspec))
	 (val  (measurement-form-value  waitspec))
	 )
    (if (not (variable-p obj))
      (if (value-constraints-met-p monitor (monitor-object-constraints monitor) obj bindings/s)
	(let ((result
	       (result-from-estimate-once monitor (make-state-variable attr obj) val task bindings/s)))
	  (if result (list result) empty-pipe))
	empty-pipe)
      (map-pipe-filtering 
       (lambda (object)
	 (if (value-constraints-met-p monitor (monitor-object-constraints monitor) object bindings/s)
	   (let ((result (result-from-estimate-once monitor (make-state-variable attr object) val task bindings/s)))
	     (if (not (null result))
	       (progn
		 (setf (binding-set result)
		   (extend-bindings obj object (binding-set result)))
		 result)
	       nil))
	   nil))
       (sv-memory-objects (agent-sv-memory (agent task)) attr)))))

(defmethod result-from-estimate-once ((monitor estimation-monitor) (sv state-variable) value (task task) bindings/s)
  (estimation-result
   (monitor-estimation-type monitor)
   monitor
   (sv-history (agent-sv-memory (agent task)) sv)
   value
   bindings/s))
	  

;;; return binding-set if succeeds
;;; return FAIL if fail

(defmethod estimation-result ((type T) (monitor estimation-monitor) (history sv-history) value/v bindings/s)
  (declare (ignore value/v bindings/s))
  (error "Unknown estmation method: ~a" type))
  
;;;
;;; Persistance estimators.
;;;
;;; Canonical example: assuming that values persist for some length of time,
;;; does a SV have a particular value at a given timepoint?
;;;
;;; (waitfor (:measurement o1 (altitude ac-1 32000)
;;;             :timestamp ( (= (+ (start-of +this-task+) P30M)) )
;;;             :estimate (persist :with-timeout P30S) ))
;;;
;;; Algorithm:
;;; 1. determine the interval extremes. (est-interval: t_1, t_2)
;;; 2. if no measurement _at_ t1, check the previous measurement (if any). If it
;;;    meets constraints (including persistence), use it; if not goto next line ...
;;;    otherwise, check each measurement in est-interval. use the first measurement
;;;    that meets the constraints.


(defmethod measurement-meets-constraints-p ((monitor measurement-monitor) (measurement measurement) bindings/s)
  (and
   (value-constraints-met-p monitor (timestamp-constraints monitor) 
			    (measurement-timestamp measurement) bindings/s)
   (value-constraints-met-p monitor (monitor-value-constraints monitor)
			    (measurement-value measurement) bindings/s)
;;;   (value-constraints-met-p monitor (monitor-object-constraints monitor)
;;;			    (sv-object (measurement-sv measurement)) bindings)
   ))

;;; actually, we need to *not* check all timestamp constraints ...
(defmethod meets-persists-constraints-p ((monitor estimation-monitor)
				      (measurement measurement)
				      bindings/s)
  (and (value-constraints-met-p 
	monitor 
	(non-comparative-constraints (timestamp-constraints monitor))
	(measurement-timestamp measurement) bindings/s)
       (value-constraints-met-p monitor (monitor-value-constraints monitor)
			    (measurement-value measurement) bindings/s)))
   
(defmethod estimation-result ((type (eql :persist)) (monitor estimation-monitor) (history sv-history) value/v bindings/s)
  (let ((duration-spec (parameter-value :with-timeout (monitor-estimation-parameters monitor))))
    (let* ((persistence-duration (if duration-spec (duration-read duration-spec) +pos-infinity+))
	   (est-interval (close-time-interval
			 (determine-extreme-values 
			  (evaluate-constraints (timestamp-constraints monitor) bindings/s)
			  (start-of monitor) :closed
			  +end-of-time+ :closed))))
      (unless est-interval (return-from estimation-result nil))
      ;;; first, we check the special case for a previous measurement within the persistence duration
      ;;; before the interval
      (let ((first-obs (first-measurement-within history est-interval)))
	(when (or (null first-obs)
		  (/= (measurement-timestamp first-obs)
		      (interval-start est-interval)))
	  ;;; is there a previous measurement within the persistence duration?
	  (let ((prev-obs  (last-measurement-within history (make-interval 0 (interval-start est-interval)))))
	    (when (and prev-obs
		       (>= persistence-duration
			   (- (interval-start est-interval)
			      (measurement-timestamp prev-obs)))
		       (meets-persists-constraints-p monitor 
						     prev-obs
						     bindings/s))
	      ;;; huzzah! we have a match.
	      (return-from estimation-result
		(make-monitor-result 
		 (if (variable-p value/v)
		   (extend-bindings 
		    value/v
		    (measurement-value prev-obs)
		    (first bindings/s))
		   (first bindings/s))
		 (make-interval (measurement-timestamp prev-obs) )))))))

      ;;; we may have exited by now -- instead, we can now look at each measurement in turn.
      (sv-history-for-each 
       history 
       est-interval
       #'(lambda (measurement)
	   (when (meets-persists-constraints-p monitor 
					       measurement
					       bindings/s))
	      ;;; huzzah! we have a match.
	   (return-from estimation-result 
	     (make-monitor-result 
	      (if (variable-p value/v)
		(extend-bindings 
		 value/v
		 (measurement-value measurement)
		 (first bindings/s))
		(first bindings/s))
	      (make-interval (measurement-timestamp measurement))))))
      ;;; alas, we got thru all the measurements in the interval, and failed
      nil)))


(defmethod estimation-result ((type (eql :linear-regression)) (monitor estimation-monitor) (history sv-history) value/v bindings/s)
  (let ((parameters (monitor-estimation-parameters monitor)))
    (let ((minimum-points (max (or (parameter-value :minimum-points parameters) 2) 2)) ;; really need 2 points
	  (maximum-error  (parameter-value :maximum-error parameters))
	  (minimum-frequency-spec (parameter-value :minimum-frequency parameters))
	  (start (patmatch-eval (convert-duration-specs (or (parameter-value :start parameters) 0)) bindings/s))
	  (end (patmatch-eval (convert-duration-specs (or (parameter-value :end parameters) (current-time))) bindings/s)))
      ;; check valid interval
      (when (> start end)
	(return-from estimation-result nil))
      (let ((data-interval (make-interval start end)))
	;; check valid min. frequency
	(when minimum-frequency-spec
	  (let ((dur (duration-read minimum-frequency-spec))
		(last-time nil))
	    (sv-history-for-each history data-interval
				 (lambda (measurement)
				   (when (and last-time
					      (> (- (measurement-timestamp measurement) last-time)
						 dur))
				     ;;(format t "Unfortunately, I must fail~%")
				     (return-from estimation-result nil))
				   (setq last-time (measurement-timestamp measurement))))))
	;; figure out the regression over the data interval ...
	;; first, set the interval
	(set-view-interval history data-interval)
	;; check valid count
	(unless (>= (view-count history) minimum-points)
	    (return-from estimation-result nil))
	  ;; check error, maybe
	  (when maximum-error
	    (when (or (null (view-error history)) (> (view-error history) maximum-error))
	      (return-from estimation-result nil)))
	  ;; ok -- now we find the estimation interval ...
	  (let ((est-interval (close-time-interval
			       (determine-extreme-values 
				(evaluate-constraints (timestamp-constraints monitor) bindings/s)
				(start-of monitor) :closed
				+end-of-time+ :closed)))
		(val-interval (determine-extreme-values
			       (evaluate-constraints (monitor-value-constraints monitor) bindings/s)
			       +neg-infinity+ :closed
			       +pos-infinity+ :closed)))
	    (multiple-value-bind (est-val est-timestamp)
		(estimate-regression-value-within
		 monitor history est-interval val-interval bindings/s)
	      (when (not 
		     (and est-timestamp 
			  est-val
			  (value-constraints-met-p 
			   monitor
			   (non-comparative-constraints (monitor-value-constraints monitor))
			   est-val
			   bindings/s)))
		(return-from estimation-result nil))
		;; huzzah! we have a match
	      (return-from estimation-result
		(make-monitor-result 
		 (if (variable-p value/v)
		   (extend-bindings value/v est-val (first bindings/s))
		   (first bindings/s))
		 (make-interval est-timestamp est-timestamp)))
	      ) ;; end of estimate-value-within
	    ) ;; end of est-interval
	nil))))

;;; 
;;; y'  = m*t1 + b
;;; y'' = m*t2 + b
;;; t'  = (ymin - b)/m ;m != 0
;;; t'' = (ymax - b)/m ;m != 0
(defmethod estimate-regression-value-within 
    ((monitor estimation-monitor) 
     (history sv-history)
     (est-interval interval) 
     (val-interval interval)
     bindings/s)
  (flet ((good-values (timestamp y)
	   ;; because we can't close the interval on y, 
	   ;; we test the 3 cases of t, t+1, and t-1
	   ;; to see if one of the predicted values 
	   ;; of t'/y f(t+1), f(t-1) meets the value
	   ;; constraints. 
	   (when
	    (value-constraints-met-p 
	     monitor 
	     (monitor-value-constraints monitor) y bindings/s)
	    (return-from estimate-regression-value-within 
	      (values y timestamp)))
	   (let* ((t+1 (1+ timestamp))
		  (y+1 (view-predict history t+1)))
	     (when
		 (and 
		  (value-constraints-met-p 
		   monitor 
		   (timestamp-constraints monitor) t+1 bindings/s)
		  (value-constraints-met-p 
		   monitor 
		   (monitor-value-constraints monitor) y+1 bindings/s))
	       (return-from estimate-regression-value-within
		 (values y+1 t+1))))
	   (let* ((t-1 (1- timestamp))
		  (y-1 (view-predict history t-1)))
	     (when
		 (and
		  (value-constraints-met-p 
		   monitor 
		   (timestamp-constraints monitor) t-1 bindings/s)
		  (value-constraints-met-p 
		   monitor 
		   (monitor-value-constraints monitor) y-1 bindings/s))
	       (return-from estimate-regression-value-within
		 (values y-1 t-1))))
	   (values nil nil)))
    (let ((slope (view-slope history))
	  (intercept (view-intercept history)))
      (unless (and slope intercept)
	(return-from estimate-regression-value-within (values nil nil)))
      ;; check y'
      (let ((yp (view-predict history (interval-start est-interval))))
	(when (value-constraints-met-p monitor (monitor-value-constraints monitor) yp bindings/s)
	  (return-from estimate-regression-value-within 
	    (values yp (interval-start est-interval)))))
      ;; check t' and t''
      (when (not (zerop slope))
	(let* ((tp  (view-predict-inverse history (interval-start val-interval)))
	       (tp-ok (value-constraints-met-p monitor
					       (timestamp-constraints monitor)
					       tp
					       bindings/s))
	       (tpp (view-predict-inverse history (interval-end val-interval)))
	       (tpp-ok (value-constraints-met-p monitor
						(timestamp-constraints monitor)
						tpp
						bindings/s)))
	  (cond
	   ((and tp-ok tpp-ok)
	    (if (> tp tpp)
	      (good-values tp (interval-start val-interval))
	      (good-values tpp (interval-end val-interval))))
	   (tp-ok (good-values tp (interval-start val-interval)))
	   (tpp-ok (good-values tpp (interval-start val-interval)))
	   (t (return-from estimate-regression-value-within
		(values nil nil))))))
      ;; failure return
      (values nil nil)
      
      )))




