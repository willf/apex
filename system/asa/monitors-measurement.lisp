;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors-measurement.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors-measurement.lisp,v 1.16 2006/01/15 03:43:00 dalal Exp $
;;; Created:        September. 2004

(in-package :cl-user)

;;;
;;; Measurement monitors are monitors based on specific individual values in the state-value
;;; history *or* values arriving as signals.
;;;
;;; Syntax
;;; 
;;; measurement-monitor-pattern:   (:measurement [<tag>] (<attr> <obj/v> = <val/v>)
;;;                                     [:timestamp <constraint>*]
;;;                                     [:value     <constraint>*]
;;;                                     [:object    <constraint>*]
;;;                                )
;;; tag:   symbol | string
;;; attr:  symbol
;;; obj/v: symbol | variable
;;; val/v: atom | variable
;;;
;;; Note: the presence of an :estimation form indicates an 'estimation' monitor. 
;;; See monitors-estimation.lisp
;;; 



(defclass measurement-monitor (non-complex-monitor probe-mixin) 
  ((timestamp-constraints :initarg :timestamp-constraints :initform NIL :accessor timestamp-constraints)))

(defun measurement-form-range (pattern)
  (sixth pattern))

(defun measurement-form-op (pattern)
  (third pattern))

(defun (setf measurement-form-op) (op pattern)
  (setf (third pattern) op))


(defmethod create-monitor-from-pattern ((type (eql :measurement)) expr parameters pattern)
  (assert (valid-measurement-query-p expr) nil
    "Measurement expression ~s is not a valid measurement form in ~s" expr pattern)
  (let ((monitor 
	 (let ((estimate-params (parameter-value :estimation parameters)))
	   (if (not estimate-params)
	     (make-instance 'measurement-monitor)
	     (make-instance 'estimation-monitor)))))
    (let ((val (measurement-form-value expr))
	  (obj (measurement-form-object expr))
	  (range (measurement-form-range expr)))
      ;; 'state' is special, since task transitions are signalled differently
      (if (eql (first expr) 'state)
	;; just the last value if not a variable
	(if (not (or (variable-p val) (pat-match-special-form-p val)))
	  (setf (relevant-types monitor)
	    (list val (first expr) ))
	  ;; otherwise, need all
	  (setf (relevant-types monitor)
	    (append  +possible-task-states+ (list (first expr)))))
	;; non-state transitions are just the first item
	(setf (relevant-types monitor)
	  (list (first expr))))
      ;; if funky operator, modify monitor
      (ecase (measurement-form-op expr)
	((=)
	 (if (and (consp val)
		  (not (pat-match-special-form-p val)))
	   (progn
	     (setf (measurement-form-value expr)
	       (generate-variable-name (measurement-form-attribute expr)))
	     (add-monitor-constraint :value monitor `(,(measurement-form-op expr) 
						      ,val)))
	   (unless (or range (pat-match-special-form-p val))
	     (add-monitor-constraint :value monitor `(%%eql-if-bound ,val)))))
	((> >= < <=)
	 (setf (measurement-form-value expr)
	   (generate-variable-name (measurement-form-attribute expr)))
	 (add-monitor-constraint :value monitor `(,(measurement-form-op expr) 
						  ,val))
	 (setf (measurement-form-op expr) '=)))

      (add-monitor-constraint :object monitor `(%%eql-if-bound ,obj))
      (maybe-add-range-constraint monitor range val))
    monitor))

(defmethod compile-monitor-post-process ((mon measurement-monitor)) 
  (let ((range (measurement-form-range (expr mon))))
    (when range
      (let ((old (expr mon)))
      (setf (expr mon) 
	`(,(first old)
	  ,(second old)
	  = 
	  ,(generate-variable-name)))))))

(defmethod maybe-add-range-constraint ((mon measurement-monitor) range value)
  (when range
    (add-monitor-constraint :value mon
			    `(<= (+ ,value ,range)))
    (add-monitor-constraint :value mon
			    `(>= (- ,value ,range)))))
  

(defmethod add-monitor-constraint ((type (eql :timestamp)) (monitor measurement-monitor) value)
  (setf (timestamp-constraints monitor)
    (append (timestamp-constraints monitor)
	    (list value))))




(defmethod measurement-results-pipe-bound ((monitor measurement-monitor) 
					   ;; (task task) 
					   (interval interval)
					   (history sv-history)
					   bindings/s
					   val/v)
 ;;(pvs! "Looking for measurement over" interval "in" history :newline "with" (monitor-value-constraints monitor) )
  (map-pipe-filtering 
   (lambda (measurement)
     (let ((val (measurement-value measurement))
	   (ts (measurement-timestamp measurement)))
       (if (and 
	    ;; VALUE constraints met?
	    (value-constraints-met-p 
	     monitor
	     (monitor-value-constraints monitor)
		 val
		 bindings/s)
	    ;; TIMESTAMP constraints met?
	    (value-constraints-met-p 
	     monitor 
	     (timestamp-constraints monitor) 
	     ts bindings/s)
	    ;; If a special form, binding match?
	    (if (pat-match-special-form-p val/v)
	      (pat-match val/v val (first bindings/s))
	      t))
	 ;; if so make a result
	 (make-monitor-result 
	  ;; extend bindings if a variable or special form ...
	  (if (or (variable-p val/v) 
		  (pat-match-special-form-p val/v))
	    (pat-match val/v val (first bindings/s))
	    (first bindings/s))
	  (make-interval ts ts)
	  monitor)
	 ;; if not, fail on this value
	 nil)))
   (sv-measurement-pipe history interval)))

(defmethod measurements-results-pipe ((monitor measurement-monitor)
				      (task task)
				      (interval interval)
				      waitspec
				      bindings/s)
  (let ((attr (substitute-bindings-stack (measurement-form-attribute waitspec) bindings/s))
	(obj/v (substitute-bindings-stack (measurement-form-object waitspec) bindings/s))
	(val/v (substitute-bindings-stack (measurement-form-value waitspec) bindings/s)))
    (if (or (variable-p obj/v)
	    (pat-match-special-form-p obj/v))
	;; we need to check all the objects
      (multi-append-pipes 
       (map-pipe 
	(lambda (object)
	  (if (or (not (value-constraints-met-p 
			monitor
			(monitor-object-constraints monitor)
			object
			bindings/s))
		  (if (pat-match-special-form-p obj/v)
		    (not (pat-match obj/v object (first bindings/s)))
		    nil))
	    empty-pipe
	    (map-pipe 
	     (lambda (result)
	       (setf (binding-set result)
		 (pat-match  
		  obj/v object
		  (binding-set result)))
	       result)
	     (measurement-results-pipe-bound 
	      monitor
	      interval
	      (sv-history (agent-sv-memory (agent task))
			  (make-state-variable attr object))
	      bindings/s
	      val/v))))
	(sv-memory-objects (agent-sv-memory (agent task)) attr)))
      ;; just check this object
      (if (not (value-constraints-met-p 
		monitor
		(monitor-object-constraints monitor)
		obj/v
		bindings/s))
	empty-pipe
	(measurement-results-pipe-bound 
	 monitor
	 interval
	 (sv-history (agent-sv-memory (agent task))
		     (make-state-variable attr obj/v))
	 bindings/s
	 val/v)))))

(defmethod results-from-query ((monitor measurement-monitor) (task task) waitspec timestamp bindings/s)
  (let ((interval
	 (determine-extreme-values 
	  (evaluate-constraints (timestamp-constraints monitor) bindings/s)
	  ;; (monitor-start monitor) :closed
	  (earliest-start-of  task) :closed
	  timestamp :closed))) ;;; !! assumes milliseconds
    (if (not interval) ;;; bad values for some reason ...
      empty-pipe
      (progn 
	(let ((pipe (measurements-results-pipe 
		     monitor
		     task
		     interval
		     waitspec
		     bindings/s)))
	  (unless (eql 'state (car (expr monitor)))
	    (setq pipe (enumerate-pipe pipe))
	    ;; (pvs! :newline ";;;" (expr monitor) :newline ";;;" pipe)
	    ;;(dolist (result pipe)
	    ;;  (pvs! (binding-set result)))
	    )
	  pipe)))))



(defmethod signal-monitor/1 ((monitor measurement-monitor) (task task) (cogevent cogevent) &optional (added-bindings no-bindings))
  (if (not (relevant-event-type-p monitor (first (content cogevent))))
    empty-pipe
    (let* (;; (eventspec (content cogevent))
	   (bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
	   (waitspec (substitute-bindings-stack (monitor-expr monitor) bindings/s))
	   (ts (timestamp cogevent)))
      (results-from-query monitor task waitspec ts bindings/s))))

;;;      (append-pipes 
;;;       ;;; all query-based monitors,
;;;       (results-from-query monitor task waitspec ts bindings/s)
;;;       ;; with a current match
;;;       (let* ((bs (pat-match waitspec eventspec)))
;;;	 ;;(format t "-------~% ~s. ~% time: ~s. waitspec: ~s~% obspec: ~S~% eventspec: ~s~% matched? ~S~%"
;;;	 ;;	(incf cnt) ts waitspec (third waitspec) eventspec (not (null bs)))
;;;	 ;; (pvs! "Result from pat-match" bs)
;;;	 (if bs
;;;	   (let ((val (measurement-form-value eventspec))
;;;		 (obj (measurement-form-object eventspec)))
;;;	     (push-bindings bs bindings/s)
;;;	     (if (and 
;;;		  (value-constraints-met-p monitor (timestamp-constraints monitor) ts bindings/s)
;;;		  (value-constraints-met-p monitor (monitor-value-constraints monitor) val bindings/s)
;;;		  (value-constraints-met-p monitor (monitor-object-constraints monitor) obj bindings/s))
;;;	       (progn
;;;		 (list (make-monitor-result 
;;;			bs
;;;			(make-interval ts ts)
;;;			monitor)))
;;;	       empty-pipe))
;;;	   empty-pipe))))))


(defmethod signal-monitor/1 ((monitor measurement-monitor) (task task) (event (eql :creation))  &optional (added-bindings no-bindings))
  ;; (format t "Signalling at creation ... ~a~% " task)
  (let* ((bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
	 (waitspec (substitute-bindings-stack (monitor-expr monitor) bindings/s))
	 (ts (current-time)))
    (results-from-query monitor task waitspec ts bindings/s)))


