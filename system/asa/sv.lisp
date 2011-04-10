;;;-*- Mode: Lisp; Package:  :apex.asa.sv -*-
;;;
;;; apex/system/asa/sv.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: sv.lisp,v 1.28 2006/01/15 03:43:01 dalal Exp $
;;; Created:        August. 2004

;; Code for state variables and measurements ...

(defpackage :apex.asa.sv
  (:use :common-lisp)
  (:use :apex.utility.datetime)
  (:use :apex.utility.timeseries)
  (:use :inet.util.process)
  (:use :apex.utility.patmatch)
  (:use :apex.utility.pipe)
  (:use :apex.utility.fp)
  (:use :pickle)
  (:export
   ;; state-variables
   #:state-variable
   #:sv-attribute
   #:sv-object
   #:make-state-variable
   #:sv-equalp
   ;; measurements
   #:measurement
   #:make-measurement
   #:measurement-sv
   #:measurement-timestamp   
   #:measurement-value   
   #:measurement-source
   #:measurement-form
   #:measurement-pat-match
   ;; state variable histories
   #:sv-history
   #:sv-histories
   #:sv-history-sv
   #:sv-history-count
   #:sv-history-array
   #:make-sv-history
   #:insert-measurement ;; also defined on memories
   #:latest-measurement
   #:earliest-measurement
   #:first-measurement-within
   #:last-measurement-within
   #:measurement-prior-or-equal
   #:sv-history-for-each
   #:sv-history-map
   #:sv-history-funcall
   #:sv-history-every
   #:sv-history-some   
   #:sv-history-for-each-reverse
   #:sv-history-map-reverse   
   #:sv-measurement-pipe   
   #:sv-history-map-filtering
   ;; state variable memories
   #:sv-memory
   #:sv-memory-table
   #:make-sv-memory
   #:sv-memory-agent
   #:*sv-memory*
   #:with-sv-memory
   ;; #:insert-measurement ;; defined for histories above
   #:sv-memory-fetch-measurements
   #:sv-memory-fetch-bindings
   ;; a method, and a class. sorry, Mike Dalal! #:sv-history
   #:sv-memory-attributes
   #:sv-memory-objects
   ;; checking patterns
   #:check-valid-measurement-pattern
   #:valid-measurement-form-p   
   #:measurement-form-sv
   #:measurement-form-attribute
   #:measurement-form-object
   #:measurement-form-value
   #:valid-sv-form-p   
   #:sv-form-attribute
   #:sv-form-object
   ;; statistics
   #:sv-history-descriptive-statistics
   #:sv-history-interval
   ;;
   #:interval-pipe
   ;; regression -- the new way to do stats
   #:create-lr-state
   #:lr-change-indices
   #:lr-change-interval
   ;; #:start-of #:end-of -- already in cl-user
   #:count-values
   #:sum-values
   #:sum-squares
   #:mean-values
   #:variance-values
   #:stddev-values
   #:min-values
   #:max-values
   #:difference-values
   #:sum-cross-products
   #:lr-error
   #:lr-solution
   #:lr-predict
   #:lr-solution-inverse
   #:lr-predict-inverse
   #:lr-state
   #:lr-slope
   #:lr-intercept
   #:lr-difference
   #:lr-state-sv-history
   ;; logging policies
   #:start-sv-logging-policy
   #:stop-sv-logging-policy
   ;; pickling
   #:pickle-interval

   )
  )

(in-package :apex.asa.sv)


(defclass state-variable ()
  ((attribute :initarg :attribute :accessor sv-attribute)
   (object    :initarg :object     :accessor sv-object)))


(defmethod sv-equalp ((sv1 state-variable) (sv2 state-variable))
  (and (eq (sv-attribute sv1) (sv-attribute sv2))
       (equal (sv-object sv1) (sv-object sv2))))

(defmethod print-object ((sv state-variable) stream)
  (print-unreadable-object (sv stream :type t :identity nil)
    (format stream "(~a ~a)" (sv-attribute sv) (sv-object sv))))

(defun make-state-variable (attribute object)
  ;;(assert (not (variable-p attribute)))
  ;;(assert (not (variable-p object)))
  (make-instance 'state-variable :attribute attribute :object object))

(defclass measurement (cl-user::appob)
  ((state-variable :initarg :state-variable :type state-variable  :accessor measurement-sv)
   (value          :initarg :value                                :accessor measurement-value)
   (form           :initarg :form           :type list            :accessor measurement-form)
   (timestamp      :initarg :timestamp      :type integer         :accessor measurement-timestamp)
   (datetime       :initarg :datetime       :type datetime        :accessor measurement-datetime)
   (source         :initarg :source                               :accessor measurement-source)
   (attributes     :initarg :attributes     :type list            :accessor measurement-attributes)))


(defun make-measurement (state-variable value timestamp &key source (attributes NIL))
  (assert (typep state-variable 'state-variable) nil
    "An measurement must be made on state variables.")
  (assert (integerp timestamp) nil
    "Timestamp must be an integer number of milliseconds.")
  (make-instance 'measurement
    :state-variable state-variable
    :value value
    :form `(,(sv-attribute state-variable) ,(sv-object state-variable) = ,value)
    :timestamp timestamp
    :datetime (milliseconds->datetime timestamp)
    :source source
    :attributes attributes))


(defmethod make-load-form ((obs measurement) &optional e)
  (declare (ignore e))
  `(make-measurement
    ,(pickle-object (measurement-sv obs))
    ,(pickle-object (measurement-value obs))
    ,(pickle-object (measurement-timestamp obs))
    :source ,(pickle-object (measurement-source obs))
    :attributes ,(pickle-object (measurement-attributes obs))))


(defmethod print-object ((obs measurement) stream)
  (print-unreadable-object (obs stream :type t :identity nil)
    (format stream "~a at ~a" 
	    (measurement-form obs)
	    (measurement-timestamp obs))
    ;; (datetime-princ (measurement-datetime obs) stream)
    (when (measurement-source obs)
      (format stream " from ~a" (measurement-source obs)))
    (when (measurement-attributes obs)
      (format stream " ~a" (measurement-attributes obs)))))

;; keep print form in synch with timestamp ...
(defmethod (setf measurement-timestamp) :after (timestamp (obs measurement))
  (setf (measurement-datetime obs) (milliseconds->datetime timestamp)))

;;; use to match a pattern to an measurement. 

(defmethod measurement-pat-match ((pattern list) (obs measurement)  &optional (binding-set no-bindings))
    (assert (cdr pattern) nil
    "Measurement pattern ~a is too short. Must be (attr obj [val] = [timestamp])" pattern)
  (let ((sv (measurement-sv obs))
	(timestamp (measurement-timestamp obs))
	(val (measurement-value obs)))
    (destructuring-bind
	(attrp objp &optional (valp '?) (timep '?)) pattern
      (let ((nb (pat-match-2 attrp (sv-attribute sv) objp (sv-object sv) binding-set)))
	(if (eq nb fail)
	  fail
	  (pat-match-2 valp val timep timestamp nb))))))

(defmethod measurement-pat-match ((obs measurement) (pattern list) &optional (binding-set no-bindings))
  (measurement-pat-match pattern obs binding-set))




  
(defclass sv-history (timeseries cl-user::appob)
  ((state-variable :initarg :state-variable :type state-variable  :accessor sv-history-sv)
   (sv-memory :initarg :sv-memory :initform nil :accessor sv-history-memory))
  (:default-initargs
      :value-key 'measurement-value))

(defmethod cl-user::appob-parent ((history sv-history))
  (sv-history-memory history))


(defmethod sv-history-count ((history sv-history))
  (ts-item-count history))

(defmethod print-object ((history sv-history) stream)
  (print-unreadable-object (history stream :type t :identity nil)
    (let ((sv (sv-history-sv history)))
      (format stream "on (~a ~a) : ~a value~:P." 
	      (sv-attribute sv) (sv-object sv) 
	      (sv-history-count history)
	      (sv-history-count history)))))

(defmethod make-sv-history ((sv state-variable) &optional memory)
  (make-instance 'sv-history
    :state-variable sv
    :sv-memory memory))

(defmethod cl-user::name ((h sv-history))
  (let* ((obj (sv-object (sv-history-sv h)))
	 (nam (if (typep  obj 'cl-user::id-mixin)
		(cl-user::id obj) 
		obj)))
    (format nil "(~a ~a)" 
	    (sv-attribute (sv-history-sv h))
	    nam)))
	  

(defmethod latest-measurement ((history sv-history))
  (let ((count (sv-history-count history)))
    (if (> count 0)
      (aref (ts-values history) (1- count))
      NIL)))

(defmethod earliest-measurement ((history sv-history))
  (let ((count (sv-history-count history)))
    (if (> count 0)
      (aref (ts-values history) 0)
      NIL)))

(defmethod measurement-prior-or-equal ((history sv-history) timestamp)
  (let ((index (find-index-floor history timestamp)))
    (if (>= index 0)
      (aref (ts-values history) index)
      nil)))
	

(defmethod insert-measurement ((history sv-history) (obs measurement) &key (warn-if-out-of-order t))
  (assert (sv-equalp (measurement-sv obs)
	      (sv-history-sv history)) nil
    "Attempting to insert an measurement ~a into the wrong state-variable history." obs)
  (let ((latest-obs (latest-measurement history)))
;;;    (assert (or (null latest-obs)
;;;		(>= (measurement-timestamp obs)
;;;		    (measurement-timestamp latest-obs)))
;;;	nil
;;;      "Attempting to insert an measurement ~a after ~a into a state-variable history out of order [~a/~a]." obs latest-obs (measurement-timestamp obs) 
;;;      (measurement-timestamp latest-obs))
  ;;; check frequency
    (let ((freq (least-restrictive-frequency-limit history)))
      (when  (or (null latest-obs)
		 (null freq)
		 (>= (- (measurement-timestamp obs)
			(measurement-timestamp latest-obs))
		     freq))
	(unless (and (realp freq) (zerop freq))
	;; ok to add
	(timeseries-insert history (measurement-timestamp obs) obs :warn-if-out-of-order warn-if-out-of-order)
	(enforce-ts-logging-policies history)))))
    obs)


(defmethod first-measurement-within ((history sv-history) (interval interval))
  (first-value-within history interval))

(defmethod last-measurement-within ((history sv-history) (interval interval))
  (last-value-within history interval))

(defmethod sv-history-for-each ((history sv-history) (interval interval) (function function))
  (multiple-value-bind (found index)
      (first-measurement-within history interval)
    (when index
      (funcall function found)
      (do ((index (1+ index) (1+ index)))
	  ((or (>= index (ts-item-count history))
	       (> (measurement-timestamp (value-entry-at history index))
		  (interval-end interval))))
	(funcall function (value-entry-at history index))))))
			   
(defmethod sv-history-map ((history sv-history) (interval interval) (function function))
  (let ((results (list)))
    (sv-history-for-each history interval #'(lambda (obs)
					      (push (funcall function obs) 
						    results)))
    (nreverse results)))

(defmethod sv-history-pipe ((history sv-history) (interval interval))
  (labels ((ts-values-pipe (start end array)
	     (if (<= start end)
	       (make-pipe (aref array start)
			  (ts-values-pipe (1+ start) end array))
	       empty-pipe)))
    (multiple-value-bind (ffound findex)
	(first-measurement-within history interval)
      (multiple-value-bind (lfound lindex)
	  (last-measurement-within history interval)
	(if (or (not ffound) (not lfound))
	  empty-pipe
	  (ts-values-pipe findex lindex (ts-values history)))))))

	
    
(defmethod sv-history-funcall ((history sv-history) (interval interval) (function function) bindings)
  (funcall function history interval bindings))

(defmethod sv-history-every ((history sv-history) (interval interval) (function function))
  (sv-history-for-each
   history 
   interval
   (lambda (obs)
     (unless (funcall function obs)
       (return-from sv-history-every NIL))))
  T)
  
(defmethod sv-history-some ((history sv-history) (interval interval) (function function))
  (sv-history-for-each
   history 
   interval
   (lambda (obs)
     (when (funcall function obs)
       (return-from sv-history-some obs))))
  NIL)

(defmethod sv-history-for-each-reverse ((history sv-history) (interval interval) (function function))
  (multiple-value-bind (found index)
      (last-measurement-within history interval)
    (when index
      (funcall function found)
      (do ((index (1- index) (1- index)))
	  ((or (< index 0)
	       (< (measurement-timestamp (value-entry-at history index))
		  (interval-start interval))))
	(funcall function (value-entry-at history index))))))

(defmethod sv-history-map-reverse ((history sv-history) (interval interval) (function function))
  (let ((results (list)))
    (sv-history-for-each-reverse history interval #'(lambda (obs)
					      (push (funcall function obs) 
						    results)))
    (nreverse results)))


;; return a pipe of measurements between these values
;;; this is wrong, because there might be multiple values at the same
;;; instant.
;;;(defmethod sv-measurement-pipe ((history sv-history) (interval interval))
;;;  (let ((first (first-measurement-within history interval)))
;;;    (if first
;;;      (if (> (1+ (measurement-timestamp first)) (interval-end interval))
;;;	(list first)
;;;	(make-pipe first (sv-measurement-pipe history (make-interval (1+ (measurement-timestamp first)) (interval-end interval)))))
;;;      nil)))

(defmethod sv-measurement-pipe ((history sv-history) (interval interval))
  (sv-history-pipe history interval))

    

(defmethod sv-history-map-filtering ((history sv-history) (interval interval) (function function) (filter function))
  (let ((results (list)))
    (sv-history-for-each history interval #'(lambda (obs)
					      (let ((result (funcall function obs)))
						(when (funcall filter result)
						  (push result results)))))
    (nreverse results)))

(defclass sv-memory (cl-user::appob)
  ((table :initform (make-hash-table) :accessor sv-memory-table)
   (agent :initform NIL :accessor sv-memory-agent)))

(defmethod cl-user::appob-parent ((mem sv-memory))
    (sv-memory-agent mem))

(defmethod cl-user::appob-children ((mem sv-memory))
  (sv-histories mem))

(defmethod sv-histories ((mem sv-memory))
  (loop for entry being the hash-value in (sv-memory-table mem) 
		     appending entry))

(defun make-sv-memory ()
  (let ((mem (make-instance 'sv-memory :name "State Variable Memory")))
    mem))

(defmethod force-sv-mem-entry ((mem sv-memory) (key symbol))
  (or (gethash key (sv-memory-table mem))
      (setf (gethash key (sv-memory-table mem))
	(list)))) 

(defmethod force-sv-history ((mem sv-memory) (sv state-variable))
  (let ((entry (force-sv-mem-entry mem (sv-attribute sv))))
    (or (find-if #'(lambda (history) 
		     (sv-equalp sv (sv-history-sv history)))
		 entry)
	(progn
	  (let ((news (push (make-sv-history sv mem)
			    (gethash (sv-attribute sv) (sv-memory-table mem)))))
	    (car news))))))


(defmethod insert-measurement ((mem sv-memory) (obs measurement)  &key (warn-if-out-of-order t))
  (insert-measurement (force-sv-history mem (measurement-sv obs)) obs :warn-if-out-of-order warn-if-out-of-order))

(defmethod add-ts-logging-policy-from-sv ((mem sv-memory) (sv state-variable) (policy ts-logging-policy))
  (add-ts-logging-policy (force-sv-history mem sv) policy))

(defmethod start-sv-logging-policy ((mem sv-memory) (sv state-variable) &key frequency-limit count-limit time-limit)
  (add-ts-logging-policy-from-sv 
   mem sv 
   (make-ts-logging-policy
    :frequency-limit (if frequency-limit (duration-read frequency-limit) NIL)
    :count-limit count-limit 
    :time-limit (if time-limit (duration-read time-limit) NIL))))

(defmethod start-sv-logging-policy ((mem sv-memory) (sv list) &key frequency-limit count-limit time-limit)
  (let ((attr (car sv))
	(obj (second sv)))
  (if (variable-p obj)
    (dolist (object (sv-memory-objects mem attr))
      (start-sv-logging-policy mem  (make-state-variable attr object)
			   :frequency-limit frequency-limit
			   :count-limit count-limit 
			   :time-limit time-limit
			   ))
    (start-sv-logging-policy mem  (apply #'make-state-variable sv) 
			     :frequency-limit frequency-limit
			     :count-limit count-limit 
			     :time-limit time-limit
			     ))))

(defmethod stop-sv-logging-policy ((mem sv-memory) (sv state-variable))
  (pop-ts-logging-policy (force-sv-history mem sv)))

(defmethod stop-sv-logging-policy ((mem sv-memory) (sv list))
  (let ((attr (car sv))
	(obj (second sv)))
    (if (variable-p obj)
      (dolist (object (sv-memory-objects mem attr))
	(stop-sv-logging-policy mem (make-state-variable attr object)))
      (stop-sv-logging-policy mem (apply #'make-state-variable sv)))))

;;; ------------------------------------------------------------------------
;;; retrieval with base query  ... (attr obj [val] [timestamp]) [interval]
;;; ------------------------------------------------------------------------

(defmethod sv-history ((mem sv-memory) (sv state-variable))
  (force-sv-history  mem sv))

(defmethod sv-history-of ((mem sv-memory) (attribute symbol) object)
  (let ((mem-entry (gethash attribute (sv-memory-table mem))))
    (find-if #'(lambda (history)
	       (equal object (sv-object (sv-history-sv history))))
	     mem-entry)))

(defmethod sv-memory-objects ((mem sv-memory) (attribute symbol))
  ;; return all of the objects keyed from attribute
  (mapcar #'(lambda (history)
	    (sv-object (sv-history-sv history)))
	  (gethash attribute (sv-memory-table mem))))

(defmethod sv-memory-attributes ((mem sv-memory))
  (loop for key being the hash-key in (sv-memory-table mem)
      collecting key))


(defun pat-match-2 (pattern1 input1 pattern2 input2 &optional (binding-set no-bindings))
  (let ((bs1 (pat-match pattern1 input1 binding-set)))
    (if (eq bs1 fail)
      fail
      (pat-match pattern2 input2 bs1))))


;;; SV-MEMORY-FETCH-BINDINGS
;;; Fetch a pipe of bindings matching a state-variable query pattern.
;;; 
;;;; this allows any and all of attr-key obj-key val-key timestamp-key to be variables.
;;; pattern matching is done at the 'base' level for the value key and the timestamp key
;;; otherwise, a recursive call is  made for object and attribute keys.
;;; the history is drawn only over the interval.

(defmethod sv-memory-fetch-bindings ((mem sv-memory) (leaf-query list) binding-set (interval interval))
  (let ((attr-key (measurement-form-attribute leaf-query))
	(obj-key (measurement-form-object leaf-query))
	(val-key (or (measurement-form-value leaf-query) `?))
	(timestamp-key '?))
    (sv-memory-fetch-bindings-c mem attr-key obj-key val-key timestamp-key binding-set interval)))

(defmethod sv-memory-fetch-bindings-c ((mem sv-memory) attr-key obj-key val-key timestamp-key binding-set (interval interval))
  (let ((attr-key-is-variable (variable-p attr-key))
	(obj-key-is-variable (variable-p obj-key)))
  (cond
   ((and (not attr-key-is-variable)
	 (not obj-key-is-variable))
    ;;; just go through history and match ...
    (let ((history (sv-history-of mem attr-key obj-key)))
      (if (null history)
	fail
	(map-pipe-filtering ;; this doesn't assume FAIL == NIL, but it could :)
	 #'(lambda (match)
	     (and (not (eq match fail)) match))
	 (sv-history-map 
	  history
	  interval
	  #'(lambda (measurement)
	      (pat-match-2  val-key (measurement-value measurement) timestamp-key (measurement-timestamp measurement) binding-set)))))))
   ((and obj-key-is-variable (not attr-key-is-variable))
    ;;; ok, extend binding set from each atomic value
    (mappend-pipe-filtering ;; this assumes FAIL == NIL
     #'(lambda (obj)
	 (map-pipe #'(lambda (bs)
		       (extend-bindings obj-key obj bs))
		   (sv-memory-fetch-bindings-c mem attr-key obj val-key timestamp-key binding-set interval)))
     (sv-memory-objects mem attr-key)))
   (attr-key-is-variable
    (mappend-pipe-filtering ;; this assumes FAIL == NIL
     #'(lambda (attr)
	 (map-pipe #'(lambda (bs)
		       (extend-bindings attr-key attr bs))
		   (sv-memory-fetch-bindings-c mem attr obj-key val-key timestamp-key binding-set interval)))
     (sv-memory-attributes mem)))
   )))


;;; SV-MEMORY-FETCH-MEASUREMENTS
;;; Fetch a pipe of measurements matching a state-variable query pattern.
;;; 
;;; this allows any and all of attr-key obj-key val-key timestamp-key to be variables.
;;; pattern matching is done at the 'base' level for the value key and the timestamp key
;;; otherwise, a recursive call is made for object and attribute keys.
;;; the history is drawn only over the interval.

(defmethod sv-memory-fetch-measurements ((mem sv-memory) (leaf-query list) binding-set (interval interval))
  (let ((attr-key (measurement-form-attribute leaf-query))
	(obj-key (measurement-form-object leaf-query))
	(val-key (or (measurement-form-value leaf-query) `?))
	(timestamp-key '?))
    (sv-memory-fetch-measurements-c mem attr-key obj-key val-key timestamp-key binding-set interval)))

(defmethod sv-memory-fetch-measurements-c ((mem sv-memory) attr-key obj-key val-key timestamp-key binding-set (interval interval))
  (let ((attr-key-is-variable (variable-p attr-key))
	(obj-key-is-variable (variable-p obj-key)))
  (cond
   ((and (not attr-key-is-variable)
	 (not obj-key-is-variable))
    ;;; just go through history and match ...
    (let ((history (sv-history-of mem attr-key obj-key)))
      (if (null history)
	'()
	(map-pipe-filtering 
	 #'identity
	 (sv-history-map 
	  history
	  interval
	  #'(lambda (measurement)
	      (if (pat-match-2  val-key (measurement-value measurement) 
				timestamp-key (measurement-timestamp measurement) binding-set)
		measurement
		nil)))))))
   ((and obj-key-is-variable (not attr-key-is-variable))
    ;;; ok, extend binding set from each atomic value
    (mappend-pipe-filtering
     #'(lambda (obj)
	 (sv-memory-fetch-measurements-c mem attr-key obj val-key timestamp-key binding-set interval))
     (sv-memory-objects mem attr-key)))
   (attr-key-is-variable
    (mappend-pipe-filtering 
     #'(lambda (attr)
	 (sv-memory-fetch-measurements-c mem attr obj-key val-key timestamp-key binding-set interval))
     (sv-memory-attributes mem)))
   )))

;;(defvar *sv-memory* (make-sv-memory))

;;(defmacro with-sv-memory ((memory) &body body)
;;  `(let ((*sv-memory* ,memory))
;;    (declare (special *sv-memory*))
;;     ,@body))

;;(defun sv-memory () *sv-memory* )


(defun check-valid-measurement-pattern (pattern)
  (let ((bs (pat-match '(?attr ?obj = ?value) pattern)))
    (assert (not (eq bs fail)) nil
      "Invalid measurement pattern ~s; must be (<attr> <obj> = <value>)" pattern)
    (let ((obj-val (lookup '?obj bs))
	  (attr-val (lookup '?attr bs))
	  (val-val (lookup '?value bs)))
      (assert (and (symbolp attr-val)
		   (not (variable-p attr-val)))
	  nil
	"The state variable attribute ~s must be a non-variable symbol in ~s" attr-val pattern)
      (let ((objt (if (variable-p obj-val) (eq (variable-type obj-val) :free) NIL))
	    (valt (if (variable-p val-val) (eq (variable-type val-val) :free) NIL)))
	(assert (not (and objt valt)) nil
	  "Both ~a and ~a in ~a may not be free variables." obj-val val-val pattern)))
    pattern
  ))

(defun valid-measurement-form-p (pattern)
  (let ((bs (pat-match '(?attr ?obj = ?value) pattern)))
    (if (eq bs fail)
      nil
      (and (lookup '?attr bs)
	   (lookup '?obj bs)))))

;;; assumes valid pattern, fully bound 
(defun measurement-form-sv (pattern) 
  (make-state-variable (car pattern) (cadr pattern)))

(defun measurement-form-attribute (pattern)
  (car pattern))

(defun measurement-form-object (pattern)
  (cadr pattern))

(defun measurement-form-value (pattern)
  (fourth pattern))

(defun (setf measurement-form-value) (val pattern)
  (setf (fourth pattern) val))

(defun valid-sv-form-p (pattern)
  (let ((bs (pat-match '(?attr ?obj) pattern)))
    (if (eq bs fail)
      nil
      (and (symbolp (lookup '?attr bs))
	   (symbolp (lookup '?obj bs))))))

(defun sv-form-attribute (pattern)
  (car pattern))

(defun sv-form-object (pattern)
  (cadr pattern))

;; if all the values in the history ore real, then return:
;;; mean count sum standard-deviation variance minimum-value maximum-value
;;; otherwise:
;;; mode count nil nil nil first-value last-value
;;; if there are multiple modes, it only returns the first
;;; if there are no values:
;;; returns nil
;;;(defmethod sv-history-descriptive-statistics ((history sv-history) (interval interval) &optional constraints)
;;;  (let ((cnt 0)
;;;	(sum 0)
;;;	(max nil)
;;;	(min nil)
;;;	(numericp T)
;;;	(cnt-table (make-hash-table))
;;;	)
;;;    (sv-history-for-each 
;;;     history interval
;;;     (lambda (measurement)
;;;       (let ((val (measurement-value measurement)))
;;;	 (when (every (lambda (constraint)
;;;			(patmatch-constraint val constraint))
;;;		      constraints)
;;;	   (incf cnt)
;;;	   (if (null min) (setq min val) (setq min (sv-min min val)))
;;;	   (if (null max) (setq max val) (setq max (sv-max max val)))
;;;	   (if (and numericp (realp val))
;;;	     (incf sum val)
;;;	     (setf numericp nil))
;;;	   (let ((v (gethash val cnt-table)))
;;;	     (if v (incf (gethash val cnt-table))
;;;		 (setf (gethash val cnt-table) 1)))))))
;;;    (if (= cnt 0)
;;;      (values 0 0 0 nil nil nil nil)
;;;      (if numericp 
;;;	(let ((mean (/ sum cnt))
;;;	      (errsum 0))
;;;	  (sv-history-for-each 
;;;	   history interval
;;;	   (lambda (measurement)
;;;	     (let*  ((val (measurement-value measurement))
;;;		     (diff (- val mean)))
;;;	       (incf errsum (* diff diff))
;;;	       )))
;;;	  (let* ((variance (/ errsum cnt))
;;;		 (stddev (sqrt variance)))
;;;	    (values mean cnt sum stddev variance min max)))
;;;	(let ((mean-cnt 0)
;;;	      (mean nil))
;;;	  (maphash (lambda (key value)
;;;		     (when (> value mean-cnt)
;;;		       (setq mean key)))
;;;		   cnt-table)
;;;	  (values mean cnt nil nil nil min max))))))

;;;;;; arithmetic min ...
;;;(defmethod sv-min ((a real) (b real))
;;;  (min a b))
;;;
;;;;;; previous ...
;;;(defmethod sv-min ((a T) (b T))
;;;  a)
;;;
;;;(defmethod sv-max ((a real) (b real))
;;;  (max a b))
;;;
;;;(defmethod sv-max ((a T) (b T))
;;;  b)


(defmethod sv-history-interval ((history sv-history) (interval interval))
  "Returns an interval starting at the 1st measurement w/in interval, and ending at the last"
  (let ((first (first-measurement-within history interval))
	(last (last-measurement-within history interval)))
    (if (and first last)
      (make-interval (measurement-timestamp first)
		     (measurement-timestamp last))
      nil)))


(defmethod make-load-form ((sv state-variable) &optional e) 
  (declare (ignore e))
  `(make-state-variable ,(pickle-object (sv-attribute sv)) ,(pickle-object (sv-object sv))))


(defmethod pickle-interval ((history sv-history) (interval interval) path)
  (pickling 
   (path *package*)
   (sv-history-for-each 
    history interval
    (lambda (item)
      (pickle-object item)))))

(defmethod pickle/spill-before ((history sv-history) timepoint path)
  (pickle-interval history (make-interval 0 timepoint) path)
  (timeseries-delete-floor history timepoint)
  path)
			
       

;;; --- end of file
