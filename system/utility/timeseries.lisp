;;;-*- Mode: Lisp; Package: :apex.utility.timeseries -*-
;;;
;;; apex/system/utility/timeseries.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: timeseries.lisp,v 1.10 2006/01/15 03:43:03 dalal Exp $
;;; Created:        November. 2004

;;;
;;; -- things to do time series analysis with. In particular, we
;;;    we try to be very efficient in calculating timeseries interpolations
;;;    even when we change the interval often.
;;;    To create a timeseries:
;;;    - use the MAKE-TIMESERIES function, and add data using
;;;      INSERT-TIMESERIES-DATA
;;;
;;;    - to delete *earlier* data, use TIMESERIES-DELETE-FLOOR
;;;
;;;    - to query, using FIND-INDEX and related functions.
;;;
;;;    - to set a 'view' for timeseries interpolation, use
;;;      SET-VIEW-INTERVAL. Then, use the VIEW-x methods
;;;      for querying the value. Be aware, though, that these might return
;;;      NIL, if there are no data points in the view interval.
;;;
;;; --------------------------------------------------------------------------
;;;
;;;
;;; MAKE-TIMESERIES ()                                               [FUNCTION]
;;;    Make a timeseries
;;;
;;; TIMESERIES-INSERT ((ts timeseries) timestamp value)                [METHOD]
;;;    Insert a timestamp/value pair into a timeseries. 
;;;
;;; TS-TIMEPOINTS ((ts timeseries))                                    [METHOD]
;;;    Returns the timepoint array
;;;
;;; TS-VALUES ((ts timeseries))                                        [METHOD]
;;;    Returns the values array
;;; 
;;; TS-ITEM-COUNT ((ts timeseries))                                    [METHOD]
;;;    Returns the number of timepoint/value pairs
;;;
;;; TS-VIEW-INTERVAL ((ts timeseries))                                 [METHOD]
;;;    Returns the current view interval.
;;;
;;; TIMESERIES-DELETE-FLOOR ((ts timeseries) timestamp)                [METHOD]
;;;    Delete everything at or before this timestamp
;;;
;;; TIMESERIES-DELETE-MAX-COUNT ((ts timeseries) max-count)            [METHOD]
;;;    Keep only MAX-COUNT last entries
;;;
;;; TIMEPOINT-AT ((ts timeseries) index)                               [METHOD]
;;;    Given an index, what timepoint is there?
;;;
;;; VALUE-AT ((ts timeseries) index)                                   [METHOD]
;;;    Value at an index
;;;
;;; FIND-INDEX ((ts timeseries) timestamp)                             [METHOD]
;;;    Find the index at this timestamp; return -1 if not found.
;;;
;;; FIND-INDEX-CEILING ((ts timeseries) timepoint)                     [METHOD]
;;;    Find the index at or above this timpoint; returns length of
;;;    timeseries if not found 
;;;
;;; FIND-INDEX-FLOOR ((ts timeseries) timepoint)                       [METHOD]
;;;    Find the index at or below this timepoint; returns -1 if not found.
;;;
;;; TIMESERIES-BEGINNING ((ts timeseries))                             [METHOD]
;;;
;;; TIMESERIES-END ((ts timeseries))                                   [METHOD]
;;;
;;; RESET-VIEW ((ts timeseries))                                       [METHOD]
;;;
;;; SET-VIEW-INTERVAL ((ts timeseries) (interval interval))         [METHOD]
;;;
;;; VIEW-SLOPE ((ts timeseries))                                       [METHOD]
;;;    linear regression slope of time series, or nil if not enough points
;;;
;;; VIEW-INTERCEPT ((ts timeseries))                                   [METHOD]
;;;    linear regression intercept of time series, or nil if not enough
;;;    points 
;;;
;;; VIEW-MIN ((ts timeseries))                                         [METHOD]
;;;    Minimum value (or nil if no values) in current view.
;;;
;;; VIEW-MAX ((ts timeseries))                                         [METHOD]
;;;    Maximum value (or nil if no values) in current view.
;;;
;;; VIEW-ERROR ((ts timeseries))                                       [METHOD]
;;;
;;; VIEW-DIFFERENCE ((ts timeseries))                                  [METHOD]
;;;
;;; VIEW-SUM ((ts timeseries))                                         [METHOD]
;;;
;;; VIEW-SUM-SQUARES ((ts timeseries))                                 [METHOD]
;;;
;;; VIEW-MEAN ((ts timeseries))                                        [METHOD]
;;;
;;; VIEW-VARIANCE ((ts timeseries))                                    [METHOD]
;;;    Returns the variance of y values, or 0 if there are no values
;;;
;;; VIEW-STDDEV ((ts timeseries))                                      [METHOD]
;;;
;;; VIEW-COUNT ((ts timeseries))                                       [METHOD]
;;;
;;; FIRST-VALUE-WITHIN ((ts timeseries) (interval interval))           [METHOD]
;;;   first value within interval; returns value + index (or nil)
;;;
;;; LAST-VALUE-WITHIN ((ts timeseries) (interval interval))            [METHOD]
;;;   last value within interval; returns value + index (or nil)

(defpackage  :apex.utility.timeseries
  (:use :common-lisp)
  (:use :apex.utility.datetime) ;; for interval
  (:use :pickle)
  (:export
   #:timeseries
   #:make-timeseries
   #:ts-timepoints
   #:ts-values
   #:ts-item-count
   #:ts-view-interval
   #:timepoint-at
   #:value-at
   #:value-entry-at   
   #:timeseries-insert
   #:timeseries-delete-floor
   #:timeseries-delete-max-count   
   #:find-index
   #:find-index-ceiling
   #:find-index-floor
   #:timeseries-beginning
   #:timeseries-end
   #:set-view-interval
   #:view-interval
   #:view-slope
   #:view-intercept   
   #:view-min
   #:view-max
   #:view-error
   #:view-difference
   #:view-sum
   #:view-sum-squares
   #:view-mean
   #:view-variance
   #:view-stddev
   #:view-count
   #:first-value-within
   #:last-value-within
   #:view-predict
   #:view-predict-inverse
   #:view-predict-function
   #:view-predict-inverse-function
   ;; logging policies
   #:make-ts-logging-policy
   #:ts-logging-policy
   #:add-ts-logging-policy
   #:pop-ts-logging-policy
   #:ts-logging-policies
   #:least-restrictive-frequency-limit
   #:enforce-ts-logging-policies
   ;; filtering
   #:display-filtered-values
   #:filter-timeseries
   #:timeseries-for-each/filtered 
   #:timeseries-for-each
   ;; continuation
   #:timeseries-continuation
   )
  )
(in-package :apex.utility.timeseries)

(defclass timeseries ()
  ((timepoints :initform (make-array 0 :adjustable t :fill-pointer 0)
	       :accessor ts-timepoints)
   (values     :initform (make-array 0 :adjustable t :fill-pointer 0)
	       :accessor ts-values)
   (bitv 
    :initform
    (make-array 0 :adjustable t :fill-pointer 0)
    :accessor ts-bitv)
   (logging-policies :initarg :logging-policies :initform (list) :accessor ts-logging-policies)
   (value-key  
    :initform #'identity 
    :initarg :value-key
    :accessor value-key
    :documentation "A function, run on a value entry, returning a value to use in calcuations.")
   (item-count 
    :initform 0
    :initarg :item-count
    :type fixnum
    :accessor ts-item-count
    :documentation "Number of items in the (real) timeseries array")
   (view-interval
    :initform NIL
    :initarg :view-interval
    :accessor view-interval
    :documentation "Interval associated with a view on the time series")
   (view-starti 
    :initform 0
    :initarg :view-starti
    :accessor view-starti
    :documentation "Start index of a view on time series")
   (view-endi
    :initform -1
    :initarg :view-endi
    :accessor view-endi
    :documentation "End index of a view on time series")
   (view-sum-x
    :initform 0
    :initarg :view-sum-x
    :accessor view-sum-x
    :documentation "Sum of timestamps")
   (view-sum-y
    :initform 0
    :initarg :view-sum-y
    :accessor view-sum-y
    :documentation "Sum of values")
   (view-sum-xx
    :initform 0
    :initarg :view-sum-xx
    :accessor view-sum-xx
    :documentation "Sum of timestamps^2")
   (view-sum-yy
    :initform 0
    :initarg :view-sum-yy
    :accessor view-sum-yy
    :documentation "Sum of values^2")
   (view-sum-xy
    :initform 0
    :initarg :view-sum-xy
    :accessor view-sum-xy
    :documentation "Sum of timestamps*values")
   (view-slope-cached
    :initform NIL
    :initarg :view-slope-cached
    :accessor view-slope-cached
    :type (or null real)
    :documentation "Cached linear regression slope")
   (view-intercept-cached
    :initform NIL
    :initarg :view-intercept-cached
    :accessor view-intercept-cached
    :type (or null real)
    :documentation "Cached linear regression intercept") 
   (view-error-cached
    :initform NIL
    :initarg :view-error-cached
    :accessor view-error-cached
    :type (or null real)
    :documentation "Cached linear regression error")
   (view-max-cached
    :initform NIL
    :initarg :view-max-cached
    :accessor view-max-cached
    :type (or null real)
    :documentation "Cached linear regression max")   
   (view-min-cached
    :initform nil
    :initarg :view-min-cached
    :accessor view-min-cached
    :type (or null real)
    :documentation "Cached linear regression min")  
   (view-count-cached
    :initform 0
    :initarg :view-count-cached
    :accessor view-count-cached
    :type (or null fixnum)
    :documentation "Cached numer of values")
   (view-timepoint-offset
    :initform NIL
    :initarg :view-timepoint-offset
    :accessor view-timepoint-offset
    :documentation "Make timestamp calculations using smaller numbers")
   (cl-user::suppressed-slots
    :reader cl-user::suppressed-slots
    :allocation :class
    :initform '(view-interval
		view-starti view-endi view-sum-x view-sum-y view-sum-xx view-sum-yy
		view-sum-xy view-slope-cached view-intercept-cached view-error-cached
		view-max-cached view-min-cached view-count-cached view-timepoint-offset))
   ))

(defun make-timeseries (&optional value-key)
  "Make a timeseries"
  (if value-key
    (make-instance 'timeseries :value-key value-key)
    (make-instance 'timeseries)))


(defmethod timepoint-at ((ts timeseries) index)
  "Given an index, what timepoint is there?"
  (aref (ts-timepoints ts) index))

(defmethod (setf timepoint-at) (val (ts timeseries) index)
  "Set the timepoint at an index (non-public)"
  (setf (aref (ts-timepoints ts) index) val))

(defmethod value-entry-at ((ts timeseries) index)
  "Value at an index"
  (aref (ts-values ts) index))

(defmethod (setf value-entry-at) (val (ts timeseries) index)
  "Set value at an index (non-public)"
  (setf (aref (ts-values ts) index) val))

(defmethod ts-bit-at ((ts timeseries) index)
  "Given an index, what is the bitv value there?"
  (aref (ts-bitv ts) index))

(defmethod (setf ts-bit-at) (val (ts timeseries) index)
  "Set the keep bit at an index (non-public)"
  (setf (aref (ts-bitv ts) index) val))

(defmethod value-at ((ts timeseries) index)
  "Value at an index"
  (funcall (value-key ts) (aref (ts-values ts) index)))


(defmethod bubble-down ((ts timeseries))
  (loop for i from (1- (ts-item-count ts)) downto 1 do
	(if (< (timepoint-at ts i)
	       (timepoint-at ts (1- i)))
	  (progn
	    (rotatef (timepoint-at ts i)
		     (timepoint-at ts (1- i)))
	    (rotatef (value-entry-at ts i)
		     (value-entry-at ts (1- i)))
	    (rotatef (ts-bit-at ts i)
		     (ts-bit-at ts (1- i)))
	    )
	  (return-from bubble-down))))

(defmethod timeseries-insert ((ts timeseries) timestamp value &key (filter-in-p t) (warn-if-out-of-order t))
  "Insert a timestamp/value pair into a timeseries. Only public method"
  (let ((needs-sorting nil))
    (when (and warn-if-out-of-order 
	       (> (ts-item-count ts) 0)
	       (< timestamp (timepoint-at ts (1- (ts-item-count ts)))))
      (warn "Adding data before end of timeseries. Last time/value: ~a ~a; Added time/value: ~a ~a"
	    (timepoint-at ts (1- (ts-item-count ts)))
	    (value-at ts (1- (ts-item-count ts)))
	    timestamp
	    value)
      (setq needs-sorting T))
  (unless (view-timepoint-offset ts)
    (setf (view-timepoint-offset ts) timestamp))
  (vector-push-extend timestamp  (ts-timepoints ts))
  (vector-push-extend value (ts-values ts))
  (vector-push-extend filter-in-p (ts-bitv ts))
  (incf (ts-item-count ts))
  (when needs-sorting (bubble-down ts))
  (values)))


    
(defmethod delete-to-index ((ts timeseries) index) 
  ;;(cl-user::pvs! index  (length (ts-values ts)) (ts-item-count ts) (- (ts-item-count ts) index 1))
  (when (>= index 0)
    ;; move data 
    (loop for i from 0 to (- (ts-item-count ts) index 2)
	doing
	  ;;(cl-user::pvs! "move" (+ index i 1) "to" i)
	  (setf (timepoint-at ts i)
	    (timepoint-at ts (+ index i 1)))
	  (setf (value-entry-at ts i)
	    (value-entry-at ts (+ index i 1))) )
    ;;(cl-user::pvs! "done updating values.")
    ;; update pointers
    (let ((fp (max 0 (- (ts-item-count ts) index 1))))
      (setf (fill-pointer (ts-timepoints ts)) fp)
      (setf (fill-pointer (ts-values ts)) fp)
      (setf (ts-item-count ts) fp)
      )
    ;; take care of internal state for slope/intercept/etc.
    (when (view-interval ts)
      (reset-view ts)
      (set-view-interval ts (view-interval ts)))
    )
  ts)

(defmethod timeseries-delete-floor ((ts timeseries) timestamp)
  "Delete everything at or before this timestamp"
  (let ((index (find-index-floor ts timestamp)))
    (delete-to-index ts index)
    ts))

(defmethod timeseries-delete-max-count ((ts timeseries) max-count)
  "Keep only MAX-COUNT last entries"
  (let ((index (1- (- (ts-item-count ts) max-count ))))
    (delete-to-index ts index))
  ts)
		  
      

(defmethod find-index-ceiling ((ts timeseries) timepoint)
  "Find the index at or above this timpoint; returns length of timeseries if not found"
  (let ((high (ts-item-count ts))
	(low -1)
	(probe 0))
    (declare (fixnum high low probe))
    (loop while (> (- high low) 1)
	do
	  (setq probe  (the fixnum (floor (+ high low) 2)))
	  (if (>= (timepoint-at ts probe) timepoint)
	    (setq high probe)
	    (setq low probe)))
    high))

(defmethod find-index-floor ((ts timeseries) timepoint)
  "Find the index at or below this timepoint; returns -1 if not found."
    (let ((high (ts-item-count ts))
	  (low -1)
	  (probe 0))
      (declare (fixnum high low probe))
      (loop while (> (- high low) 1)
	  do
	    (setq probe  (the fixnum (floor (+ high low) 2)))
	    (if (<= (timepoint-at ts probe) timepoint)
	      (setq low probe)
	      (setq high probe)))
      low))

(defmethod find-index ((ts timeseries) timestamp)
  "Find the index at this timestamp; return -1 if not found."
  (let ((high (ts-item-count ts))
	  (low -1)
	  (probe 0))
      (declare (fixnum high low probe))
      (loop while (> (- high low) 1)
	  do
	    (setq probe  (the fixnum (floor (+ high low) 2)))
	    (if (< (timepoint-at ts probe) timestamp)
	      (setq low probe)
	      (setq high probe)))
      (if (or (= high (ts-item-count ts))
	      (/= (timepoint-at ts high) timestamp))
	-1
	high)))


;;; ---
;;; especially dealing with internal state values. The idea is that
;;; we want to make as few computations as possible wrt to sums, means,
;;; slope, etc. from time series. so, we have to do a lot of bookkeeping.
;;; ---

;;; clear all the cached values
(defmethod clear-view-caches ((ts timeseries))
  (setf 
      (view-slope-cached ts) nil
      (view-intercept-cached ts) nil
      (view-error-cached ts) nil
      (view-max-cached ts) nil
      (view-min-cached ts) nil
      (view-count-cached ts) nil
      )
  (values))

;;; this shouldn't be called ...
(defmethod recalculate-state ((ts timeseries))
  (clear-view-caches ts)
  (setf (view-sum-x ts) 0
	(view-sum-y ts) 0
	(view-sum-xx ts) 0
	(view-sum-xy ts) 0
	(view-sum-yy ts) 0
	(view-sum-xy ts) 0)
  (loop for index from (view-starti ts) to (view-endi ts) doing
	(increment-state-at ts index))
  (values))

;;; update the internal state by decrementing appropriate values
;;; note -- does *not* clear caches.
(defmethod decrement-state-at ((ts timeseries) (index fixnum))
  (when (> index -1)
    (let ((x (- (timepoint-at ts index) (view-timepoint-offset ts)))
	  (y (value-at ts index)))
      ;; (cl-user::pvs! "Decrementing" index x y (realp y))
      (when (realp y)
	(decf (view-sum-x ts) x)
	(decf (view-sum-y ts) y)
	(decf (view-sum-xx ts) (* x x))
	(decf (view-sum-yy ts) (* y y))
	(decf (view-sum-xy ts) (* x y)))))
  (values))

;;; update the internal state by incrementing appropriate values
;;; note -- does *not* clear caches.
(defmethod increment-state-at ((ts timeseries) (index fixnum))
  (when (> index -1)
    (let ((x (- (timepoint-at ts index) (view-timepoint-offset ts)))
	  (y (value-at ts index)))
      ;; (cl-user::pvs! "Incrementing" index x y (realp y))
      (when (realp y)
	(incf (view-sum-x ts) x)
	(incf (view-sum-y ts) y)
	(incf (view-sum-xx ts) (* x x))
	(incf (view-sum-yy ts) (* y y))
	(incf (view-sum-xy ts) (* x y)))))
  (values))

(defmethod timeseries-beginning ((ts timeseries))
  (if (> (ts-item-count ts) 0)
    (timepoint-at ts 0)
    nil))

(defmethod timeseries-end ((ts timeseries))
  (if (> (ts-item-count ts) 0)
    (timepoint-at ts (1- (ts-item-count ts)))
    nil))

(defmethod reset-view ((ts timeseries))
  (clear-view-caches ts)
  (setf (view-sum-x ts) 0
	(view-sum-y ts) 0
	(view-sum-xx ts) 0
	(view-sum-xy ts) 0
	(view-sum-yy ts) 0
	(view-sum-xy ts) 0
	(view-starti ts) 0
	(view-endi ts) -1)
  (values))

;;; public method to change a view on a time series. Note:
;;; if interval start is before beginning of time series, or if
;;; interval end is after, it is restricted to the start/end of
;;; the time series
(defmethod set-view-interval ((ts timeseries) (interval interval))
  (setf (view-interval ts) interval)
  (when (> (ts-item-count ts) 0)
    (if (or 
	 (> (interval-start interval)
	    (timeseries-end ts))
	 (> (timeseries-beginning ts)
	    (interval-end interval)))
      (reset-view ts)
      (let ((new-starti (find-index-ceiling ts (interval-start interval)))
	    (new-endi (find-index-floor ts (interval-end interval))))
      ;;; ensure we have a valid interval
	(when (= new-endi -1)
	  (setq new-endi 0))
	(when (= new-starti (ts-item-count ts))
	  (setq new-starti (1- new-endi)))
	
	;; make adjustments 
	(let ((old-starti (view-starti ts))
	      (old-endi (view-endi ts)))
	  ;; if there's any differences -- clear caches
	  (when (or (/= new-starti old-starti)
		    (/= new-endi old-endi))
	    (clear-view-caches ts))
	  ;;(format t "Adjusting start:~S to ~S~%" old-starti new-starti)
	  (cond
	   ((< new-starti old-starti)
	    (loop for index from new-starti to (1- old-starti)
		do (increment-state-at ts index)))
	   ((> new-starti old-starti)
	    (loop for index from old-starti to  (1- new-starti)
		doing (decrement-state-at ts index))))
	  ;;(format t "Adjusting end:~S to ~s~%" old-endi new-endi)
	  (cond
	   ((< new-endi old-endi)
	    (loop for index from (1+ new-endi) to old-endi
		doing (decrement-state-at ts index)))
	   ((> new-endi old-endi)
	    (loop for index from (1+ old-endi) to new-endi
		doing (increment-state-at ts index))))
	  (setf (view-starti ts) new-starti)
	  (setf (view-endi ts) new-endi)
	  ))))
  (values))

;;; if we need to, we calculate the slope and intercept. Non-public
(defmethod calculate-slope-intercept ((ts timeseries))
  "Solve the linear regression equation if possible."
  (let ((n (view-count ts)))
    (unless (< n 2)
	(let ((sum-x (view-sum-x ts))
	      (sum-y (view-sum-y ts))
	      (sum-xx (view-sum-xx ts))
	      (sum-xy (view-sum-xy ts)))
	  (let ((nssx (- (* n sum-xx) (* sum-x sum-x)))
		(nssxy (- (* n sum-xy) (* sum-x sum-y))))
	    (unless (zerop nssx)
	      (let* ((slope (/ nssxy nssx))
		     (intercept (/ (- sum-y (* slope sum-x)) n)))
		(setf (view-slope-cached ts) slope)
		(setf (view-intercept-cached ts) (-  intercept (view-timepoint-offset ts)))))))))
  (values))

(defmethod view-slope ((ts timeseries))
  "linear regression slope of time series, or nil if not enough points"
  (or (view-slope-cached ts)
      (progn
	(calculate-slope-intercept ts)
	(view-slope-cached ts))))
	  
    
(defmethod view-intercept ((ts timeseries))
    "linear regression intercept of time series, or nil if not enough points"
  (or (view-intercept-cached ts)
      (progn
	(calculate-slope-intercept ts)
	(view-intercept-cached ts))))

;;; calulate error/min/max if needed.
(defmethod calculate-error-min-max ((ts timeseries))
  (when (> (view-count ts) 0)
    (let ((slope (view-slope ts))
	  (intercept (view-intercept ts))
	  (min nil)
	  (max nil)
	  (err nil)
	  (first-value (value-at ts (view-starti ts))))
      (when (realp first-value)
	(loop for i from (view-starti ts) to (view-endi ts) doing
	      (let ((tp (timepoint-at ts i))
		    (val (value-at ts i)))
		(when (or (null min)
			  (< val min))
		  (setq min val))
		(when (or (null max)
			  (> val max))
		  (setq max val))
		(when (and slope intercept)
		  (if (null err)
		    (setf err (abs (- (+ (* slope tp) intercept)
				      val)))
		    (incf err (abs (- (+ (* slope tp) intercept)
				      val)))))
		(setf (view-min-cached ts) min)
		(setf (view-max-cached ts) max)
		(setf (view-error-cached ts) err))))))
  (values))
    
(defmethod view-min ((ts timeseries))
  "Minimum value (or nil if no values) in current view."
  (or (view-min-cached ts)
      (progn
	(calculate-error-min-max ts)
	(view-min-cached ts))))

(defmethod view-max ((ts timeseries))
    "Maximum value (or nil if no values) in current view."
  (or (view-max-cached ts)
      (progn
	(calculate-error-min-max ts)
	(view-max-cached ts))))

(defmethod view-error ((ts timeseries))
  (or (view-error-cached ts)
      (progn
	(calculate-error-min-max ts)
	(view-error-cached ts))))

(defmethod view-difference ((ts timeseries))
  (let ((max (view-max ts))
	(min (view-min ts)))
    (if (and max min)
      (- max min)
      nil)))

(defmethod view-sum ((ts timeseries))
  (view-sum-y ts))

(defmethod view-sum-squares ((ts timeseries))
  (view-sum-xx ts))

(defmethod view-mean ((ts timeseries))
  (if (and (not (null (view-count ts))) (not (zerop (view-count ts))))
    (/ (view-sum-y ts)
       (view-count ts))
    nil))

(defmethod view-variance ((ts timeseries))
  "Returns the variance of y values, or 0 if there are no values"
  (if (zerop (view-count ts))
    nil
    (let ((n (view-count ts))
	  (sum-yy (view-sum-yy ts))
	  (sum-y (view-sum-y ts)))
      (/ (- sum-yy (/ (* sum-y sum-y) n)) n))))

(defmethod view-stddev ((ts timeseries))
  (let ((var (view-variance ts)))
    (if var (sqrt var) nil)))

(defmethod view-count ((ts timeseries))
  (or (view-count-cached ts)
      (progn
	  (setf (view-count-cached ts)
	    (1+ (- (view-endi ts) (view-starti ts))))
	  (view-count-cached ts))))

(defmethod view-predict ((ts timeseries) timestamp)
  "Predict value at timestamp"
  (let ((slope (view-slope ts))
	(intercept (view-intercept ts)))
    (if (and slope intercept)
      (+ (* slope timestamp) intercept)
      nil)))

(defmethod view-predict-function ((ts timeseries))
  "Return a function that predicts a value at timestamp"
  #'(lambda (timestamp)
      (view-predict ts timestamp)))

(defmethod view-predict-inverse ((ts timeseries) value)
  "Predict timestamp for this value"
  (let ((slope (view-slope ts))
	(intercept (view-intercept ts)))
    (if (and slope intercept (not (zerop slope)))
      (/ (- value intercept) slope)
      nil)))

(defmethod view-predict-inverse-function ((ts timeseries))
  "Return a function that predicts timestamp for a value"
  #'(lambda (value)
      (view-predict-inverse ts value)))

;;;
;;; - other queries on intervals

(defmethod first-value-within ((ts timeseries) (interval interval))
  "First value in the interval' returns value + index; or nil"
  (let ((index (find-index-ceiling ts (interval-start interval))))
    (if (= index (ts-item-count ts))
      nil
      (let ((tp (aref (ts-timepoints ts) index))
	    (val (aref (ts-values ts) index)))
	(if (<= tp (interval-end interval))
	  (values val index)
	  nil)))))

(defmethod last-value-within ((ts timeseries) (interval interval))
  "Last value in the interval. Returns value + index; or nil"
  (let ((index (find-index-floor ts (interval-end interval))))
    (if (= index -1)
      nil
      (let ((tp (aref (ts-timepoints ts) index))
	    (val (aref (ts-values ts) index)))
	(if (>= tp (interval-start interval))
	  (values val index)
	  nil)))))


;;; --- logging policies

(defclass ts-logging-policy ()
  ((count-limit :initarg :count-limit :initform NIL :accessor ts-logging-policy-count-limit)
   (time-limit :initarg :time-limit :initform NIL :accessor ts-logging-policy-time-limit)  
   (frequency-limit :initarg :frequency-limit :initform NIL :accessor ts-logging-policy-frequency-limit)))

(defmethod print-object ((policy ts-logging-policy) stream)
  (print-unreadable-object (policy stream :type t :identity nil)
    (with-slots (count-limit time-limit frequency-limit) policy
      (when count-limit
	(format stream ":count-limit ~a " count-limit))
      (when time-limit
	(format stream ":time-limit ~a " time-limit))
      (when frequency-limit
	(format stream ":frequency-limit ~a " frequency-limit)))))

(defmethod make-load-form ((policy ts-logging-policy) &optional env)
  (declare (ignore env))
  (with-slots (count-limit time-limit frequency-limit) policy
    `(make-instance ',(type-of policy)
       :count-limit ,count-limit
       :time-limit ,time-limit
       :frequency-limit ,frequency-limit)))
		

(defmethod make-ts-logging-policy (&key count-limit time-limit frequency-limit)
  (make-instance 'ts-logging-policy
    :count-limit count-limit
    :time-limit time-limit
    :frequency-limit frequency-limit))

(defmethod add-ts-logging-policy ((ts timeseries) (policy ts-logging-policy))
  (setf (ts-logging-policies ts) (push policy (ts-logging-policies ts)))
  (enforce-ts-logging-policies ts)
  (ts-logging-policies ts))

(defmethod pop-ts-logging-policy ((ts timeseries))
  (setf (ts-logging-policies ts) (cdr (ts-logging-policies ts))))

(defmethod make-least-restrictive-policy ((ts timeseries))
  (let ((count-limit nil)
	(frequency-limit nil)
	(time-limit nil))
    (dolist (p (ts-logging-policies ts))
      (unless (null (ts-logging-policy-count-limit p))
	(if (null count-limit)
	  (setq count-limit (ts-logging-policy-count-limit p))
	  (setq count-limit (max count-limit (ts-logging-policy-count-limit p)))))
      (unless (null (ts-logging-policy-frequency-limit p))
	(if (null frequency-limit)
	  (setq frequency-limit (ts-logging-policy-frequency-limit p))
	  (setq frequency-limit (min frequency-limit (ts-logging-policy-frequency-limit p)))))
      (unless (null (ts-logging-policy-time-limit p))
	(if (null time-limit)
	  (setq time-limit (ts-logging-policy-time-limit p))
	  (setq time-limit (max time-limit (ts-logging-policy-time-limit p))))))
    (make-ts-logging-policy 
     :count-limit count-limit
     :frequency-limit frequency-limit
     :time-limit time-limit)))


(defmethod enforce-ts-logging-policies ((ts timeseries))
  (if (null (ts-logging-policies ts))
    (enforce-default-history-policy ts)
    (let ((count (least-restrictive-count-limit ts))
	  (dur (least-restrictive-time-limit ts)))
      (when count (enforce-ts-logging-policy-count-limit ts count))
      (when dur   (enforce-ts-logging-policy-time-limit ts dur)))))


(defmethod enforce-default-history-policy ((ts timeseries))
  (enforce-ts-logging-policy-count-limit ts 2))


(defmethod enforce-ts-logging-policy-count-limit ((ts timeseries) (count real))
  ;; have n in history, want only m; delete minimum node n-m times
  (timeseries-delete-max-count ts count)
  (ts-item-count ts))

(defmethod enforce-ts-logging-policy-time-limit ((ts timeseries) (duration real))
  ;; delete any node before now - duration 
  (let ((then (- (current-time) duration)))
    (timeseries-delete-floor ts then)
    (ts-item-count ts)))

(defmethod least-restrictive-frequency-limit ((ts timeseries))
  (let ((freq nil))
    (loop for policy in (ts-logging-policies ts)
	when (realp (ts-logging-policy-frequency-limit policy))
	do (if (null freq) 
	     (setq freq (ts-logging-policy-frequency-limit policy))
	     (setq freq (min freq (ts-logging-policy-frequency-limit policy)))))
    freq))

(defmethod least-restrictive-time-limit ((ts timeseries))
  (let ((dur nil))
    (loop for policy in (ts-logging-policies ts)
	when (realp (ts-logging-policy-time-limit policy))
	do (if (null dur) 
	     (setq dur (ts-logging-policy-time-limit policy))
	     (setq dur (max dur (ts-logging-policy-time-limit policy)))))
    dur))

(defmethod least-restrictive-count-limit ((ts timeseries))
  (let ((count nil))
    (loop for policy in (ts-logging-policies ts)
	when (realp (ts-logging-policy-count-limit policy))
	do (if (null count) 
	     (setq count (ts-logging-policy-count-limit policy))
	     (setq count (max count (ts-logging-policy-count-limit policy)))))
    count))


;;; -- Filtering

(defmethod filter-timeseries ((ts timeseries) (function function))
  (loop for i from 0 to (1- (ts-item-count ts)) doing
	(setf (ts-bit-at ts i)
	  (funcall function (value-at ts i) (timepoint-at ts i))))
  (values))

(defmethod timeseries-for-each ((ts timeseries)
				(function function))
  (loop for i from 0 to (1- (ts-item-count ts)) doing
	(funcall function (value-at ts i) (timepoint-at ts i))))

(defmethod timeseries-for-each/filtered ((ts timeseries) 
					 (do-function function)
					 &optional filter-function)
  (loop for i from 0 to (1- (ts-item-count ts)) doing
	(when filter-function
	  (setf (ts-bit-at ts i)
	    (funcall filter-function (value-at ts i) (timepoint-at ts i))))
	(when (ts-bit-at ts i)
	  (funcall do-function (value-at ts i) (timepoint-at ts i))))
  (values))

(defmethod display-filtered-values ((ts timeseries) (stream stream))
  (timeseries-for-each/filtered 
   ts
   (lambda (value timepoint)
     (declare (ignore timepoint))
     (format stream "~a~%" value)))
  (values))

(defmethod timeseries-continuation ((ts timeseries)
				    (function function))
  "returns a continuation which, when called, calls function over all ts values from
   the point of its last call."
  (let ((i 0))
    (lambda ()
      (loop 
	  while (< i (ts-item-count ts))
	  do
	    (funcall function (value-at ts i) (timepoint-at ts i))
	    (incf i)))))

;;; from save-object.lisp

(defun GET-COMPILED-FUNCTION-NAME (fn)
  "Given a function object <fn>, return the symbol name of the function."
  #+lispm
  (when (si:lexical-closure-p fn)
    (return-from get-compiled-function-name nil))
  (etypecase fn 
    (symbol fn)
    (compiled-function #+old-cmu(kernel:%function-header-name fn)
                       #+cmu(kernel:%function-name fn)
                       #+mcl(ccl::function-name fn)
                       #+lispm(si:compiled-function-name fn)
                       #+akcl(system::compiled-function-name fn)
                       #+lucid
                       (when (sys:procedurep fn)
                         (sys:procedure-ref fn SYS:PROCEDURE-SYMBOL))
                       #+excl (xref::object-to-function-name fn)
                       )))

(defun bits/values/timepoint-forms (ts len vname)
  (loop for i from 0 to (1- len) appending
	(list 
	 `(vector-push-extend ,(aref (ts-bitv ts) i) (ts-bitv ,vname))
	 `(vector-push-extend ,(if (typep (aref (ts-values ts) i) 'standard-object)
				 `(progn ,@(multiple-value-list (make-load-form (aref (ts-values ts) i))))
				 (aref (ts-values ts) i))
				 (ts-values  ,vname))
	 `(vector-push-extend ,(aref (ts-timepoints ts) i) (ts-timepoints ,vname)))))

	 
(defmethod make-load-form ((ts timeseries) &optional env)
  (Declare (ignore env))
  (let ((vname (gentemp))
	(len (length (ts-bitv ts))))
    `(let ((,vname (make-instance ',(type-of ts)
		      :value-key ,(if (value-key ts)
				    `',(get-compiled-function-name (value-key ts))
				    nil)
		      :logging-policies
		      (list ,@(loop for policy in (ts-logging-policies ts)
				  collecting
				    `(progn ,@(multiple-value-list
					       (make-load-form policy))))))))
       (setf (slot-value ,vname 'apex.utility.timeseries::bitv)
	 (make-array ,len :adjustable t :fill-pointer 0))
       (setf (slot-value ,vname 'apex.utility.timeseries::values)
	 (make-array ,len :adjustable t :fill-pointer 0))
       (setf (slot-value ,vname 'apex.utility.timeseries::timepoints)
	 (make-array ,len :adjustable t :fill-pointer 0))
       ,@(bits/values/timepoint-forms ts len vname)
       ,vname)))


;;; --- end of file
