;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/estimation-methods.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: estimation-methods.lisp,v 1.4 2006/01/15 03:43:00 dalal Exp $
;;; Created:        September. 2004

;;;
;;; Estimation methods for monitors 
;;;

;;;;;; CASE ONE
;;;(defmethod estimate-persist ((vtype (eql :bound))
;;;			     (ttype (eql :defined))
;;;			     (history sv-history) value interval timestamp duration)
;;;  (declare (ignore interval))
;;;  (let ((obs (observation-prior-or-equal history timestamp)))
;;;    (if (and obs 
;;;	     (equal (observation-value obs) value) 
;;;	     (>= (+ (observation-timestamp obs) duration) timestamp)
;;;	     ;; I think the following makes sense ...
;;;	     )
;;;       no-bindings ;; ie success
;;;      fail)))
;;;	
;;;;;; CASE TWO
;;;(defmethod estimate-persist ((vtype (eql :bound))
;;;			     (ttype (eql :undefined))
;;;			     (history sv-history) value interval timestamp duration)
;;;  (declare (ignore timestamp))
;;;  (let ((first (first-observation-within history interval))
;;;	(prev  (last-observation-within history (make-interval 0 (interval-start interval)))))
;;;    ;; we fail if there aren't any observations at all...
;;;    (when (and (null first) (null prev))
;;;      (return-from estimate-persist fail))
;;;    ;; however, if there's no previous obs, or the first obs is at the
;;;    ;; interval boundary ...
;;;      (if (or (null prev)
;;;	      (and first
;;;		   (= (observation-timestamp first)
;;;		      (interval-start interval))))
;;;	(sv-history-for-each 
;;;	 history interval
;;;	 (lambda (obs)
;;;	   (when (equal (observation-value obs) value) 
;;;	     ;; Non-local exit
;;;	     (return-from estimate-persist no-bindings))))
;;;	;;; otherwise, we have a prev, so we need to look at it ...
;;;	(when (and (equal (observation-value prev) value)
;;;		   (<= (- (interval-start interval)
;;;			  (observation-timestamp prev))
;;;		       duration))
;;;	  (return-from estimate-persist no-bindings))))
;;;      ;;; we didn't exit non-locally, so fail
;;;      fail)
;;;
;;;
;;;
;;;;;; CASE THREE
;;;(defmethod estimate-persist ((vtype (eql :unbound))
;;;			     (ttype (eql :defined))
;;;			     (history sv-history) value interval timestamp duration)
;;;  (declare (ignore interval))
;;;  (let ((obs (observation-prior-or-equal history timestamp)))
;;;    (if (and obs 
;;;	     ;; (equal (observation-value obs) value) 
;;;	     (>= (+ (observation-timestamp obs) duration) timestamp)
;;;		 )
;;;      (extend-bindings value (observation-value obs) no-bindings) ;; ie success
;;;      fail)))
;;;
;;;;;; CASE FOUR
;;;(defmethod estimate-persist ((vtype (eql :unbound))
;;;			     (ttype (eql :undefined))
;;;			     (history sv-history) value interval timestamp duration)
;;;  (declare (ignore timestamp ))
;;;  (let ((first (first-observation-within history interval))
;;;	(prev  (last-observation-within history (make-interval 0 (interval-start interval)))))
;;;    ;; we fail if there aren't any observations at all...
;;;    (when (and (null first) (null prev))
;;;      (return-from estimate-persist fail))
;;;    ;; however, if there's no previous obs, or the first obs is at the
;;;    ;; interval boundary ...
;;;      (if (or (null prev)
;;;	      (and first
;;;		   (= (observation-timestamp first)
;;;		      (interval-start interval))))
;;;	(sv-history-for-each 
;;;	 history interval
;;;	 (lambda (obs)
;;;	     ;; Non-local exit
;;;	   (return-from estimate-persist
;;;	     (extend-bindings value (observation-value obs) no-bindings))))
;;;	(when (and ;; (equal (observation-value prev) value)
;;;		   (<= (- (interval-start interval)
;;;			  (observation-timestamp prev))
;;;		       duration))
;;;	  (return-from estimate-persist no-bindings))))
;;;      ;;; we didn't exit non-locally, so fail
;;;      fail)
;;;
;;;
;;;;;;====================================================================================
;;;;;;====================================================================================
;;;
;;;;;; ---
;;;;;;
;;;;;; Regression  estimators.
;;;;;; 
;;;;;; Apex supports linear, quadratic, cubic regression & n-degree regression.
;;;;;;
;;;;;; The keyword for linear regression is linear-regression
;;;;;; The keyword for quadratic regression is quadratic-regression
;;;;;; The keyword for cubic regression is cubic-regression
;;;;;; The keyword for n-degree regression is regression 
;;;;;;
;;;;;; Canonical example: given a set of observations, does a linear regression on
;;;;;; what is the estimated value at a given timepoint?
;;;;;; 
;;;;;; (waitfor (:observation o1 (altitude ac-1 ?alt)
;;;;;;		:timestamp ( (= (+ (start-of +this-task+) P30M))
;;;;;;                          (>= (- (start-of +this-task+) P30M))
;;;;;;                          (<= (start-of +this-task+)) )
;;;;;;		:estimate (linear-regression :minimum-points 10 :maximum-error 25) ))
;;;;;;
;;;;;;
;;;;;; Observations are sampled from the 'maximum duration' defined by the :TIMESTAMP
;;;;;; parameter. In the example above, the interval [(>= (- (start-of +this-task+) P30M)).
;;;;;;  (<= (start-of +this-task+)) ]. If the duration is not defined, then it defaults
;;;;;; to [ 0, (start-of +this-task+)] (i.e., all stored observations, which have to 
;;;;;; be logged to be available).
;;;;;;
;;;;;; The TIMESTAMP parameter to :OBSERVATION may also define a timepoint of interest (TPI). 
;;;;;; This is defined by the = operator. It defaults to +now+ 
;;;;;;
;;;;;; Parameters to LINEAR-REGRESSION and QUADRATIC-REGRESSION and REGRESSION form: 
;;;;;;   :minimum-points :maximum-error :maximum-difference :minimum-frequency
;;;;;;
;;;;;; Minumum points defines the minumum points used to calculate the regression function. If 
;;;;;; there are not that many points, then the monitor immediately fails.
;;;;;; It defaults to the degree of the regression + 1 (that is, 2 points for linear,
;;;;;; three points for quadratic, etc.).
;;;;;; 
;;;;;; Maximum error defines the maximum standard error of the estimate acceptable. 
;;;;;; where standard error of the estimate is sqrt(sum((pred-actual)^2)/n). It
;;;;;; defaults to infinity.
;;;;;; 
;;;;;; Maximum difference defines the maximum difference acceptable for an
;;;;;; an estimate from a given value. It defaults to the standard error of the
;;;;;; estimate (which isn't defined until the regression is done).
;;;;;;
;;;;;; Minimum frequency defines the minimum frequency of the sample observations
;;;;;; acceptable; below 2 points in the sample fall below this frequency, then
;;;;;; the regression will fail. It defaults to infinity.
;;;;;; 
;;;;;; The REGRESSION form has an additional keyword:
;;;;;;   :degree 
;;;;;; 
;;;;;; Degree defines the degree desired for the regression formula. It defaults to 1 (linear
;;;;;; regression).
;;;;;;
;;;;;;
;;;;;; The SV value may be bound or unbound.
;;;;;;
;;;;;; The interesting parameter space: 
;;;;;; 
;;;;;;   sv value:                    bound or unbound
;;;;;; 
;;;;;; Thus, there are 2 cases. 
;;;;;; 
;;;;;; Note: The minimum number of points and maximum error define thresholds which
;;;;;; must be met for a monitor to succeed. If the threshold isn't met, the monitor
;;;;;; can't succeed in other ways.
;;;;;; 
;;;;;; If the SV object is a variable, each SV in the sv memory with the defined
;;;;;; attribute undergoes the case analysis.
;;;;;; 
;;;;;; It is an error to have the SV attribute unbound when a monitor is checked.
;;;;;;
;;;;;;====================================================================================
;;;;;; CASE 1: SV value bound.
;;;;;; Example: 
;;;;;;
;;;;;; (waitfor (:observation o1 (altitude ac-1 32000)
;;;;;;		       :timestamp ((= (+ (start-of +this-task+) P30M)))
;;;;;;		       :estimate (linear-regression :maximum-difference 100)))
;;;;;; --------		
;;;;;; Procedural semantics:
;;;;;; Using observations on SV [(altitude ac1)] in the interval, 
;;;;;; calculate the regression function
;;;;;; solve for y at t = TPI [(+ (start-of +this-task+) P30M)]
;;;;;; is y within maximum-difference (100) of SV value? (32000)? 
;;;;;;====================================================================================
;;;;;; CASE 2: SV value unbound.
;;;;;; Example: 
;;;;;;
;;;;;; (waitfor (:observation o1 (altitude ac-1 ?val)
;;;;;;		       :estimate (:linear-regression :maximum-difference 100)))
;;;;;; --------		
;;;;;; Procedural semantics:
;;;;;;  
;;;;;; Using observations on SV [(altitude ac1)] in the interval, 
;;;;;; calculate the regression function.
;;;;;; bind SV value variable to fn(TPI).
;;;;;; Warn if :maximum-difference is defined.
;;;;;;
;;;;;;====================================================================================
;;;;;; First, some code for doing polynomial curve fitting ...
;;;;;; ----
;;;;;;
;;;;;; Given two column matrices of the same size, x, y, 
;;;;;; We wish to determine a polynomial of the form:
;;;;;;
;;;;;;  Q(x) = b_0 + b_1x + b_2x^2 + ... + b_nx^n
;;;;;;
;;;;;; that minimizes:
;;;;;;
;;;;;; S = (y_0 - Q(x_0))^2 + (y_1 - Q(x_1))^2 + ... + (y_n - Q(x_n))^2
;;;;;;
;;;;;; According to Venit & Bishop*, we can define the linear system:
;;;;;;
;;;;;; (U^TU)b = U^Ty
;;;;;;
;;;;;; where y = (y_0, y_1, ..., y_m) and (a *column matrix*)
;;;;;;
;;;;;; U = [ 1 x_0 x_0^2 ... x_0^n]
;;;;;;     [ 1 x_1 x_1^2 ... x_1^n]
;;;;;;     [ ...   ...   ... ...  ]
;;;;;;     [ 1 x_m x_m^2 ... x_m^n]
;;;;;;
;;;;;; U is called the Vandermonde matrix.
;;;;;;
;;;;;; The matrix (U^TU) adjoin U^Ty  (ie, 'stuck next to each other')
;;;;;; 
;;;;;; can then be solved as a linear system in the usual way,
;;;;;; and this will define the coefficients of Q.
;;;;;;
;;;;;; *Venit, Stewart and Wayne Bishop, Elementary Linear Algebra, 
;;;;;;  Prindle, Weber & Schmidt, 1981, pp. 287ff).
;;;
;;;(defun vandermonde-matrix (xs degree)
;;;  (let ((rows (matrix:num-rows xs))
;;;	(cols (1+ degree)))
;;;    (let ((U (matrix:make-matrix rows cols)))
;;;      (dotimes (r rows)
;;;	(setf (aref U r 0) 1)
;;;	(loop for c from 1 to (1- cols)
;;;	    doing
;;;	      (let ((x (aref xs r 0)))
;;;		(setf (aref U r c) 
;;;		  (expt x c)))))
;;;      U)))
;;;
;;;(defun matrix-copy-into (source target &key (source_row 0)
;;;					  (source_col 0)
;;;					  (target_row 0)
;;;					  (target_col 0))
;;;  "Copy from source to target, copying into"
;;;  (let ((rows (matrix:num-rows source))
;;;         (cols (matrix:num-cols source)))
;;;    (dotimes (row rows target)
;;;      (dotimes (col cols)
;;;        (setf (aref target
;;;		    (+ target_row row)
;;;		    (+ target_col col))
;;;	  (aref source (+ source_row row)
;;;		(+ source_col col)))))))
;;;;;; input: xs:matrix of x values
;;;;;;        ys:matrix of y values
;;;;;;        degree:integer degree of polynomial
;;;;;; output:
;;;;;;     soln:vector a vector representing the 
;;;;;;     polynomial that fits xs to ys, where the polynomial
;;;;;;     is y = c_0x^0 + c_1x^1 + c_2x^2 + ... + c_nx^n, where
;;;;;;     that is $$y = \sum_{i=0}^n{c_ix^i}$$, where $c_i$ is the 
;;;;;;     $i$th element in the vector.
;;;;;;
;;;(defun least-squares-polynomial-matrix (xs ys degree)
;;;  (flet ((ls-adjoin-matrix (lhs rhs)
;;;	   (let ((m (matrix:make-matrix (matrix:num-rows lhs) (1+ (matrix:num-cols lhs)))))
;;;	     (matrix-copy-into lhs m)
;;;	     (matrix-copy-into rhs m :target_col  (matrix:num-cols lhs))
;;;	     m)))
;;;    (let* ((U (vandermonde-matrix xs degree))
;;;	   (UT (matrix:transpose-matrix U))
;;;	   (coeff (matrix:multiply-matrix UT U))
;;;	   (ysol (matrix:multiply-matrix UT ys))
;;;	   (Q (matrix:solve-matrix (ls-adjoin-matrix coeff ysol) t)))
;;;      Q)))
;;;
;;;
;;;;;; 
;;;;;; That's all very good. We should also calculate the error
;;;;;;
;;;(defun calculate-least-squares-error (xs ys fn)
;;;  (let ((rows (matrix:num-rows xs)))
;;;    (sqrt (loop for r from 0 to (1- rows)
;;;	      with diff = (- (funcall fn (aref xs r 0))
;;;			     (aref ys r 0))
;;;	      sum (* diff diff)))))
;;;;;;
;;;;;; Ana a matrix of coefficients are nice, but a function
;;;;;; is a lot more useful.
;;;;;;
;;;(defun least-squares-function (sol)
;;;  (lambda (x)
;;;    (loop for coef across sol
;;;	for exponent from 0
;;;	summing (* coef (expt x exponent)))))
;;;
;;;;;; OK! Given column vectors x and y and a desired degree,
;;;;;; return three values:
;;;;;; a function of x which minimizes error to y;
;;;;;; the error term from the data
;;;;;; and the coefficient matrix, just in case that's useful...
;;;(defun least-squares-polynomial-function (xs ys degree)
;;;  (let* ((sol (least-squares-polynomial-matrix xs ys degree))
;;;	 (fn (least-squares-function sol)))
;;;    (values fn (calculate-least-squares-error xs ys fn) sol)))
;;;
;;;;;; Now, let's provide these answers from state variable histories
;;;
;;;(defun list->matrix (l &optional (column? t))
;;;  (let ((len (length l)))
;;;    (if column?
;;;      (let ((m (matrix:make-matrix len 1)))
;;;	(loop for x in l
;;;	    for i from 0
;;;	    do    (setf (aref m i 0) x))
;;;	m)
;;;      (let ((m (matrix:make-matrix 1 len)))
;;;	(loop for x in l
;;;	    for i from 0
;;;	    do (setf (aref m 0 i) x))
;;;	m))))
;;;
;;;(defmethod regression-function ((hist sv-history) (interval interval) (degree integer))
;;;  (let ((xs (list))
;;;	(ys (list)))
;;;    (sv-history-for-each 
;;;     hist interval
;;;     (lambda (obs)
;;;       (push (observation-timestamp obs) xs)
;;;       (push (observation-value obs) ys)))
;;;    (assert (<= degree (length xs)))
;;;    (assert (>= (length xs) 2))
;;;    (least-squares-polynomial-function 
;;;     (list->matrix (nreverse xs))
;;;     (list->matrix (nreverse ys))
;;;     degree)))
;;;
;;;;;; how many observation points in an interval?
;;;    
;;;(defmethod observation-count ((hist sv-history) (interval interval))
;;;  (let ((cnt 0))
;;;    (sv-history-for-each hist interval
;;;			 (lambda (obs) (declare (ignore obs)) (incf cnt)))
;;;    cnt))
;;;
;;;(defmethod estimate ((type (eql :linear-regression)) (history sv-history) value interval timestamp parameters)
;;;  (estimate-regression history value interval timestamp parameters 1))
;;;
;;;(defmethod estimate ((type (eql :quadratic-regression)) (history sv-history) value interval timestamp parameters)
;;;  (estimate-regression history value interval timestamp parameters 2))
;;;
;;;(defmethod estimate ((type (eql :cubic-regression)) (history sv-history) value interval timestamp parameters)
;;;  (estimate-regression history value interval timestamp parameters 3))
;;;
;;;(defmethod estimate ((type (eql :regression)) (history sv-history) value interval timestamp parameters)
;;;  (let ((degree (or (parameter-value :degree parameters) 1)))
;;;    (estimate-regression history value interval timestamp parameters degree)))
;;;
;;;(defmethod estimate-regression ((history sv-history) value interval timestamp parameters degree)
;;;  (let ((minimum-points (or (parameter-value :minimum-points parameters) (1+ degree)))
;;;	(maximum-error  (parameter-value :maximum-error parameters))
;;;	(minimum-frequency-spec (parameter-value :minimum-frequency parameters))
;;;	(start (patmatch-eval (convert-duration-specs (or (parameter-value :start parameters) 0)) no-bindings))
;;;	(end (patmatch-eval (convert-duration-specs (or (parameter-value :end parameters) (current-time))) no-bindings))	
;;;	(timestamp (or timestamp (current-time))))
;;;    (if (or (> start end)
;;;	    (not (<= (interval-start interval) timestamp (interval-end interval))))
;;;      fail
;;;    (let ((data-interval (make-interval start end)))
;;;      (setq minimum-points (max minimum-points (1+ degree))) ;; in case someone makes a mistake ...
;;;    ;;; Non local exit
;;;      (unless (> (observation-count history data-interval)
;;;		 minimum-points)
;;;      ;; (format t "Unfortunately, there are not enough data points in ~S over ~S (only ~S, need ~S)~%" history interval (observation-count history interval) minimum-points)
;;;	(return-from estimate-regression FAIL))
;;;    ;;; non-local exit
;;;      (when minimum-frequency-spec
;;;	(let ((dur (duration-read minimum-frequency-spec))
;;;	      (last-time nil))
;;;	  (sv-history-for-each history data-interval
;;;			       (lambda (observation)
;;;				 (when (and last-time
;;;					    (> (- (observation-timestamp observation) last-time)
;;;					       dur))
;;;				   ;;(format t "Unfortunately, I must fail~%")
;;;				   (return-from estimate-regression FAIL))
;;;				 (setq last-time (observation-timestamp observation))))))
;;;      (multiple-value-bind (fn err)
;;;	  (regression-function history data-interval degree)
;;;      ;;; non-local exit
;;;	(when (and maximum-error (> err maximum-error) )
;;;	  ;;(format t "Unfortunately, the error is too great (was ~A, max is ~A)~%" err maximum-error)
;;;	  (return-from estimate-regression FAIL))
;;;	(let ((estimate (funcall fn timestamp)))
;;;	  (if (variable-p value)
;;;	    (progn
;;;	      (when (parameter-value :maximum-difference parameters)
;;;		(warn "~A is unbound; maximum difference ignored." value))
;;;	      (extend-bindings value estimate no-bindings))
;;;	    (let ((maximum-difference (or (parameter-value :maximum-difference parameters) 
;;;					  +pos-infinity+)))
;;;	      (if (> (abs (- value estimate)) maximum-difference)
;;;		FAIL
;;;		no-bindings)))))))))



