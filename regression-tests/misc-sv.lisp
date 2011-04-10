;;;-*- Mode: Lisp; Package: :apex.asa.sv.test -*-
;;; ------------------------------------------------------------------------
;;; File:           $Id: misc-sv.lisp,v 1.10 2006/01/15 03:42:59 dalal Exp $
;;; Created:        July, 2004
;;;
;;; Description: Tests pattern matching
;;; 
;;;


(defpackage :apex.asa.sv.test
  (:use :common-lisp
	:apex.asa.sv
	:apex.utility.datetime
	:apex.utility.pipe
	:apex.utility.timeseries
	:apex.utility.unit-test)
  (:export #:main))

(in-package :apex.asa.sv.test)

(defun timestamp-of-<= (history ts)
  (let ((obs (measurement-prior-or-equal history ts)))
    (and obs (measurement-timestamp obs))))

(defmacro test-policy (kind constraint limit target )
  `(let ((ctimedef (fdefinition 'cl-user::current-time)))
     (unwind-protect 
	 (let ((mem (make-sv-memory))
	       (sv (make-state-variable 'alt 'ac1)))
	   (start-sv-logging-policy mem sv ,kind ,constraint)
	   (dotimes (i ,limit)
	     (setf (fdefinition 'cl-user::current-time)
	       (lambda () i))
	     (insert-measurement mem (make-measurement sv i i)))
	   (let* ((h (sv-history mem sv))
		  (currents (loop for i from 0 to (1- (ts-item-count h))
			     collecting
				  (measurement-value (elt (ts-values h) i)))))
	     (format t "Policy: ~S ~S; inserts: ~s Values: ~S~%" ,kind  ,constraint ,limit currents)
	     (test currents ,target :test #'equal)))
       (setf (fdefinition 'cl-user::current-time) ctimedef))))

(defun main ()
  (with-tests (:name "State Variables")
    (with-tests (:name "Basics")
      (test t
	    (sv-equalp  (make-state-variable 'color 'red)
			(make-state-variable 'color 'red)))
      
      (test nil
	    (sv-equalp (make-state-variable 'color 'blue)
		       (make-state-variable 'color 'red))))
    (with-tests (:name "SV History")
      (let ((mem (make-sv-memory))
	    (sv (make-state-variable 'alt 'ac1))
	    (sv2 (make-state-variable 'alt 'ac2))
	    (tm (current-time)))
	(start-sv-logging-policy mem sv :count-limit 1000)
	(start-sv-logging-policy mem sv2 :count-limit 1000)	
	
	(dotimes (i 10)
	  (test-no-error 
	   (insert-measurement 
	    mem
	    (make-measurement sv i (+ tm (* 3600 i)))))
	  (test-no-error 
	   (insert-measurement 
	    mem
	    (make-measurement sv2 i (+ tm (* 3600 i))))))
	(test 0
	      (measurement-value (earliest-measurement (sv-history mem sv))))
	(test 9
	      (measurement-value (latest-measurement (sv-history mem sv))))
	(test t (= 10 (sv-history-count (sv-history mem sv2))))
	(test t (= 10 (sv-history-count (sv-history mem sv))))
	(test (loop for i from 0 to 9 collecting i)
	      (sv-history-map (sv-history mem sv) (make-interval tm (+ tm  (* 3600 9))) #'measurement-value))
	(test (loop for i from 0 to 5 collecting i)
	      (sv-history-map (sv-history mem sv) (make-interval tm (+ tm  (* 3600 5))) #'measurement-value))
	(test (list 5)
	      (sv-history-map (sv-history mem sv) (make-interval (+ tm  (* 3600 5)) (+ tm  (* 3600 5))) #'measurement-value))
	(test (list 5)
	      (sv-history-map (sv-history mem sv) (make-interval (1- (+ tm  (* 3600 5)))  (1+ (+ tm  (* 3600 5)))) #'measurement-value))
	(test tm (timestamp-of-<= (sv-history mem sv) tm))
	(test tm (timestamp-of-<= (sv-history mem sv) (+ tm 1)))

	))
    (with-tests (:name "Valid Measurement Patterns")
    ;;; some tests about measurement patterns
      (test-error (check-valid-measurement-pattern '()))
      (test-error (check-valid-measurement-pattern '(a)))
      (test-error (check-valid-measurement-pattern '(a b)))
      (test-error (check-valid-measurement-pattern '(a b c d)))
      (test-error (check-valid-measurement-pattern '(a b c d e)))
      (test-no-error (check-valid-measurement-pattern '(a ?b = c)))
      (test-error (check-valid-measurement-pattern '(10 a c)))
      (test-error (check-valid-measurement-pattern '(?a b c)))
      (test-error (check-valid-measurement-pattern '(+a+ b c)))
      (test-no-error (check-valid-measurement-pattern '(a b = c)))    
      (test-no-error (check-valid-measurement-pattern '(alt ac-1 = 32000)))
      (test-no-error (check-valid-measurement-pattern '(alt ac-1 =  ?alt)))
      (test-no-error (check-valid-measurement-pattern '(alt ac-1 = <alt>)))
      (test-no-error (check-valid-measurement-pattern '(alt ac-1 = +cruise-alt+)))
      (test-no-error (check-valid-measurement-pattern '(alt <ac> = 32000)))
      (test-no-error (check-valid-measurement-pattern '(alt <ac> = ?)))
      (test-no-error (check-valid-measurement-pattern '(alt <ac> = <alt>)))
      (test-no-error (check-valid-measurement-pattern '(alt <ac> = +cruise-alt+)))
      (test-no-error (check-valid-measurement-pattern '(alt <ac> = ?)))
      (test-no-error (check-valid-measurement-pattern '(alt +a+ = +b+)))
      (test-no-error (check-valid-measurement-pattern '(alt ?a = b+)))
      (test-error (check-valid-measurement-pattern '(?a ?b = ?c)))
      (test-error (check-valid-measurement-pattern '(a ?b = ?c)))
      (test-error (check-valid-measurement-pattern '(a ?b = ?)))
      (test-no-error (check-valid-measurement-pattern '(a ? = <?c>)))
      (test-no-error (check-valid-measurement-pattern '(a ? = c)))      
      )
    (with-tests (:name "Descriptive Statistics")
      (let ((mem (make-sv-memory))
	    (sv (make-state-variable 'problem 'gauss)))
	(start-sv-logging-policy mem sv :count-limit 1000)
	(loop for i from 1 to 99 do
	      (insert-measurement 
	       mem
	       (make-measurement sv i i)))
	(flet ((approx= (x y)
		 (< (abs (- x y)) .005)))
	  (set-view-interval (sv-history mem sv) (make-interval 0 100))
	  (let ((mean (view-mean (sv-history mem sv)))
		(count (view-count (sv-history mem sv)))
		(sum (view-sum (sv-history mem sv)))
		(stddev (view-stddev (sv-history mem sv)))
		(variance (view-variance (sv-history mem sv)))
		(min (view-min (sv-history mem sv)))
		(max (view-max (sv-history mem sv))))
	    (format t "mean: ~a; count: ~a; sum: ~a; stddev: ~a; var: ~a~%" mean count sum stddev variance)
	    (test 50 mean  :test #'approx=)
	    (test 99 count :test #'approx=)
	    (test 4950 sum :test #'approx=)
	    (test 816.6667 variance :test #'approx=)
	    (test 28.577381 stddev :test #'approx=)
	    (test 1 min :test #'approx=)
	    (test 99 max :test #'approx=)	  
	    ))))
    
    (with-tests (:name "Measurement pipes")
      (let ((mem (make-sv-memory))
	    (sv (make-state-variable 'problem 'gauss)))
	(start-sv-logging-policy mem sv :count-limit 1000)
	(loop for i from 1 to 99 do
	      (insert-measurement 
	       mem
	       (make-measurement sv i i)))
	(flet ((polen (start end)
		 (length (enumerate-pipe (sv-measurement-pipe (sv-history mem sv) (make-interval start end))))))
	  (test 0 (polen 0 0))
	  (test 1 (polen 0 1))
	  (test 1 (polen 1 1))
	  (test 2 (polen 1 2))
	  (test 2 (polen 98 99))
	  (test 99 (polen 0 1000))
	  (test 99 (polen 1 99))
	  )))
    
    (with-tests (:name "First/Last obs within")
      (let ((mem (make-sv-memory))
	    (sv (make-state-variable 'fought 'usa)))
	(start-sv-logging-policy mem sv :count-limit 1000)
	(flet ((make-war (war year)
		 (insert-measurement 
		  mem 
		  (make-measurement sv war year)))
	       (war-of (obs)
		 ;; (format t "Measurement: ~S~%" obs)
		 (if obs (measurement-value obs))))
	  (flet ((wars-between (st end)
		   (let* ((interval (make-interval st end))
			  (wars
			   (list (war-of (first-measurement-within (sv-history mem sv) interval))
				 (war-of (last-measurement-within (sv-history mem sv) interval)))))
		     ;; (format t "wars between ~s and ~s: ~a~%" st end wars)
		     wars)))
	  (make-war 'revolution 1776)
	  (make-war 'war-1812   1812)
	  (make-war 'civil-war  1864)
	  (make-war 'ww-i       1912)
	  (make-war 'ww-ii      1945)
	  (make-war 'korean-war 1950)
	  (test '(nil nil) (wars-between 1000 1200))
	  (test '(revolution revolution) (wars-between 1000 1776))
	  (test '(revolution war-1812) (wars-between 1000 1815))
	  (test '(revolution war-1812) (wars-between 1776 1812))
	  (test '(revolution war-1812) (wars-between 1776 1812))
	  (test '(war-1812 ww-i) (wars-between 1800 1915))
	  (test '(ww-i korean-war) (wars-between 1900 1950))
	  (test '(korean-war korean-war) (wars-between 1949 1950))	  
	  (test '(korean-war korean-war) (wars-between 1950 1950))	  	  
	  (test '(korean-war korean-war) (wars-between 1950 2000))	  	  	  
	  (test '(nil nil) (wars-between 2000 2004))
	  (test '(revolution korean-war) (wars-between 1776 1950))
	  (test '(revolution korean-war) (wars-between 1700 1950))	  
	  (test '(revolution korean-war) (wars-between 1776 2000))	  
	  (test '(revolution korean-war) (wars-between 1700 2000))	  	  
	  
	  ))))
    
    
    (with-tests (:name "Basic logging tests")
      (test-policy :count-limit 0 5 '())
      (test-policy :count-limit 10 5 '(0 1 2 3 4))
      (test-policy :count-limit 1 100 '(99))
      (test-policy :count-limit 10 100 '(90 91 92 93 94 95 96 97 98 99))
      (test-policy :frequency-limit '(0 ms) 5 '())
      (test-policy :frequency-limit '(1 ms) 5 '(0 1 2 3 4))
      (test-policy :frequency-limit '(2 ms) 5 '(0 2 4))
      (test-policy :frequency-limit '(100 ms) 5 '(0))
      (test-policy :time-limit 0 5 '())
      (test-policy :time-limit '(10 ms) 5 '(0 1 2 3 4))
      (test-policy :time-limit '(10 ms) 100 '(90 91 92 93 94 95 96 97 98 99))
      (test-policy :time-limit '(1 ms) 100 '(99))

      )))



(main)

