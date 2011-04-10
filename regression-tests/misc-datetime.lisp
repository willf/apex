;;;-*- Mode: Lisp; Package: :apex.utility.datetime.test -*-
;;;
;;; apex/regression-tests/misc-datetime.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-datetime.lisp,v 1.5 2006/01/15 03:42:59 dalal Exp $



(defpackage :apex.utility.datetime.test
  (:use :common-lisp)
  (:use :apex.utility.datetime)
  (:use :apex.utility.unit-test)
  (:use :inet.util.process)
  (:export #:main)
  )

(in-package :apex.utility.datetime.test)

;;; Test input for 'nil-on-error' option to duration-read
(defparameter *bad-durations*
    '((3 se)
      (3)
      foo
      "Pie"
      "P1"
      "P1b"
      p234m34ss))

(defun bad-durations-test ()
  (notany #'duration-expression? *bad-durations*))

(defun random-duration-princ-test (&key (tests 100) ( max (expt 10 12)))
  (let ((succeed T))
    (dotimes (i tests)
      (let ((n (random (floor max))))
	(unless 
	    (= n 
	       (duration-read (with-output-to-string (str) 
				(duration-princ n str))))
	  (warn "Durations not equal. Duration: [~S]. Duration read: [~S]. Period output: ~S. Difference: [~s]."
		n
		(duration-read (with-output-to-string (str) 
				 (duration-princ n str)))
		(with-output-to-string (str) 
		  (duration-princ n str))
		(- n (duration-read (with-output-to-string (str) 
				      (duration-princ n str)))))
	  (setq succeed NIL))))
    succeed))

(defun run-several-test-duration-princ-tests (&optional (tests 100))
  (format t "~%Running ~S tests of reading and writing durations.~%" (* tests 10))
  (and (random-duration-princ-test :tests tests :max 100)
       (random-duration-princ-test :tests tests :max 1000)
       (random-duration-princ-test :tests tests :max 10000)
       (random-duration-princ-test :tests tests :max (* 2 apex.utility.datetime::*msecs-per-minute*))
       (random-duration-princ-test :tests tests :max (* 2 apex.utility.datetime::*msecs-per-hour*))
       (random-duration-princ-test :tests tests :max (* 2 apex.utility.datetime::*msecs-per-day*))
       (random-duration-princ-test :tests tests :max (* 2 apex.utility.datetime::*msecs-per-week*))
       (bad-durations-test)))





(defun random-interval ()
  (let* ((now (current-milliseconds))
	 (a (random now))
	 (b (random now)))
    (make-interval (min a b) (max a b))))


(let ((stats (list (cons 'contains-p 0)
		   (cons 'finishes-p 0)
		   (cons 'starts-p 0)
		   (cons 'before-p 0)
		   (cons 'meets-p 0)
		   (cons 'overlaps-p 0)
		   (cons 'cotemporal-p 0)
		   (cons 'during-p 0)
		   (cons 'finished-by-p 0)
		   (cons 'started-by-p 0)
		   (cons 'after-p 0)
		   (cons 'met-by-p 0)
		   (cons 'overlapped-by-p 0)
		   (cons #'(lambda (x y) (declare (ignore x y)) t) 0)))
      (continue nil))
  (defun stats ()
    stats)
  (defun run-stats-once ()
    (let ((a (random-interval))
	  (b (random-interval)))
    (loop for pair in stats doing
	  (when (funcall (car pair) a b)
	    (incf (cdr pair))))))
  (defun stop-stats ()
    (setq continue nil) stats)
  (defun run-stats ()
    (setq continue t)
    (proc.run-function "Running stats."
      #'(lambda ()
	  (loop while continue do (run-stats-once)))))
  (defun reset-stats ()
    (loop for pair in stats doing (setf (cdr pair) 0))
    stats))


(defun check-stats ()
  (let* (
	 (stats (stats))
	 (totals (mapcar #'cdr (butlast stats)))
	 (total (reduce #'+ totals))
	 (pcts (mapcar #'(lambda (x) (floor (* 100.0 (/ x total)))) totals))
	 (total-ok (= total (cdr (first (last stats)))))
	 (pcts-ok (every #'(lambda (x)
			     (or (= x 16)  (= x 0)))
			 pcts))
	 (non-zeros (loop for x in (butlast stats) when (< 0  (cdr x)) collect (car x)))
	 (non-zeros-ok (equalp non-zeros '(contains-p before-p overlaps-p during-p after-p overlapped-by-p)))
	 
	 )
    (unless total-ok (warn "totals do not match: ~S vs ~S" total (cdr (first (last stats)))))
    (unless pcts-ok (warn "Cumulative percentages are not correct to 1% point: ~A" pcts))
    (unless non-zeros-ok (warn "Surprising extra non-zeros ~A or zeros ~a accumulated."
			       (set-difference '(contains-p before-p overlaps-p during-p after-p overlapped-by-p) 
					       non-zeros)
			       (set-difference non-zeros
					       '(contains-p before-p overlaps-p during-p after-p overlapped-by-p))))			       
			       
    (list total-ok pcts-ok non-zeros-ok)))

(defun tests-run ()
  (cdr (first (last (stats)))))

(defun do-interval-test-check (&optional (n 250000))
  (format t "Running ~s tests of interval comparisons.~%" n)
  (reset-stats)
  (dotimes (i n) (run-stats-once))
  (equalp (check-stats) '(t t t)))


(defun temporal-test (name s1 f1 s2 f2)
  (if (funcall name (make-interval s1 f1) (make-interval s2 f2))
      t
    (progn
      (warn "~A is not correct with intervals [~a ~a] [~a ~a]"
	    name s1 f1 s2 f2)
      nil)))

(defun testit ()
  (temporal-test #'before-p 0 50 100 200)
  (temporal-test #'contains-p 50 250 100 200)
  (temporal-test #'after-p 250 300 100 200)
  (temporal-test #'overlaps-p 50 150 100 200)
  (temporal-test #'overlapped-by-p 150 250 100 200)
  (temporal-test #'finished-by-p 50 200 100 200)
  (temporal-test #'started-by-p 100 250 100 200)
  (temporal-test #'cotemporal-p 100 200 100 200)
  (temporal-test #'starts-p 100 150 100 200)
  (temporal-test #'finishes-p 150 200 100 200)
  (temporal-test #'meets-p 0 100 100 200)
  (temporal-test #'met-by-p 200 300 100 200)
  (temporal-test #'during-p 125 175 100 200))

  
(defun main ()
  (with-tests (:name "Time and Date functions")
    (test T
	  (run-several-test-duration-princ-tests 100))

    (test T  
	  (temporal-test 'before-p 0 50 100 200))
    (test T 
	  (temporal-test 'contains-p 50 250 100 200))

    (test T 
	  (temporal-test 'after-p 250 300 100 200))

    (test T 
	  (temporal-test 'overlaps-p 50 150 100 200))

    (test T 
	  (temporal-test 'overlapped-by-p 150 250 100 200))

    (test T 
	  (temporal-test 'finished-by-p 50 200 100 200))

    (test T 
	  (temporal-test 'started-by-p 100 250 100 200))

    (test T 
	  (temporal-test 'cotemporal-p 100 200 100 200))

    (test T 
	  (temporal-test 'starts-p 100 150 100 200))

    (test T 
	  (temporal-test 'finishes-p 150 200 100 200))

    (test T 
	  (temporal-test 'meets-p 0 100 100 200))

    (test T 
	  (temporal-test 'met-by-p 200 300 100 200))

    (test T 
	  (temporal-test 'during-p 125 175 100 200))

    (test t
	  (temporal-test 'during-p 125 125 100 200))

    (test t
	  (temporal-test 'contains-p 100 200 125 125))

    ))

(main)