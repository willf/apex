;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/misc-monitors-timepoint.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-monitors-timepoint.lisp,v 1.3 2006/01/15 03:42:59 dalal Exp $


(defparameter *timepoint-test-count* 0)
(defparameter *timepoint-test-ok* nil)


(primitive (increment test count)
  (duration P1s)
  (on-completion
   (incf  *timepoint-test-count*)))

(procedure (increment test count ten times)
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  (step (increment test count))
  )
  
(procedure :seq (do-nada))

(primitive (print ?x) (on-start (print ?x)))

(procedure (test timepoint monitor)
  (step (increment test count ten times))
  (step p1 (print "step one") (waitfor (:timestamp (+ (start-of +this-task+) P1s))))
  (step p2 (print "step two") (waitfor t1 (:timestamp (+ (start-of +this-task+) P5s))))
  (step p3 (print "step three") (waitfor t2 (:timestamp (+ (start-of +this-task+) P10s) (+ (start-of +this-task+) P20s))))
  (step p4 (print "step four") (waitfor (:timestamp  P100s)))
  (step (terminate)
	(waitfor ?p1 ?p2 ?p3 ?p4)
	(on-start 
	 (setq *timepoint-test-ok* t))))

(defapplication "Timepoint Monitor Test"
    :init (init-sim))

(defun init-sim ()
  (setq *timepoint-test-ok* nil)
  (setq *timepoint-test-count* 0)
  (make-instance 'agent
    :name "Fn"
    :locale (make-instance 'locale)
    :initial-task '(test timepoint monitor)))

(apex.utility.unit-test:with-tests (:name "Monitor Timepoint Test")
  (apex.utility.unit-test:test 'done (startapp))
  (apex.utility.unit-test:test t *timepoint-test-ok*))
