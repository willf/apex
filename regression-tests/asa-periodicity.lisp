;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-periodicity.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-periodicity.lisp,v 1.8 2006/01/15 03:42:59 dalal Exp $


;;; Tests of PERIOD (to be obsoleted), REPEATING, and RESPONDING clauses.

(in-package :user)

(defapplication "Periodicity Tests"
  :init (init-sim))

;;; Regression test status
(defvar *all-tests-passed*)

(defun init-sim ()
  (setq *all-tests-passed* t)
  (make-instance 'agent
    :name "Perry"
    :locale (make-instance 'locale)
;    :initial-task '(run one test))
    :initial-task '(run tests))
  (show stop)
  (show threw)
)

;;; The tests perform counting and manipulate/check this global variable.
(defvar *count*)

;;; Display updates of the count.
(defparameter *show-count* nil)


;;; NOTE: the commented out lines are tests that do not terminate
;;; successfully (or at all)

(procedure :sequential
  (index (run tests))
  (reset-count) (period-recurrent) 
  (reset-count) (period-recurrent-with-test) 
  (reset-count) (period-recurrent-reftime-enabled) 
  (reset-count) (period-recurrent-enabled-with-test) ; no output 
  (reset-count) (period-recurrent-recovery) 
  (reset-count) (period-recurrent-reftime-enabled-with-vars) 
  (reset-count) (repeating) 
  (reset-count) (repeating-while) 
  (reset-count) (repeating-until) 
  (reset-count) (repeating-until-while) 
  (reset-count) (responding) 
  (reset-count) (repeating-enabled) 
  (reset-count) (repeating-enabled-with-test) ; no output 
  (reset-count) (repeating-with-recovery) 
  (reset-count) (responding-unbinding)  
  (reset-count) (repeating-unbinding) 
  (reset-count) (repeating-with-min-interval) 
)

;;; PERIOD clause tests --------------------------------------------------------

(procedure (period-recurrent)
 (step (count to 10)
       (period :recurrent))
 (step (wrapup +this-task+ 10) (waitfor (stop))))

(procedure (period-recurrent-with-test)
 (step s1 (count to 10)
       (period :recurrent (< *count* 5)))
 (step (wrapup +this-task+ 5)
       (waitfor ?s1)))

(procedure (period-recurrent-reftime-enabled)
 (step (schedule-go)
       (period :recurrent))
 (step (count to 10)
       (waitfor (go))
       (period :recurrent :reftime enabled))
 (step (wrapup +this-task+ 10)
       (waitfor (stop))))

(procedure (period-recurrent-enabled-with-test)
 (step s0 (schedule-go))
 (step s1 (count to 10)
       (waitfor (go))
       (period :recurrent :enabled (< *count* 3)))
 (step (wrapup +this-task+ 3)
       (waitfor ?s1)))

(procedure (period-recurrent-recovery)
 (step (count to 10)
       (period :recurrent :recovery 'p1s))
 (step (wrapup +this-task+ 10)
       (waitfor (stop))))

(procedure (period-recurrent-reftime-enabled-with-vars)
 (step (throw a number)
       (period :recurrent))
 (step (count to 10)
       (waitfor (threw ?n))
       (on-start (inform-catch ?n))
       (period :recurrent :reftime enabled))
 (step (wrapup +this-task+ 10)
       (waitfor (stop))))


;;; REPEATING clause tests -----------------------------------------------------

(procedure (repeating)
 (step (count to 10)
       (repeating))
 (step (wrapup +this-task+ 10)
       (waitfor (stop))))

(procedure (repeating-while)
 (step s1 (count to 10)
       (repeating :while (< *count* 5)))
 (step (wrapup +this-task+ 5)
       (waitfor ?s1)))
       
(procedure (repeating-until)
 (step s1 (count to 10)
       (repeating :until (> *count* 4)))
 (step (wrapup +this-task+ 5)
       (waitfor ?s1)))

(procedure (repeating-until-while)
 (step s1 (count to 10)
       (repeating
        :while (< *count* 2)
        :until (> *count* 1)))
 (step (wrapup +this-task+ 2)
       (waitfor ?s1)))
             
(procedure (repeating-enabled)
 (step (schedule-go))
 (step (count to 10)
       (waitfor (go))
       (repeating :enabled))
 (step (wrapup +this-task+ 10)
       (waitfor (stop))))

(procedure (repeating-enabled-with-test)
 (step s1 (schedule-go))
 (step s2 (count to 10)
       (waitfor ?s1 (go))
       (repeating :enabled (< *count* 3)))
 (step (wrapup +this-task+ 3)
       (waitfor ?s2)))

(procedure (repeating-with-recovery)
 (step (count to 10)
       (repeating :with-recovery 'p1s))
 (step (wrapup +this-task+ 10)
       (waitfor (stop))))

(procedure (repeating-with-min-interval)
 (step (count to 10)
       (repeating :with-min-interval 'p1s))
 (step (wrapup +this-task+ 10)
       (on-end
        (format t "CHECK that task start times are at least 1 second apart~%"))
       (waitfor (stop))))

(procedure (repeating-unbinding)
 (step (throw a number)
       (repeating))
 (step (count to 5)
       (waitfor (threw ?n))
       (on-start (inform-catch ?n))
       (repeating :for-new ?n))
 (step (wrapup +this-task+ 5)
       (waitfor (stop))))


;;; RESPONDING clause tests ----------------------------------------------------

(procedure (responding)
 (step (schedule-go)
       (repeating))
 (step (count to 10)
       (waitfor (go))
       (responding))
 (step (wrapup +this-task+ 10)
       (waitfor (stop))))

(procedure (responding-unbinding)
 (step (throw a number)
       (repeating))
 (step (count to 5)
       (waitfor (threw ?n))
       (on-start (inform-catch ?n))
       (responding :for-new ?n))
 (step (wrapup +this-task+ 5)
       (waitfor (stop))))


;;; Supporting procedures  -----------------------------------------------------

(procedure :sequential
 (index (throw a number))
 (primitive throw a number))

(primitive
    (index (primitive throw a number))
  (duration (100 ms))
  (on-start (let ((r (random 10)))
	      (inform `(threw ,r)))))
 
(primitive
 (index (count to ?n))
 (duration (10 ms))
 (on-start
  (if *show-count* (format t "Count = ~a~%" *count*))
  (if (>= *count* ?n)
      (inform '(stop))
    (incf *count*))))

(primitive
    (index (schedule-go))
  (duration (10 ms))
 (profile foo) ;; needed to enable repitition
 (on-start (inform '(go))))

(procedure :sequential (wrapup ?task ?count)
  (step (verify-count ?count ?task))
  (step (reset-count))
  (step (terminate ?task)))

(primitive
 (index (verify-count ?val ?task))
 (on-start
  (if (= *count* ?val)
      (format t "Passed ~a~%" (index (proc ?task)))
    (progn
      (format t "Failed ~a~%" (index (proc ?task)))
      (format t "  Count was ~a but expected ~a~%" *count* ?val)
      (setq *all-tests-passed* nil)))))

(primitive (reset-count)
  (on-start (setq *count* 0)))

(defun inform-catch (n)
;;; BUG: substituting this will cause non-termination.
;;;  (inform `(caught ,n)))
  (format t "Caught ~a~%" n))

;;; Misc

;;; Convenience for running one test alone.
(procedure :sequential
  (index (run one test))
  (reset-count) (responding-unbinding))


(if (and (boundp '*regression-testing*) *regression-testing*)
    (apex.utility.unit-test:with-tests (:name "Periodicity Tests.")
      (apex.utility.unit-test:test 'done (startapp))
      (apex.utility.unit-test:test t *all-tests-passed*)))
