;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/misc-monitors-function.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-monitors-function.lisp,v 1.3 2006/01/15 03:42:59 dalal Exp $
;;;
;;;

(defparameter *functional-test-count* 0)
(defparameter *functional-test-ok* nil)


(primitive (increment test count)
  (duration P10s)
  (on-completion
   (incf  *functional-test-count*)))

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

(procedure (test function monitor)
  (step (increment test count ten times))
  (step p1 (do-nada) (waitfor (:function (>= *functional-test-count* 1))))
  (step p2 (do-nada) (waitfor (:function (>= *functional-test-count* 5))))
  (step p3 (do-nada) (waitfor (:function (>= *functional-test-count* 10))))
  (step (terminate)
	(waitfor ?p1 ?p2 ?p3)
	(on-start 
	 (setq *functional-test-ok* t))))

(defapplication "Functional Monitor Test"
    :init (init-sim))

(defun init-sim ()
  (setq *functional-test-ok* nil)
  (setq *functional-test-count* 0)
  (make-instance 'agent
    :name "Fn"
    :locale (make-instance 'locale)
    :initial-task '(test function monitor)))

(apex.utility.unit-test:with-tests (:name "Monitor Functional Test")
  (apex.utility.unit-test:test 'done (startapp))
  (apex.utility.unit-test:test t *functional-test-ok*))
