;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-primitive-return-val.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-primitive-return-val.lisp,v 1.6 2006/01/15 03:42:59 dalal Exp $
;;; Description: Tests using Test facility with Apex


(in-package :user)

(defapplication "ASA Test Worlds: Basic"
    :init (initialize-sim))

(defun initialize-sim ()
  (make-instance 'agent
    :name "T. Test"
    :locale (make-instance 'locale :name "Cyberspace")
    :initial-task '(run-tests)))

(primitive (index (test-value ?expected ?got ?test))
  (profile test-variables)
  (on-start (apex.utility.unit-test:test  ?expected ?got  :test ?test)))

(procedure (index (run-tests))
  (step s1 (test-1))
  (step s2 (test-2) (waitfor ?s1))
  (step final (terminate) 
	(waitfor ?s2)))

(procedure :sequential (index (test-1))
  (+ 1 3 => ?added)
  (test-value 4 ?added =))

(procedure :sequential (index (test-2))
  (just return 22 => ?val)
  (test-value 22 ?val =))

(primitive (index (+ ?a ?b))
  (locals (sum 0))
  (on-start (setq sum (+ ?a ?b)))
  (return sum)
  )

;;; Tests local used for reference only
(primitive (index (just return ?val))
  (locals (retval ?val))
  (return retval)
  )

(apex.utility.unit-test:with-tests (:name "ASA Test: Primitive Return Value")
  (apex.utility.unit-test:test-no-error (startapp)))
