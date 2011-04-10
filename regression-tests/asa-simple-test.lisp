;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-simple-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-simple-test.lisp,v 1.6 2006/01/15 03:42:59 dalal Exp $
;;; Description: Tests using Test facility with Apex

(in-package :user)
(setf *print-readably* nil)
(setq *print-miser-width* NIL)
(setq *print-lines* NIL)
(setq *print-right-margin* NIL)

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
  (step final (terminate) 
	(waitfor ?s1)))

(procedure :sequential (index (test-1))
  (test-value 1 1 =)
  (test-value a a eql)
  (test-value (1 2 3) (1 2 3) equal)
  (test-value "Mike" "mike" string-equal)
  (+ 1 3 => ?added)
  (test-value 4 ?added =))

(primitive (index (+ ?a ?b))
  (return (+ ?a ?b)))

(apex.utility.unit-test:with-tests (:name "ASA Test: Simple")
  (apex.utility.unit-test:test 'done (startapp)))
