;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-sequential-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-sequential-test.lisp,v 1.7 2006/01/15 03:42:59 dalal Exp $

(in-package :user)

(defvar *results* NIL)

(defapplication "ASA Test Worlds: Sequential Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-seq)))


(procedure  :sequential
 (index (test-seq))
 (do a)
 (do b)
 (do c))

(procedure
 (index (do ?x))
 (profile anon-resource) 
 (step s1 (add-result ?x))
 (step s2 (terminate) (waitfor ?s1)))

(primitive
 (index (add-result ?x))
 (profile anon-resource)
 (on-start 
  ;; (format t "Entering ADD-RESULT with ?x=~S.~%" ?x)
  (setq *results* (append *results* (list ?x)))))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Sequential.")
  (apex.utility.unit-test:test 'done (startapp))
  (apex.utility.unit-test:test  '(a b c) *results* :test #'equal))
