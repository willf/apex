;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-ranked-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-ranked-test.lisp,v 1.7 2006/01/15 03:42:59 dalal Exp $

(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Ranked Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-ranked)))

(procedure  :ranked
 (index (test-ranked))
 (do a)
 (do b)
 (do c))

(procedure
 (index (do ?x))
 (profile anon-resource) ;; note -- a resource to contend for is needed for rank to take effect.
 (step s1 (add-result ?x))
 (step s2 (terminate) (waitfor ?s1)))


(primitive
 (index (add-result ?x))
 (on-start 
  (format t "Entering asa-ranked add-result with ?x=~S~%" ?x)
  (setq *results* (append *results* (list ?x)))))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Ranked.")
  (apex.utility.unit-test:test 'done (ignore-errors (startapp)))
  (format t "At end, results are ~a~%." *results*)
  (apex.utility.unit-test:test '(a b c) *results* :test #'equal))
