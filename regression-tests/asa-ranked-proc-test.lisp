;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-rank-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-ranked-proc-test.lisp,v 1.13 2006/03/17 18:44:46 will Exp $

(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Rank Testing: $RCSfile: asa-ranked-proc-test.lisp,v $"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-rank)))

(primitive (test-result ?probe)
  (on-start
   (apex.utility.unit-test:test *results* ?probe :test #'equal)))

(primitive (clear)
  (on-start (setq *results* '())))

(procedure :seq 
  (index (test-rank))
  (step (clear))
  (step (test-rank-1))
  (step (test-result (a b c)))
  (step (clear))
  (step (test-rank-2))
  (step (test-result (a b c)))
  (step (clear))
  (step (test-rank-3))
  (step (test-result (a b c)))
  (step (clear))
  (step (test-rank-4))
  (step (test-result (a b c)))
  )
  
  
(procedure :ranked
 (index (test-rank-1))
 (step s1 (do a))
 (step s2 (do b))
 (step s3 (do c)))

(procedure 
    (proctype ranked)
 (index (test-rank-2))
 (step s1 (do a))
 (step s2 (do b))
 (step s3 (do c)))

(procedure :soft-seq
 (index (test-rank-3))
 (step s1 (do a))
 (step s2 (do b))
 (step s3 (do c)))

(procedure :soft-sequential
 (index (test-rank-4))
 (step s1 (do a))
 (step s2 (do b))
 (step s3 (do c)))


(procedure
 (index (do ?x))
 (profile anon-resource) 
 (step s1 (add-result ?x))
 (step s2 (terminate) (waitfor ?s1)))

(primitive
 (index (add-result ?x))
 (profile anon-resource)
 (on-start 
  ;(format t "Entering ADD-RESULT with ?x=~S.~%" ?x)
  (setq *results* (append *results* (list ?x)))))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Rank: $RCSfile: asa-ranked-proc-test.lisp,v $")
  (apex.utility.unit-test:test 'done (startapp)))
