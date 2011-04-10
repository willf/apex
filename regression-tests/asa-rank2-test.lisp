(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Rank Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-rank)))

(procedure 
 (index (test-rank))
 (step s1 (do a) (rank 0))
 (step s2 (do b) (rank -10))
 (step s3 (do c) (rank 1))
 (step s4 (do d) (rank 20))
 (step s4 (terminate) (waitfor ?s1 ?s2 ?s3 ?S4)))

(procedure
 (index (do ?x))
 (profile anon-resource) 
 (step s1 (add-result ?x))
 (step s2 (terminate) (waitfor ?s1)))

(primitive
 (index (add-result ?x))
 (profile anon-resource)
 (on-start 
  (format t "Entering ADD-RESULT with ?x=~S.~%" ?x)
  (setq *results* (append *results* (list ?x)))))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Rank.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(b a c d) *results* :test #'equal))
