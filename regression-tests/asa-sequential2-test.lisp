(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Proctype Sequential Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-sequential)))

(procedure (proctype sequential)
 (index (test-sequential))
 (step s1 (do a))
 (step s2 (do b))
 (step s3 (do c))
 (step s4 (do d)))


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

(apex.utility.unit-test:with-tests (:name "ASA Test World: Proctype Sequential.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(a b c d) *results* :test #'equal))
