(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Priority Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-priority)))

(procedure 
 (index (test-priority))
 (step s1 (do a) (priority -10))
 (step s2 (do b) (priority -5))
 (step s3 (do c) (priority 2))
 (step s4 (do d) (priority 20))
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

(apex.utility.unit-test:with-tests (:name "ASA Test World: Priority.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(d c b a) *results* :test #'equal))