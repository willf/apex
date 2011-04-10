(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Select Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-select)))

(procedure 
 (index (test-select))
 (step s1 (select 1 2 11))
 (step s2 (select 3 4 5 ) (waitfor ?s1))
 (step s3 (select 5 6 12) (waitfor ?s2))
 (step s4 (terminate) (waitfor ?s1 ?s2 ?s3)))

(procedure
 (index (select ?a ?b ?c))
 (profile anon-resource) 
 (step s1 (add-result ?x) (select ?x (if (> ?c 10) ?a ?b)))
 (step s2 (terminate) (waitfor ?s1)))

(primitive
 (index (add-result ?x))
 (profile anon-resource)
 (on-start 
  (format t "Entering ADD-RESULT with ?x=~S.~%" ?x)
  (setq *results* (append *results* (list ?x)))))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Select.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(1 4 5) *results* :test #'equal))
