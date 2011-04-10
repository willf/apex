(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Resume Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-resume)))

(procedure 
 (index (test-resume))
 (step s1 (wait) 
       (on-start 
                  (format t "Entering ADD-RESULT with ?x=~S.~%" 1)
                  (setq *results* (append *results* (list 1))))
       (on-end 
                (format t "Entering ADD-RESULT with ?x=~S.~%" 2)
                (setq *results* (append *results* (list 2))))
       (resume (when (start)))
      
       )
 (step s2 (suspend ?s1))
 (step s3 (add 3) (waitfor ?s2))
 (step s4 (inform start) (waitfor ?s2))
 (step s5 (add 4) (waitfor ?s3))
 (step s6 (add 5) (waitfor ?s5))
 (step s7 (terminate) (waitfor ?s6)))

(primitive
 (index (inform stop))
 (duration (200 ms))
 (on-completion
  (inform `(stop))))

(primitive
 (index (inform start))
 (duration (200 ms))
 (on-completion
  (inform `(start))))

(primitive
 (index (add ?x))
 (profile anon-resource)
 (duration (100 ms))
 (on-start 
  (format t "Entering ADD-RESULT with ?x=~S.~%" ?x)
  (setq *results* (append *results* (list ?x)))))


(primitive
 (index (wait))
 (duration (1000 ms)))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Resume.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(1 3 4 5 1 2) *results* :test #'equal))
