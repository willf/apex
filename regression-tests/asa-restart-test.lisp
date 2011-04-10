(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Restart Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-restart)))

(procedure 
 (index (test-restart))
 (step s1 (wait) 
       (on-start 
                  (format t "Entering ADD-RESULT with ?x=~S.~%" 1)
                  (setq *results* (append *results* (list 1))))
       (on-end 
                (format t "Entering ADD-RESULT with ?x=~S.~%" 2)
                (setq *results* (append *results* (list 2))))
       (restart (when (stop)))
       )
 (step s2 (inform stop))
 (step s3 (add 3) (waitfor ?s2))
 (step s4 (add 4) (waitfor ?s3))
 (step s5 (add 5) (waitfor ?s4))
 (step s6 (terminate) (waitfor ?s5)))

(primitive
 (index (inform stop))
 (duration (200 ms))
 (on-completion
  (inform `(stop))))

(primitive
 (index (add ?x))
 (profile anon-resource)
 (on-start 
  (format t "Entering ADD-RESULT with ?x=~S.~%" ?x)
  (setq *results* (append *results* (list ?x)))))


(primitive
 (index (wait))
 (duration (10000 ms)))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Restart.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(1 1 3 4 5 2) *results* :test #'equal))
