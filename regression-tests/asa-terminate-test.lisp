(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: Terminate Testing"
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
       (terminate (when (stop)))
      
       )
 
 (step s3 (add 3))
 (step s4 (inform stop) (waitfor ?s3))
 (step s5 (add 4) (waitfor ?s4))
 (step s6 (add 5) (waitfor ?s5))
 (step s7 (terminate) (waitfor ?s6)))

(primitive
 (index (inform stop))
 (duration (50 ms))
 (on-completion
  (format t "Informing stop~%")
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
 (duration (2000 ms)))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Terminate.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(1 3 2 4 5) *results* :test #'equal))
