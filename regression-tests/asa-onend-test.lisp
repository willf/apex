(in-package :user)


(defvar *results* NIL)

(defapplication "ASA Test Worlds: On-end Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-onstart)))

(procedure 
 (index (test-onstart))
 (step s1 (wait) (on-end   
  (setq *results* (append *results* (list 1)))))
 (step s2 (wait) (on-end   
  (setq *results* (append *results* (list 4))))(waitfor ?s1))
 (step s3 (wait) (on-end   
  (setq *results* (append *results*  (list 5))))(waitfor ?s2))
 (step s4 (terminate) (waitfor ?s3)))

(primitive
 (index (wait)))

(apex.utility.unit-test:with-tests (:name "ASA Test World: On-end testing.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "The results are ~a~%" *results*)
  (apex.utility.unit-test:test '(1 4 5) *results* :test #'equal))
