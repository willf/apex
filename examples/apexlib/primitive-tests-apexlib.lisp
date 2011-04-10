;;; apex/examples/apexlib/primitive-tests.lisp
;;; Test suite for primitive procedures.

(procedure :sequential
  (index (main))
  (main1))

(procedure :sequential                  ; Top level test procedure
 (index (main1))
 (non-durative-test)
 (returnval-test)
 (duration-tests)
 (update-tests)
 (resource-tests)
 (end-application)
 )

(procedure :sequential                  ; Tests UPDATE clause
 (index (update-tests))
 (concurrent-update-tests)
 (update-with-termination))

(procedure                              ; Tests simultaneously updating tasks.
 (index (concurrent-update-tests))
 (step s1 (update-test one))
 (step s2 (update-test two))
 (step s3 (terminate) (waitfor ?s1 ?s2))
 )

(procedure :sequential                  ; Tests resources
 (index (resource-tests))
 ;;; (profile right-hand)
 ;; How to test a property of hand?
 (pick-up with right-hand))

(procedure :sequential
 (index (duration-tests))
 (simple-duration)
 (computed-duration 4))

(procedure                              ; Tests return values from primitive
 (index (returnval-test))
 (step s1 (compute-square 5 => ?sq5))
 (step s2 (compute-square 3 => ?sq3))
 (step t1 (wait 1 and return 1 => ?time10))
 (step t2 (wait 2 and return 3 => ?time30))
 (step s3 (print (+ ?sq3 ?sq5 ?time10 ?time30)) (waitfor ?s1 ?s2 ?t1 ?t2))
 (step s4 (terminate) (waitfor ?s3)))

(primitive                              ; Return the square
 (index (compute-square ?x))
 (return (* ?x ?x)))

(primitive                              ; Return ?y after ?n seconds
 (index (wait ?n and return ?y))        
 (duration (?n secs))
 (return ?y))

(primitive                              ; A zero duration activity
 (index (non-durative-test)) 
 (on-start (message "non-durative-test did its thing")))

(primitive                              ; Fixed duration activity with
                                        ; completion action.
 (index (simple-duration))
 (duration (2 secs))
 (on-completion (message "simple-duration completed")))

(primitive                              ; Computed duration activity with
                                        ; completion action.
 (index (computed-duration ?x))
 (duration (list (compute-a-duration ?x) 'ms))
 (on-completion (message "computed-duration completed: ~a"
                         (compute-a-duration ?x))))

(defun compute-a-duration (x)
  (* 2 x))

(primitive                              ; Updating activity with fixed duration
 (index (update-test ?name))
 (update (100 ms) (note-update ?name))
 (duration (2 secs)))

(primitive                              ; Updating activity with self-termination
                                        ; (also tests attributes)
 (index (update-with-termination))
  (locals
   (count 0)
   (name (format nil "~a~a" "Fr" "ed")))
  (on-start
   (setq count 0))
  (update (100 ms)
   (note-update "update-with-termination")
   (incf count)
   (when (> count 5)
     (setq name (format nil "~a ~a" "Hello, " name))
     (complete)))
  (on-completion
   (message "Count=~a" count)
   (message "~a" name)))

(primitive                              ; tests profile clause
 (index (pick-up with ?hand))
 (profile ?hand)
 (duration (300 ms)))


;;; Some supporting Lisp functions

(defun message (str &rest args)
  (format t "~a~%" (apply #'format (append (list nil str) args))))

(defun note-update (name)
  (message "  updating ~a" name))
