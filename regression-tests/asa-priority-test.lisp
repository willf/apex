;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-priority-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-priority-test.lisp,v 1.7 2006/01/15 03:42:59 dalal Exp $

(in-package :user)


(defvar *result-list* NIL)

(defapplication "ASA Test Worlds: Priority Testing"
    :init (initialize-sim))

(defun initialize-sim ()
  (setq *result-list* '())
  (make-instance 'agent :locale (make-instance 'locale)))

(procedure 
 (index (do-domain))
 (step s0 (print "priority-test: should see b then a then c"))
 (step s1 (do a) (priority 3) (waitfor ?s0))
 (step s2 (do b) (priority (+ 2 3)) (waitfor ?s0))
 (step s3 (do c) (priority 2) (waitfor ?s0))
 (step s4 (terminate) (waitfor ?s1 ?s2 ?s3)))

(primitive
 (index (add-result ?x))
 (return (setq *result-list*
	    (append *result-list*
		    (list ?x)))))
(procedure
 (index (do ?x))
 (profile brain)
 (step s1 (print ?x))
 (step s2 (add-result ?x))
 (step s3 (terminate) (waitfor ?s1 ?s2)))

(primitive
 (index (print ?string))
 (return (progn (format t "~a~%" ?string) t)))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Priority Tests.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "After running the application, the result list is ~S~%" *result-list*)
  (apex.utility.unit-test:test t (equal '(b a c) *result-list*)))