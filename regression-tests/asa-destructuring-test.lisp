;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-destructuring-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-destructuring-test.lisp,v 1.2 2006/01/15 03:42:59 dalal Exp $

(in-package :user)


(defvar *result-list* NIL)

(defapplication "ASA Test Worlds: Destructuring Testing"
    :init (initialize-test))

(defun initialize-test ()
  (setq *result-list* '())
  (make-instance 'agent :locale (make-instance 'locale)))

(primitive (copies ?n ?object)
  (return (let ((co (loop for i from 1 to ?n collecting ?object)))
	    (format t "COPIES returning ~a~%" co)
	    co)))

(procedure 
 (index (do-domain))
 (step s0 (print "destructuring-test: should see (A) then (A A) then (A A A)"))
 (step s1 (do 1) (waitfor ?s0))
 (step s2 (do 2) (waitfor ?s1))
 (step s3 (do 3) (waitfor ?s2))
 (step s4 (terminate) (waitfor ?s3)))

(primitive
 (index (add-result ?x))
 (return (setq *result-list*
	    (append *result-list*
		    (list ?x)))))
(procedure
 (index (do 1))
 (profile brain)
 (step s0 (copies 1 A => (?a)))
 (step s1 (print (?a)) (waitfor ?s0))
 (step s2 (add-result (?a)) (waitfor ?s0))
 (step s3 (terminate) (waitfor ?s1 ?s2)))

(procedure
 (index (do 2))
 (profile brain)
 (step s0 (copies 2 A => (?a ?b)))
 (step s1 (print (?a ?b)) (waitfor ?s0))
 (step s2 (add-result (?a ?b)) (waitfor ?s0))
 (step s3 (terminate) (waitfor ?s1 ?s2)))

(procedure
 (index (do 3))
 (profile brain)
 (step s0 (copies 3 A => (?a ?b ?c)))
 (step s1 (print (?a ?b ?c)) (waitfor ?s0))
 (step s2 (add-result (?a ?b ?c)) (waitfor ?s0))
 (step s3 (terminate) (waitfor ?s1 ?s2)))

(primitive
 (index (print ?string))
 (return (progn (format t "~a~%" ?string) t)))

(apex.utility.unit-test:with-tests (:name "ASA Test World: Destructuring Tests.")
  (apex.utility.unit-test:test 'done (startapp))
  (format t "After running the application, the result list is ~S~%" *result-list*)
  (apex.utility.unit-test:test t (tree-equal '((a) (a a) (a a a)) *result-list*)))