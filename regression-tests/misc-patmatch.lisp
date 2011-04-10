;;;-*- Mode: Lisp; Package:  :apex.utility.patmatch.test -*-
;;;
;;; apex/regression-tests/misc-patmatch.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-patmatch.lisp,v 1.4 2006/01/15 03:42:59 dalal Exp $



(defpackage :apex.utility.patmatch.test
  (:use :common-lisp
	:apex.utility.patmatch
	:apex.utility.unit-test)
  (:export #:main))

(in-package :apex.utility.patmatch.test)

(defun the-number-one (this-task) 1)
(defun the-bee-who-is-following-me-with-angry-eyes (this-task) (the-number-one this-task))

;;; these tests assume that indexical-functional variables have been defined.
;;; as a function of a task.

(defun main ()
  (with-tests (:name "Pattern Matching")
    (test t (variable-p '?test))
    (test :free (variable-type '?test))
    (test t (variable-p '<?test>))
    (test :bound (variable-type '<?test>))
    (test t (variable-p '+the-bee-who-is-following-me-with-angry-eyes+))
    (test :indexical-functional (variable-type '+the-bee-who-is-following-me-with-angry-eyes+))
    (test nil (variable-p 'test))
    (test-error (variable-type 'test))
    (test nil (variable-p "test"))
    (test-error (variable-type "test"))
    (test t (and (pat-match '?test 'one) t))
    (test t (and (pat-match '<?test> 'one) t))
    (test t (and (pat-match '(?test ?test two) '(one one two)) t))
    (test t (and (pat-match '(?test <?test> two) '(one one two)) t))    
    (test t (and (pat-match '(<?test> ?test two) '(one one two)) t)) 
    (test t (and (pat-match
		  '(a (b c) (d e))
		  '(a (b c) (d e)))
		 t))
    (test t (and (pat-match
		  '(a (?b c) (?d e))
		  '(a (b c) (d e)))
		 t))
    (test t (and (pat-match '(+the-number-one+ 2 3) '(1 2 3)) t))
    (test t (and (pat-match '(+the-bee-who-is-following-me-with-angry-eyes+ 2 3) '(1 2 3)) t))    
    
    ))

(main)

