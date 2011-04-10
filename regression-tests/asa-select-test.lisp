;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/asa-select-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: asa-select-test.lisp,v 1.3 2006/01/15 03:42:59 dalal Exp $

(in-package :user)

(defvar *results* NIL)

(defapplication "ASA Test Worlds: Select Testing"
    :init (initialize-sim))


(defun button-height (button)
  (ecase button
    (high-button 100)
    (low-button 20)))

(defun initialize-sim ()
  (setq *results* '())
  (make-instance 'agent 
    :locale (make-instance 'locale)
    :initial-task '(test-select)))

(primitive (press ?button with ?extremity ?correct)
  (on-completion 
   (progn
     (apex.utility.unit-test:test ?extremity ?correct))))

(procedure :seq 
  (index (test-select))
  (step (press high-button with ?extremity hand)
	(select ?extremity (if (> (button-height 'high-button) 50) 'hand 'foot)))
  (step (press low-button with ?extremity foot)
	(select ?extremity (if (> (button-height 'low-button) 50) 'hand 'foot))))


(apex.utility.unit-test:with-tests (:name "ASA Test World: Select.")
  (apex.utility.unit-test:test 'done (startapp)))