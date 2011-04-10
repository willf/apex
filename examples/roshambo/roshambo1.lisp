;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/roshambo/roshambo1.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: roshambo1.lisp,v 1.3 2006/01/15 03:42:54 dalal Exp $

(in-package :cl-user)

(defapplication "Roshambo 1"
    :init (initialize-sim))

(defun initialize-sim ()
  (make-instance 'agent
    :name "Jill"
    :locale (make-instance 'locale :name 'gameroom)
    :initial-task '(practice roshambo)))

(procedure :sequential 
  (index (practice roshambo))
  (step (prime))
  (step (make game gesture)))

(procedure :seq
  (index (make game gesture))
  (step (gesture rock)))

(procedure :seq (index (prime)))

(primitive (index (gesture ?gesture))
  (profile hand)
  (duration (500 ms))
  (on-completion (inform `(gestured ,+self+ ,?gesture))))

