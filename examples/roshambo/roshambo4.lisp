;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/roshambo/roshambo4.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: roshambo4.lisp,v 1.3 2006/01/15 03:42:54 dalal Exp $

(in-package :cl-user)

(defapplication "Roshambo 4"
    :init (initialize-sim))

(defun initialize-sim ()
  (let ((locale (make-instance 'locale :name 'gameroom)))
    (make-instance 'agent
      :name "Jill"
      :locale locale
      :use-bundles '(:paper-strategy)
      :initial-task '(practice roshambo))
    (make-instance 'agent
      :name "Jack"
      :locale locale
      :use-bundles '(:rock-strategy)
      :initial-task '(practice roshambo))))

(procedure :sequential 
  (index (practice roshambo))
  (step (prime))
  (step (choose gesture => ?my-gesture))
  (step (gesture <?my-gesture>)))

(procedure :seq (index (prime)))

(primitive (index (gesture ?gesture))
  (profile hand)
  (duration (500 ms))
  (on-completion (inform `(gestured ,+self+ ,?gesture))))

(in-apex-bundle :rock-strategy)

(primitive (choose gesture) ;; always choose rock
  (profile brain)
  (duration (500 ms))
  (return 'rock))


(in-apex-bundle :paper-strategy)

(primitive (choose gesture) ;; always choose paper
  (profile brain)
  (duration (500 ms))
  (return 'paper))

