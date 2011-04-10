;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/goms/atm/klm.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: klm.lisp,v 1.4 2006/01/15 03:42:54 dalal Exp $

;;; Example application: Withdrawal of money from at ATM using the KLM
;;; approach.

;;; KLM stands for Keystroke Level Model.  It is an alternative form of
;;; GOMS that attaches no meaning or value to a goal hierarchy, treating
;;; a flat list of operator-level goals as the essential units of task
;;; analysis.  (The Apex version generates this from a hierarchy, but
;;; that is not a necessary part of the method) As with CMN-GMS, most
;;; operator-level goals correspond to interface manipulation actions.

(in-package :user)

(defapplication "ATM-KLM World"
    :libraries ("atm")
    :files ("common.lisp")
    :init (initialize-klm))

;;; Initialization of simworld.  

(defun initialize-klm ()
  (let* 
      ((bank (make-instance 'locale :name 'bank))
       (human (make-instance 'human :name 'agent :locale bank
                             :initial-task '(get 80 from atm)
                             :location '(0 0 22)))
       (atm (make-instance 'atm :name 'atm :locale bank 
                           :pos '(0 0) :dimensions '(182 164)))
       (money-slot (make-instance 'money-slot :name 'money-slot :locale bank
                                  :pos '(11 9) :dimensions '(98 2)))
       (screen (make-instance 'screen :name 'screen :locale bank
                              :pos '(21 23) :dimensions '(108 68)))
       (checking (make-instance 'button :name 'checking-key :locale bank
                                :pos '(131 51) :dimensions '(15 11)))
       (withdraw (make-instance 'button :name 'withdraw-key :locale bank
                                :pos '(131 66) :dimensions '(15 11)))
       (correct (make-instance 'button :name 'correct-key :locale bank
                                :pos '(131 66) :dimensions '(15 11)))
       (no (make-instance 'button :name 'no-key :locale bank
                                :pos '(131 81) :dimensions '(15 11)))
       (keypad (create-keypad '(43 104) '(13 11) '(4 2) 'down bank))
       (ok (make-instance 'button :name 'ok-key :locale bank
                                :pos '(95 129) :dimensions '(15 11)))
       (card-slot (make-instance 'card-slot :name 'card-slot :locale bank
                                 :pos '(131 127) :dimensions '(43 2)))
       (mouse (create-mouse bank))
       )
    ;; assemble top level objects
    (mapc #'assemble (list human atm))  
    ;; assemble components of ATM
    (mapc #'(lambda (obj) (assemble obj :component-of (component-set atm)))
          (list screen keypad card-slot money-slot
                withdraw checking correct mouse no ok))
    (display-handler screen '(insert card))))

;;; Top level procedure

(procedure
 (index (get ?amt from atm))
 (step s1 (initiate ATM session) (rank 1))
 (step s2 (withdraw ?amt from atm) (rank 2))
 (step s3 (end ATM session) (rank 3))
 (step s4 (end-trial) (waitfor ?s1 ?s2 ?s3)))

(procedure
 (index (initiate ATM session))
 (step m1 (mentally-prepare initiate-session))
 (step p1 (move-cursor card-slot) (waitfor ?m1))
 (step k1 (mouse-click card-slot) (waitfor ?p1))
 (step p2 (move-cursor key-4) (waitfor ?k1))
 (step k2 (mouse-click key-4) (waitfor ?p2))
 (step p3 (move-cursor key-9) (waitfor ?k2)) 
 (step k3 (mouse-click key-9) (waitfor ?p3))
 (step p4 (move-cursor key-0) (waitfor ?k3))
 (step k4 (mouse-click key-0) (waitfor ?p4))
 (step p5 (move-cursor key-1) (waitfor ?k4))
 (step k5 (mouse-click key-1) (waitfor ?p5))
 (step p6 (move-cursor OK-key) (waitfor ?k5))
 (step k6 (mouse-click OK-key) (waitfor ?p6))
 (step t (terminate) (waitfor ?k6)))

(procedure
 (index (withdraw ?amt from atm))
 (step m1 (mentally-prepare do-transaction))
 (step p1 (move-cursor withdraw-key) (waitfor ?m1))
 (step k1 (mouse-click withdraw-key) (waitfor ?p1))
 (step p2 (move-cursor checking-key) (waitfor ?k1))
 (step k2 (mouse-click checking-key) (waitfor ?p2))
 ;; todo: synthesize this sequence at runtime
 (step p3 (move-cursor key-8) (waitfor ?k2)) 
 (step k3 (mouse-click key-8) (waitfor ?p3))
 (step p4 (move-cursor key-0) (waitfor ?k3))
 (step k4 (mouse-click key-0) (waitfor ?p4))
 (step p5 (move-cursor correct-key) (waitfor ?k4))
 (step k5 (mouse-click correct-key) (waitfor ?p5))
 (step p6 (move-cursor money-slot) (waitfor ?k5))
 (step k6 (mouse-click money-slot) (waitfor ?p6))
 (step t (terminate) (waitfor ?k6)))

(procedure
 (index (end ATM session))
 (step m1 (mentally-prepare do-transaction))
 (step p1 (move-cursor no-key) (waitfor ?m1))
 (step k1 (mouse-click no-key) (waitfor ?p1))
 (step p2 (move-cursor card-slot) (waitfor ?k1))
 (step k2 (mouse-click card-slot) (waitfor ?p2))
 (step p3 (move-cursor money-slot) (waitfor ?k2)) 
 (step k3 (mouse-click money-slot) (waitfor ?p3))
 (step t (terminate) (waitfor ?k3)))

(procedure :sequential
 (index (mentally-prepare ?activity))
 (use resource memory for (1350 ms)))

(procedure :sequential
  (index (move-cursor ?target))
  (mouse-time ?target => ?time)
  (move-mouse to ?target with right-hand taking ?time))

(procedure :sequential
  (index (mouse-click ?target))
  (mouse-object => ?object)
  (mouse-click ?object with right-hand taking (200 ms)))

