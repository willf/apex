;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/simulation/test/test2.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: test2.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $


(in-package :common-lisp-user)

;;; Test Sim 2: one appob, three activities.

;;; ---- Appobs

;;; A simple Appob who's only semantics is a 'value' (integer).

(defclass foob (appob)
  ((value :initform 0 :accessor value)))
   
;;; A method to increase the value by a specified amount.

(defmethod increase ((f foob) n)  ; Foob * int -> ()
  (setx (value f) (+ (value f) n)))

;;; A basic constructor

(defun make-foob (name)   ; () -> Foob
  (make-instance 'foob :simulation *application* :name name))

;;; ---- Activities

;;; A simple Activity that increases a Foob's value over time.

(defclass foobing (activity) ())

;;; An update method that increases a Foob's value until it
;;; reaches 10, at which time the activity is complete.

(defmethod update-activity ((act foobing) (ob foob))
  ;;
  ;; Foobing * Foob -> ()
  ;;
  (let ((val (value ob)))
    (format t "Value of foob is ~a.~%" val)
    (if (< val 10)
      (increase ob 1)
      (complete-activity act ob))))

;;; A completion method that simply reports completion.

(defmethod complete-activity ((act foobing) (ob foob))
  ;;
  ;; Foobing * Foob -> ()
  ;;
  (format t "Foobing of ~a is finished.~%" ob))

;;; ---- Simulation Initialization

(initialize-simulation
  (let ((ob1 (make-foob "joe"))
        (ob2 (make-foob "sue"))
        (ob3 (make-foob "bob")))
    ;; Activity that takes zero time
    (start-activity ob1 'foobing :primary-object ob1
                    :duration 0)
    ;; Activity with fixed completion time
    (start-activity ob1 'foobing :primary-object ob1
                    :duration 10)
    ;; Activity with regular updates
    (start-activity ob2 'foobing :primary-object ob2
                    :update-interval 1)
    (start-activity ob3 'foobing :primary-object ob3
                    :update-interval 1)))

(do-before-all-trials
 (format t "Before all trials...~%"))

(do-after-each-trial
 (format t "Did a trial!~%"))

(do-after-all-trials
 (format t "Finished all trials!~%"))

