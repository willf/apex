;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/simulation/test/test1.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: test1.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $


(in-package :common-lisp-user)

;;; Test Sim 1: one (unnamed) appob, one activity.

;;; Specifically tests (among other things):
;;;   - schedule-completion method

;;; ---- Appobs

;;; A simple Appob who's only semantics is a 'value' (integer).

(defclass foob (appob)
  ((value :initform 0 :accessor value)))
   
;;; A method to reset the value to 0.

(defmethod clear ((f foob))  ; Foob -> ()
  (setx (value f) 0))

;;; A method to increase the value by a specified amount.

(defmethod increase ((f foob) n)  ; Foob * int -> ()
  (setx (value f) (+ (value f) n)))

;;; A basic constructor

(defun make-foob ()   ; () -> Foob
  (make-instance 'foob :simulation *application*))

;;; ---- Activities

;;; A simple Activity that increases a Foob's value over time.

(defclass foobing (activity) ())

;;; An update method that increases a Foob's value until it
;;; reaches 10, at which time a completion for the activity is
;;; scheduled (15 time units from now).

(defmethod update-activity ((act foobing) (ob foob))
  ;;
  ;; Foobing * Foob -> ()
  ;;
  (let ((val (value ob)))
    (format t "Value of foob is ~a.~%" val)
    (if (< val 10)
      (increase ob 1)
      (schedule-completion act '(15 ms)))))

;;; A completion method that simply reports completion.

(defmethod complete-activity ((act foobing) (ob foob))
  ;;
  ;; Foobing * Foob -> ()
  ;;
  (format t "Foobing is finished.~%"))

;;; ---- Simulation Initialization

(initialize-simulation
  (let ((ob (make-foob)))
    (start-activity ob 'foobing :primary-object ob
                    :update-interval 1)))
