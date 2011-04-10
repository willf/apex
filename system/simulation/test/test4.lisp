;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/simulation/test/test4.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: test4.lisp,v 1.3 2006/01/15 03:43:02 dalal Exp $


(in-package :common-lisp-user)

;;; Test file

;;; What is tested includes...
;;; - multiple activities updating concurrently
;;; - activity with both completion time and update
;;; - activity class with no defined completion method
;;; - functional activity time specs (update-interval)
;;; - multiple trials incl. after-each-trial, after-all-trials meths

;;; Need to test...
;;; - starting activities after simulation init
;;; - if activity scheduled for simultaneous completion and update,
;;;   update should occur, then completion (unless explicitly stopped)
;;; - pause presets (add in abs-time preset capability)

(defvar *verbose-test* t)  ;; t means show init, updates, completes

;;; ------
;;; Model: Initialization creates one food object, one diner object,
;;; and two activities -- cooling of the food and eating of the food
;;; by the diner.  Eating can terminate for any of 4 reasons: the
;;; diner eats enough to satisfy hunger, the diner eats all the food,
;;; the food cools to room temperature (becoming inedible), and the
;;; time allotted to eat runs out.  The food-amount and allotted-time
;;; are varied randomly over a series of trials. At the end of all 
;;; trials, the simulation reports the number of times eating
;;; completed for each completion condition.


;;; ---- Appob and activity definitions

(defclass physobj (appob)
  ((temp :accessor temp :initarg :temp)
   (mass :accessor mass :initarg :mass)))

(defclass hotfood (physobj)
  ((temp :initform 100 :initarg :temp
         :accessor temp)))  

(defclass cooling (activity)
  ((rate :initarg :rate :accessor rate)))  ;; rate of decline of temp

(defmethod initialize-activity ((cooling cooling) (physobj physobj))
  (declare (ignore physobj))
  (format t "Cooling initialized!~%"))

(defmethod update-activity ((cooling cooling) (physobj physobj))
  (cond ((= (temp physobj) 0) 
         ;; at room temp
         (signal-event (complete-activity cooling physobj)) )
        (t (setx (temp physobj) 
                 (max 0 (- (temp physobj) (rate cooling)))
                 )))  ;; reduce temp
  (if *verbose-test*
      (format t "(~a) ~a temp reduced to ~a~%" (current-time) 
              physobj (temp physobj))))

(defclass diner (appob)
  ((hunger :initarg :hunger :accessor hunger)))  ;; max mass to eat

(defclass eating (activity)
  ((food :accessor food :initarg :food)  ;; pointer to what is being eaten
   (rate :accessor rate :initarg :rate))) ;; mass consumed / update

(defmethod initialize-activity ((act eating) (obj diner))
  (format t "Looks delicious!~%"))

(defmethod update-activity ((eating eating) (diner diner))
  (let* ((food (food eating))
         (amt (min (mass food) (rate eating)))) ;; how much to eat
    (signal-event (reduce-mass food amt))
    (setx (hunger diner) (- (hunger diner) amt))
    (when *verbose-test*
      (format t "(~a) Food mass = ~a  temp = ~a" (current-time)
              (mass food) (temp food))
      (format t "  Diner hunger = ~a~%" (hunger diner)))
    (cond    ;note: only one result reported even if several would apply
     ((= (mass food) 0)
      (log-result 'no-more)
      (signal-event (stop-activity eating)))
     ((<= (hunger diner) 0)
      (log-result 'full)
      (signal-event (stop-activity eating)))
     ((= 0 (temp food))
      (log-result 'cold)
      (signal-event (stop-activity eating))))))

(defmethod complete-activity ((eating eating) (diner diner))
  (log-result 'timeout))

;;; reduces objects mass by set amount then, if mass=0, causes
;;; all activities associated with the object to complete

(defun reduce-mass (ob amt &key cause)
  (setx (mass ob) (max 0 (- (mass ob) amt)))
  (when (= 0 (mass ob))
    (if *verbose-test*
        (format t "Completing activities ~a for ~a~%" 
                (activities ob) ob))
    (mapc #'(lambda (act) (complete-activity act ob))
            (activities ob))))

;;; ---- Multitrials

(defvar *numtrials* 1)
(defvar *results*) ;; used to keep track of trial outcomes

(defun init-results-log ()
  (setf *results* (list 0 0 0 0)))

(defun log-result (res)
  (incf (nth (case res (full 0) (cold 1) (timeout 2) (no-more 3))
             *results*)))

(do-after-each-trial
  (if (not *verbose-test*)
    (princ "*")))  ;; brief indication of new trial

(do-after-all-trials
  (format t "~%Results ~a~%" *results*))

;;; ---- Initialization

(initialize-simulation
  ;; initialize the multitrial run
  (do-before-all-trials
    (format t "Trials are about to begin...~%"))
  (when (= 1 (current-trial))    ;; counts eating completion types
    (init-results-log)
    (set-number-of-trials *numtrials*))
  ;; initialize the specific trial
  (let ((fred (make-instance 'diner :hunger 10 :name 'fred)) 
        (soup (make-instance 'hotfood :temp 70 :mass (+ 6 (random 8))
                             :name 'soup))
        (lunch-duration (+ 20 (random 20))))
    (start-activity fred 'eating :primary-object fred :rate 1
                    :food soup :update-interval 3 :duration lunch-duration)
    (start-activity soup 'cooling :primary-object soup :rate 5
                    :update-interval #'cool-rate)
    (if *verbose-test* 
        (print-start-conditions soup fred lunch-duration))))

(defun print-start-conditions (food diner dur)
  (format t "~%Initial conditions for trial ~a~%" (current-trial))
  (format t "  food quantity: ~a~%" (mass food))
  (format t "  food temp: ~a~%" (temp food))
  (format t "  diner hunger: ~a~%" (hunger diner))
  (format t "  allotted lunch time: ~a ~%" dur))  

(defun cool-rate (cooling)
  (declare (ignore cooling))
  (+ 1 (random 3)))
