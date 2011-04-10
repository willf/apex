;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/phone.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: phone.lisp,v 1.2 2006/01/15 03:42:57 dalal Exp $

(in-package :user)

;;;; ------ Phone --------------------------------------------------

;;;; Modified from Hello.lisp

(defclass telephone (physob)
  ((state
    :type symbol   ; possible values: silent, ringing, engaged
    :initform 'silent
    :accessor state)
   (state-start-time  ;used to determine how long it will ring
    :type number
    :initform 0
    :accessor state-start-time)))

;;; Physical objects (physobs) require "assembly".  The assemble method
;;; is a convenient means for combining many objects into one (not
;;; applicable in this case) and starting initial activities.


(defmethod assemble ((tel telephone) &key component-of)
  (start-activity tel 'phone-being :update-interval 10))


;;; ---- Activities 

;;; 1. "Being" a Phone

;;; We are modeling the telephone as a passive object that simply
;;; "waits" until something happens to it, in our case being it starts
;;; ringing (without any modeled cause).  The activity of "being" models
;;; this passive state.

;;; The (simulation) activity of "being a telephone" never ends.  It is
;;; updated at regular intervals, at which time if the phone is silent
;;; it may randomly start ringing.  


(defclass phone-being (activity) ())

(defmethod update-activity ((act phone-being) (tel telephone))
  (progn
    (when (and (eq 'silent (state tel)) (ringing-chance))
      (setx (state tel) 'ringing)
      (inform `(ringing ,tel) 
              :router *walter-router*)
      (setf (state-start-time tel) (current-time)))
    ;; stops ringing after a certain period, depending on when it 
    ;; started ringing.
    (when (eq 'ringing (state tel))
      (cond
       ((< (state-start-time tel) 6000) 
	(when (>= (- (current-time) (state-start-time tel)) 1000) 
	  (setx (state tel) 'silent) 
          (inform `(silent ,tel) 
                  :router *walter-router*)) )
       ((< (state-start-time tel) 9000) 
	(when (>= (- (current-time) (state-start-time tel)) 1500)
          (setx (state tel) 'silent) 
          (inform `(silent ,tel) 
                  :router *walter-router*)))
       (t 
	(when (>= (- (current-time) (state-start-time tel)) 4500)
	  (setx (state tel) 'silent) 
          (inform `(silent ,tel) 
                  :router *walter-router*)))))))

(defun ringing-chance ()                ; Support function for update-activity.
  (or (= (random 10) 1)))

;;; 3. Saying hello

;;; For this we'll used the SPEAKING activity defined in the Human
;;; library.  We'll just specialize its completion method to create an
;;; appropriate event.

(defmethod complete-activity ((act speaking) (v voice))
  (log-event `(said ,(utterance act)) :agent (find-agent 'walter)))