;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/simulation/classes.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: classes.lisp,v 1.8 2006/01/15 03:43:02 dalal Exp $


;;; This file contains all class definitions used in this directory.
;;; They have been put into this file (which should be compiled first)
;;; to obviate evaluation order problems that otherwise easily manifest.

;;; ! Could use some better organization and formatting; needs more
;;; documentation.  

(in-package :common-lisp-user)

;;; ----------------------------------------------------------------------------
;;; ---- Simulation class

(defclass sim-agenda (red-black-tree) 
  ()
  (:default-initargs :order-predicate #'< :key 'scheduled-time))

(defclass native-sim-application (application)
  ((time                                ; The current (simulation) time.
    :initform 0
    :accessor get-current-time)
   (agenda                              ; Queue of time-ordered events to trigger
    :type sim-agenda                         ; sim-agenda(Agendum)
    :initform (make-instance 'sim-agenda)
    :accessor agenda)
   (trial :initform 0 :accessor trial)  ; current trial iteration-value
   (numtrials :initform 1               ; how many trials should be performed
              :accessor numtrials) 
   (command :initform nil               ; unprocessed command
            :accessor command)          ; stop, pause, advance-trial, nil
   (event-count :initform 0             ; Number of events since last
					; pause; an event is the
                :accessor event-count)  ; processing of an Activity.
   (pause-cycle :type integer           ; If defined, number of events
					; after which to pause sim. 
					; ! could make initform max int
                :initform nil           ; for type consistency).
                :accessor pause-cycle)
   (pause-time :type integer            ; If defined, absolute time at
					; which to pause sim.
               :initform nil
               :accessor pause-time)
   (next-pause-time                     ; used internally to manage pause time
    :type integer
    :initform nil
    :accessor next-pause-time)
   (pause-interval
    :type integer                       ; If defined, time interval at
                                        ; which to pause sim.
    :initform nil
    :accessor pause-interval)
   (pause-after-each-trial?             ; Flag to pause sim after trial.
    :type boolean
    :initform nil
    :accessor pause-after-each-trial?)
   (pause-after-initialization?         ; Flag to pause after sim initialization.
    :type boolean
    :initform nil
    :accessor pause-after-initialization?)
   (after-each-trial-action             ; Action to take between each trial.
    :type function                      ; Thunk
    :accessor after-each-trial-action)
   (after-all-trials-action             ; Action to take after all trials done.
    :type function                      ; Thunk
    :accessor after-all-trials-action)
   (before-all-trials-action            ; Action to take before trials begin.
    :type function                      ; Thunk
    :accessor before-all-trials-action
    :initform nil)
   ;; ! The following flag might not be needed if the sim engine loop is
   ;; refactored.
   (before-all-trials-done?             ; Internal flag
    :type boolean
    :accessor before-all-trials-done?
    :initform nil)
   (step?                               ; Internal flag to indicate that a single
    :type boolean                       ; step (event) was requested.
    :accessor step?
    :initform nil)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(after-all-trials-action after-each-trial-action
                before-all-trials-action before-all-trials-done?))))


(defclass agendum (appob)            ; A scheduled action 
  ((scheduled-time
    :type integer
    :initarg :time
    :reader scheduled-time)
   (action
    :type function
    :initarg :action
    :reader action)))

(defmethod agendum-ready? ((a agendum) (time integer))  ; Agendum, time -> bool
  (= time (time a)))

(defmethod do-agendum ((a agendum))  ; Agendum -> ()
  (funcall (action a)))

