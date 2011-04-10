;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/simulation/test/test5.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: test5.lisp,v 1.3 2006/01/15 03:43:02 dalal Exp $


;;; Test of event tracing.
;;; Scenario: Water is heated, causing it to boil, which causes it to evaporate.

(defconstant *boiling-point* 212)

;;; Water

(defclass water (appob)
  ((temp
    :type integer
    :initform 200
    :accessor temp)
   (level
    :type integer
    :initform 10
    :accessor level)))

(defmethod print-object :around ((w water) stream)
  (call-next-method)
  (format stream " temp ~a, level ~a" (temp w) (level w)))

(defmethod heat ((w water) &key cause)
  (declare (ignore cause))
  ;; Raises temperature of water one degree.
  (setx (temp w) (1+ (temp w))))

(defmethod evaporate ((w water) &key cause)
  (declare (ignore cause))
  ;; Evaporates one unit of water
  (setx (level w) (1- (level w))))

(defmethod boil ((w water) &key cause)
  (declare (ignore cause))
  ;; Causes evaporation (temperature remains constant)
  (signal-event (evaporate w)))

;;; Heating Activity
  
(defclass heating (activity) ())

(defmethod update-activity ((h heating) (w water))
  (format t "[~a] Heating ~a~%" (current-time) w)  ; abstract this
  (signal-event (heat w))
  (when (= (temp w) *boiling-point*)
    (signal-event (complete-activity h w))
    (signal-event (start-activity w 'boiling :update-interval 1 
                                  ))))

;;; Boiling Activity

(defclass boiling (activity) ())

(defmethod update-activity ((b boiling) (w water))
  (format t "[~a] Boiling ~a~%" (current-time) w)  ; abstract this
  (signal-event (boil w))
  (when (= 0 (level w))
    (signal-event (complete-activity b w))
    (log-event `(evaporated ,w))))

(initialize-simulation
 (let ((water (make-instance 'water)))  ; add random temperatures and levels
   (start-activity water 'heating :update-interval 1)))
