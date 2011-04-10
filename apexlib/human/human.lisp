;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Human class
;;; apex/apexlib/human/human.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: human.lisp,v 1.8 2006/01/15 03:42:50 dalal Exp $

(in-package :common-lisp-user)

(require-apex-library "physob")

;;; Human agents couple the generic agent architecture for action
;;; selection with the human-specific resource-architecture.  This
;;; file includes funcitons for creating human-agents.

;;; -------------------------------------------------------------------------
;;; === Human

;;; A standard human uses all available resource components including
;;; two hand components.  As additional resource types are
;;; constructed, these will also be incorporated.  If variants of some
;;; resource types are available, the standard human will incorporate
;;; the one viewed as most complete (though perhaps less efficient
;;; than others)

;;; A standard human is kind of agent.  Every agent has certain
;;; properties; a human agent adds properties particular to the human
;;; resource architecture.

(defclass human (agent physob)
  (;; same as view-location; also eye pos
   (location :accessor location 
             :initarg :location
             :initform '(0 0 0))
   (vision :accessor vision :initarg :vision)
   (voice :accessor voice :initarg :voice)
   (gaze :accessor gaze :initarg :gaze)
   (left-hand :accessor left-hand :initarg :left-hand)
   (right-hand :accessor right-hand :initarg :right-hand)
   (memory :accessor memory :initarg :memory)
   (audition :accessor audition :initarg :audition)
   (external-event :accessor external-event :initarg :external-event)))

;;; ! Hack to work around hierarchy problem.  Without this method, POS
;;; of human yields POS of its superclass Agent, when we want POS of its
;;; superclass Physob.  Since POS is a slot in Physob, we just take the
;;; slot value here.

(defmethod pos ((x human))
  (slot-value x 'pos))

;;; ! get rid of resource-specific slots... do all through func-relations
;;; ! need to work out something better for setting value of procedures slot
;;;   and for initialing biases from procedures.  Probably init-biases and
;;;   procedures should be stored together in a lib struct.  Or maybe biases
;;;   should be derived at initialization time for each agent.
;;; ! scenario might specify non-default initial state for resources
;;; ! hook provided for scenario varying of which is dominant hand

(defmethod assemble ((human-1 human) &key component-of &allow-other-keys)
  (add-apex-resource (make-instance 'vision) human-1)
  (add-apex-resource (make-instance 'gaze) human-1)
  (add-apex-resource (make-instance 'voice) human-1)
  (add-apex-resource (make-instance 'left-hand) human-1 :alias 'non-dominant-hand)
  (add-apex-resource (make-instance 'right-hand) human-1 :alias 'dominant-hand)
  (add-apex-resource (make-instance 'memory) human-1)
  (add-apex-resource (make-instance 'audition) human-1)
  (add-apex-resource (make-instance 'external-event) human-1)
  (start-activity human-1 'seeing :vision-resource (vision human-1)
		  :update-interval 500 :fullcycle 20)
  ;;need activity for every perceptual resource
  (asamain human-1) ;; do initial expansion of tasks
  human-1 
  )

;;; ! This is re-introduced (temporarily) because its refactoring in
;;; 2.4.3 had some problems who's cause was not found.
;
(defmethod add-apex-resource ((r resource) (h human) &key alias)
  (add-resource h r)
  (setf (slot-value h (type-of r)) r)
  (push r (components h))
  (push (cons (type-of r) r) (func-relations h))
  (if alias (push (cons alias r) (func-relations h)))
  (when (typep r 'physob) 
    (setf (locale r) (locale h)) 
    ;;
    ;; ! NOTE: component-of is not passed here since taken care of
    ;; above; resources are a special kind of component
    ;;
    (assemble r))) 


;;; ! This is re-introduced (temporarily, and as a method and with some
;;; refactoring) because its refactoring in 2.4.3 had some problems
;;; who's cause was not found.
;
(defmethod replace-local-names (expr (human human))
  (cond ((null expr) nil)
	((consp expr)
	 (mapcar #'(lambda (e) (replace-local-names e human)) expr))
	((keywordp expr) expr)
	((symbolp expr)
	 (or 
	  (cdr (assoc expr (func-relations human))) ;; substitute in resource
	  (lookup-unique-name expr human) ;; map unique name to visob
	  expr)) ;; no substitution
	(t expr)))
