;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Default Apex Library
;;; apex/apexlib/default-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: default-apexlib.lisp,v 1.8 2006/03/14 22:36:47 will Exp $

;;; This library is included by default in all applications.  It
;;; contains basic things used by many other libraries.


(in-package :common-lisp-user)

;;; -------------------------------------------------------------------------
;;; Application control
;;; -------------------------------------------------------------------------

(primitive
 (index (end-trial))
 (on-start (end-trial)))

(primitive
 (index (end-application))
 (on-start (end-application *application*)))

;;; -------------------------------------------------------------------------
;;; "Generic Activity"
;;; -------------------------------------------------------------------------

(primitive
 (index (use resource ?resource for ?duration))
 (profile ?resource)
 (duration ?duration))

(primitive
 (index (?activity using ?resource for ?duration))
 (profile ?resource)
 (duration ?duration))

;;; -------------------------------------------------------------------------
;;; External (e.g. world) events
;;; -------------------------------------------------------------------------

;;; These definitions create a resource class and associated activity classes
;;; that can be used to time intervals from PDL. At present this is the
;;; only way to easily wait for a specified amount of time. It violates a
;;; clean separation between the human agent and external devices. But,
;;; it provides a needed functionality. This will be replaced in future
;;; releases.

(defclass external-event (resource) nil)

;;; Misc

;; Find an object by its name

(primitive (find object named ?name)
  (duration (0 ms))
  (return (instance-named ?name)))

(primitive (find agent ?name)
  (duration (0 ms))
  (return (find-agent ?name)))

;; Publish a message to the default router

(primitive (publish ?message)
  (duration (0 ms))
  (on-completion (inform ?message)))

;; Publish a message to a specific router

(primitive (publish ?message to ?router)
  (duration (0 ms))
  (on-completion
   (let ((r (if (typep ?router 'ps-router) 
	      ?router
	      (instance-named ?router))))
     (if r 
       (inform ?message :router r)
       (warn "No router with that name.")))))

;; Subscribe to a router

(primitive (subscribe to ?router)
  (duration (0 ms))
  (on-completion 
   (let ((r (if (typep ?router 'ps-router) 
	      ?router
	      (instance-named ?router))))
     (if r 
       (subscribe +self+ ?router)
       (warn "No router with that name.")))))
  
(primitive (unsubscribe from ?router)
  (duration (0 ms))
  (on-completion 
   (let ((r (if (typep ?router 'ps-router) 
	      ?router
	      (instance-named ?router))))
     (if r 
       (unsubscribe +self+ ?router)
       (warn "No router with that name.")))))


;; Print -- replaces current print, this can print multiple forms
;;; (step s1 (print this is a test))

(primitive (print . ?forms)
  (duration (0 ms))
  (on-completion (format t "~{~a ~}~%" ?forms)))


;;;;;;;;;;;;;;;  Trace support ;;;;;;;;;;;;;;;;;;;;;;;

;;; A few show-level definitions that might prove useful

;;; need to expand this somehow to include other hand activities
(define-show-level actions
    (or (event-type started)	(event-type completed)
	(event-type stopped)	(event-type clobbered)
	(event-type fixated)	(event-type winnowed)
	(event-type encoded)	(event-type retrieved)
	(event-type grasped)	(event-type released)))

(define-show-level default
    (or (event-type task-started)))

(define-show-level asa-low
    (or (event-type task-started) (event-type interrupted)
	(event-type terminated) (event-type resumed)))

(define-show-level asa-medium
    (or (event-type monitor-satisfied)
	(event-type enabled)
	(event-type procedure-selected)
	(event-type conflict-resolved)
	(event-type task-started)
	(event-type executed)
	(event-type interrupted)
	(event-type resumed)
	(event-type terminated)
	(event-type assumption-violated)))
	
(define-show-level asa-high
    (or (event-type monitor-satisfied) (event-type enabled)
	(event-type procedure-selected) (event-type conflict-resolved)
	(event-type task-started) (event-type executed)
	(event-type interrupted) (event-type resumed)
	(event-type terminated) (event-type assumption-violated)
	(event-type task-created) (event-type monitor-created)
	(event-type refused-enablement) (event-type conflict-detected)
	(event-type resource-allocated)
	(event-type resource-deallocated) (event-type reset)
	(event-type reinstantiated)))

(define-show-level cogevents
    (or (event-type enabled) (event-type refused-enablement)
        (event-type conflict-resolved) (event-type resource-allocated)
	(event-type interrupted) (event-type resource-deallocated)
	(event-type task-started) (event-type executed)
	(event-type resumed) (event-type terminated)
	(event-type reset) (event-type reinstantiated)
	(event-type completed) (event-type stopped) (event-type started)
	(event-type clobbered) (event-type pos)
	(event-type color) (event-type orientation)
	(event-type shape) (event-type contrast)
	(event-type blink) (event-type elements)
	(event-type contains) (event-type contained-by)
	(event-type fixated) (event-type winnowed)
	(event-type encoded) (event-type retrieved)
	(event-type new) (event-type revalued)
	(event-type refined) (event-type grasped)
	(event-type released) (event-type moved)
	(event-type struck) (event-type turned-dial)
	(event-type typed) (event-type pressed)))


