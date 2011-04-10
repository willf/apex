;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/sherpa/eventtypes.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: eventtypes.lisp,v 1.4 2006/01/15 03:43:02 dalal Exp $


(in-package :common-lisp-user)

(defconstant asa-eventtypes-default
    '(task-created enabled refused-enablement procedure-selected
      conflict-detected conflict-resolved resource-allocated interrupted
      resource-deallocated started executed resumed terminated reset 
      reinstantiated assumption-violated procedure-retrieved task-started))

(defconstant ra-activity-eventtypes-default
    '(started completed stopped clobbered))

(defconstant vision-eventtypes-default
    '(nothing-new pos color orientation shape contrast blink elements
      contains contained-by))

(defconstant gaze-eventtypes-default
    '(fixated winnowed held-gaze))
    
(defconstant memory-eventtypes-default
    '(encoded retrieved new revalued refreshed refined))

(defconstant hand-eventtypes-default
    '(grasped released moved turned-dial pressed-button))

(defconstant ra-eventtypes-default
    (list ra-activity-eventtypes-default
	  vision-eventtypes-default
	  gaze-eventtypes-default
	  memory-eventtypes-default
	  hand-eventtypes-default))

(defparameter *apex-event-types*
    (flatten (list ra-eventtypes-default asa-eventtypes-default)))

(defun application-specific-event-types ()  ; -> list(symbol)
  ;;
  ;; returns a list of recorded event types that are not predefined by
  ;; Apex
  ;;
  (filter #'(lambda (type)
              (not (member type *apex-event-types*)))
          *recorded-event-types*))
