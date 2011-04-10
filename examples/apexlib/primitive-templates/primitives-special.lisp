;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/apexlib/primitive-templates/primitives-special.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: primitives-special.lisp,v 1.6 2006/01/15 03:42:53 dalal Exp $

;;SPECIAL PRIMITIVES
;;MJB/BEJ: AS OF 07/31/02

#|
    This file contains "special" procedures (which can break out of PDL
    and into lisp) and the lisp code that implements them.

    This file contains:
       1. The procedures that implement interacting with a world.
       2. A lisp function, depends-on, that implements the conditional
          used for a GOMS Decide operator or selection rules.
       3. Code for inserting a break into pdl to use during debugging
       4. Code necessary to stop the simulation

    The special procedures that are necessary for Fitts's Law calculations
    are found in  primitives-motor.lisp because they are so tied to the
    motor actions it seemed more natural to put them in there than in this file.
|#


;WORLD
;; The following WORLD procedure is used in a step at the template-level when
;; a motor action results in a change to the world.
;; The "world" in this case is represented as a list of attribute-value pairs.
;; The list is called *varlist* and the pairs can be anything the analyst
;; needs in the particular world being modeled.
;; The WORLD procedure can put a new attribute-value pair on the list or
;; change the value of a pair already on the list. This is an extremely
;; simple implementation of an interactive world.
;; This simple implementation allows templates to effect each other.
;; E.g., consider making a selction of a menu command.
;; The click on the menu title makes the menu items appear,
;; so the user can see them and click on one of them. Thus, the mouse-click in
;; the template that clicks on the menu title uses a WORLD step to make the
;; menu items visible.
;;
;; Code written by Mike Matessa, summer 2001

(procedure
   (index (WORLD ?dur ?var ?val))
   (profile external-event)
   ;; assuming unit is millisecond
   (step s1 (use resource external-event for (?dur ms)))
   (step w (set ?var ?val) (waitfor ?s1))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?w ?s1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Code needed to set variables
;; and generate cogevents that report changes to these variables Code
;; written by Mike Mattessa, summer 2001

(defvar *varlist* nil)
(setf *varlist* nil)

;;;(procedure :special
;;;   (index (var-encode ?var ?val))
;;;   (var-encode ?var ?val))


(primitive 
    (index (var-encode ?var ?val))
   (on-start (var-encode ?var ?val)))

(defun var-encode-varname (x)
  (cond
   ((symbolp x) x)
   ((typep x 'id-mixin) (name  x))
   (t x)))

(defun var-encode (var val)
  (let ((var (var-encode-varname var)))
    ;; (format t "Encoding ~a as ~a~%" var val)
    (if (assoc var *varlist*)
      (setf *varlist* (remove (assoc var *varlist*) *varlist* :test 'equal)))
    (push (list var val) *varlist*)
    (cogevent (list 'variable var val) *agent*)))

(procedure
  (index (set ?var ?val))
  (step s1 (var-encode ?var ?val))
  (step t (terminate) (waitfor ?s1)))



;DEPENDS-ON A lisp function that checks selection rules and conditions
;; in decide operators It matches the task conditions with the variables
;; in the *varlist* Thus, decide operators or selection rules can match
;; on values in the "world" or other attributes can also be put on the
;; *varlist*, like whether the hand is on the mouse or on the keyboard
;; (to select between menus or keyboard shortcuts) or whether something
;; is remembered (see example below).  var is the attribute you want to
;; be tested val-method-list is a list of pairs, where the first element
;; in a pair is a possible value of the var to be tested and the second
;; element in the pair is the name of the procedure you want to be
;; executed if the value in the first element is indeed the current
;; value of var.  NOTE: the second element of a pair in val-method-list
;; can be no-op, that is, the result of depends-on can be to do nothing.
;; You can find the procedure for no-op in primitives-cognition.lisp
;; Example of the use of this function in the select clause in the
;; foxCMN model is:
;;
;; (step s1 (?this-step) (select ?this-step (depends-on '*task*
;; '((remembered no-op) (forgotten acquire-unit-task)))))

;; This means that if the task is remembered, then do nothing at this
;; point and proceed to the next step (which is to execute the task),
;; and if the task is forgotten, then use a procedure called
;; acquire-unit-task to find out what the task is before trying to
;; execute it (presumably, acquire-unit-task would involve looking at
;; task instructions, but it is not implemented yet in our examples).


(defun depends-on (var val-meth-list)
   (second (assoc (second (assoc var *varlist*)) val-meth-list)))


;EXTRACT
;; Extracts the value of a variable in the *varlist* and returns it
;; extract is used with a PDL step that needs to assign a value to a variable
;; that will be used multiple times in a procedure.
;; For example:
;;  (step assign1 (extract *first-word-to-be-moved* => ?first-word))

;;;(procedure :special
;;;  (index (extract ?info))
;;;  (second (assoc ?info *varlist*)))

(primitive (index (extract ?info))
  (return (second (assoc ?info *varlist*))))

;BREAK Code to define (break) as legal pdl so you can insert a break
;; into the simulation when debugging
;;;(procedure :special
;;;  (index (break))
;;;(break))

(primitive (index (break))
  (on-start (break)))

