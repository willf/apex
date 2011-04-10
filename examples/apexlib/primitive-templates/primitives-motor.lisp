;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/apexlib/primitive-templates/primitives-motor.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: primitives-motor.lisp,v 1.9 2006/01/15 03:42:53 dalal Exp $

;;MOTOR PRIMITIVES
;;MJB: AS OF 07/29/02, BEJohn 01aug02

#|

    This file contains the primitives that do motor actions.

    Special procedures that are necessary for Fitts's Law calculations
    are also in here because they are so tied to the motor actions that
    it seemed more natural to put them in here than in the 
primitives-special file
|#

(require-apex-library "visob")

;EYE-MOVEMENT This uses the gaze resource which is some combination of
;; CPM-GOMS' visual perception and eye-movement resources.

(procedure
   (index (eye-movement ?target))
   (profile gaze)
   (step s1 (use resource gaze for (30 ms)))
   (step s2 (target-value ?target => ?val) (waitfor ?s1))
   (step s3 (set ?target ?val) (waitfor ?s2))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s3))
   )

;;;(procedure :special
;;;   (index (target-value ?target))
;;;   (second (assoc ?target *varlist*)))

(defun target-value-name (target)
  (cond
   ((symbolp target) target)
   ((typep target 'id-mixin) (name target))
   (t target)))

(defun target-value-f (target)
  (let ((key (target-value-name target)))
    ;;(format t "TARGET: ~A; KEY: ~a; *VARLIST*: ~A~%"
    ;;    target
    ;;    (second (assoc key *varlist*))
    ;;    *varlist*)
    (second (assoc key *varlist*))))

(primitive (index (target-value ?target))
  (return (target-value-f ?target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FITTS LAW
; T = a log2(D/S + b) where D is the distance from where the cursor is
;    to the centroid of the target and S is the minimum of the height
;    and width of the target as of 01aug02, only rectangular targets are
;    supported in ApexBuilder and only rectangular targets appear in any
;    models so this definition is meaningful

(primitive
 (index (mouse-time ?object))
 (return (list (fitts-time (pointer *mouse*) ?object) 'ms)))

;;;(procedure :special
;;;   (index (mouse-object))
;;;   (object *mouse*))

(primitive (index (mouse-object))
  (return (object *mouse*)))

(setf *fitts-a* 100)
(setf *fitts-b* 0.5)

(defmethod fitts-time (pointer obj)
   (if (> 0 (fitts-time-calc pointer obj)) 0 (fitts-time-calc pointer obj)))

(defmethod fitts-time-calc ((pointer visob) (obj visob))
   (let* (;;(obj (lookup-unique-name obj *agent*))
          (d (distance (pos pointer) (pos obj)))
          (s (min (first (dimensions obj))
                  (second (dimensions obj)))))
     (if obj
       (floor (* *fitts-a* (log (+ (/ d s) *fitts-b*) 2)))
       (format t "Error: no object found~&"))))


;;;; The function DISTANCE, used in the Fitts's Law calculation is defined in
;;   the library as (including the warning):
;;
;;
;;(defun distance (p1 p2)
;;  (sqrt (+ (expt (- (first p1) (first p2)) 2)
;;	   (expt (- (second p1) (second p2)) 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;HORIZONTAL-MOVEMENT
;; 		Eventually the time will need to be calculated by Fitts's law,
;; 		and the model should know which hand resource to allocate.
;; 		This is currently just for the left hand.

(procedure
   (index (horizontal-movement ?key))
   (profile left-hand left-hand-block)
   (step s1 (use resource left-hand for (100 ms))) ;100 was used in Baskin & John
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;MOUSE-CLICK
(procedure :sequential
  (index (mouse-click ?target))
  (mouse-object => ?object)
  (mouse-click ?object with right-hand taking (200 ms)))


;MOUSE-CHORD-DOWN
(procedure
   (index (mouse-chord-down ?target))
   (profile right-hand)
   (step s1 (use resource right-hand for (100 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;MOUSE-CHORD-UP
(procedure
   (index (mouse-chord-up ?target))
   (profile right-hand)
   (step s1 (use resource right-hand for (100 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;MOUSE-DOWN
(procedure :sequential
  (index (mouse-down ?target))
  (mouse-object => ?object)
  (mouse-down on ?object with right-hand taking (100 ms)))


;;; MOUSE-DOWN-LONG
;;; ! note that ?target is unused
(procedure :sequential
  (index (mouse-down-long ?target ?duration))
  (mouse-object => ?object)
  (mouse-down on ?object with right-hand taking ?duration))


;;;MOUSE-DOWN-WITHOUT-WORLD
;;; ! note that ?target is unused
(procedure
   (index (mouse-down-without-world ?target))
   (profile right-hand)
   (step s1 (use resource right-hand for (100 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;MOUSE-UP
(procedure :sequential
  (index (mouse-up ?target))
  (mouse-object => ?object)
  (mouse-up on ?object with right-hand taking (100 ms)))


;MOUSE-UP-WITHOUT-WORLD
(procedure
   (index (mouse-up-without-world ?target))
   (profile right-hand)
   (step s1 (use resource right-hand for (100 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1)))


;MOVE-AND-TAP-FINGER BEJ: Move-and-tap-finger is exactly the same as the
;; move-cursor that calls Fitts's Law, except that it doesn't need to be
;; followed by the click since the original Fitts's Law is only tapping,
;; so it is palready included).

(procedure :sequential
   (index (move-and-tap-finger ?target))
   (mouse-time ?target => ?time)
   (move-mouse to ?target with right-hand taking ?time))


;MOVE-CURSOR
(procedure :sequential
  (index (move-cursor ?target))
  (mouse-time ?target => ?time)
  (move-mouse to ?target with right-hand taking ?time))


;MOVE-CURSOR-WITHOUT-MOUSETIME
(procedure
   (index (move-cursor-without-mousetime ?target))
   (profile right-hand both-hand-block right-hand-block)
   (step s1 (use resource right-hand for (1 sec)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;PRESS-SHIFT
(procedure :sequential
  (index (press-shift))
  (use resource left-hand for (100 ms)))

;;PUSH-KEY 
(procedure :sequential
  (index (push-key))
  (use resource right-hand for (100 ms)))

;;;RELEASE-KEY
(procedure :sequential
  (index (release-key))
  (use resource right-hand for (50 ms)))


;RELEASE-SHIFT
(procedure :sequential
  (index (release-shift))
  (use resource left-hand for (100 ms)))


;RIGHT-HAND-HOME-TO
(procedure
   (index (right-hand-home-to ?device))
   (profile right-hand right-hand-block)
   (step s1 (use resource right-hand for (350 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1)))



;TYPE-KEY-DOWN
(procedure
   (index (type-key-down ?key))
   (profile left-hand)
   ;; 100 was used in Baskin & John
   (step s1 (use resource left-hand for (100 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1)))


;TYPE-KEY-RIGHT
(procedure
   (index (type-key-right ?key))
   (profile right-hand right-hand-block type-string-block)
   (step s1 (use resource right-hand for (170 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;TYPE-KEY-LEFT
(procedure
   (index (type-key-left ?key))
   (profile left-hand left-hand-block type-string-block)
   (step s1 (use resource left-hand for (170 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;TYPE-KEY-UP
(procedure
   (index (type-key-up ?key))
   (profile left-hand)
   (step s1 (use resource left-hand for (100 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)

