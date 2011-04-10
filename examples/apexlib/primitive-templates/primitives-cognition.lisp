;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Primitives for cognition
;;; apex/examples/apexlib/primitive-templates/primitives-cognition.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: primitives-cognition.lisp,v 1.4 2006/01/15 03:42:53 dalal Exp $

(apex-info :version "2.4")


;ATTEND-AUDITORY-STIMULUS
(procedure
  (index (attend-auditory-stimulus))
  (profile memory audition-block)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;ATTEND-CURSOR-AT-TARGET
(procedure
  (index (attend-cursor-at-target ?target))
  (profile memory vision-block)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;ATTEND-TARGET
(procedure
   (index (attend-target ?target))
   (profile memory vision-block)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;ATTEND-VISUAL-STIMULUS
(procedure
  (index (attend-visual-stimulus))
  (profile memory vision-block)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;GET-CHUNK
(procedure
   (index (get-chunk ?chunk))
   (profile memory typing-cognition-block)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1)))



;IDENTIFY-AUDITORY-STIMULUS
(procedure
  (index (identify-auditory-stimulus))
  (profile memory audition)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;IDENTIFY-VISUAL-STIMULUS
(procedure
  (index (identify-visual-stimulus))
  (profile memory vision)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?r))
  )

;INITIATE-CLICK
(procedure
   (index (initiate-click ?target))
   (profile memory right-hand-block typing-cognition-block) 
   
;; This requires both the right-hand-block and the
;;     typing-cognition-block to perform correctly but I can't figure
;;     out precisely why BEJ 07nov01

   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INITIATE-CHORD-CLICK
(procedure
   (index (initiate-chord-click ?target))
   (profile memory)
;;     This requires NEITHER the right-hand-block nor
;;     the typing-cognition-block to perform
;;     correctly but I can't figure out precisely why
   ;;     I can't figure out why it is different from initiate-click BEJ 07nov01
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INITIATE-DOUBLE-CLICK
(procedure
   (index (initiate-double-click ?target))
   (profile memory right-hand-block typing-cognition-block)
;;     This was created on 30jun02 by BEJ for the fox task
;;     It requires both the right-hand-block and
;;     the typing-cognition-block to perform
;;     because intiate-click does, but it is subject
   ;;     to the "I don't know why" note on that primitive
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INITIATE-EYE-MOVEMENT
(procedure
   (index (initiate-eye-movement ?target))
   (profile memory)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INITIATE-HOME-TO
(procedure
   (index (initiate-home-to ?device))
   (profile memory right-hand-block)

;;   This must use and hold the right-hand-block to prevent
;; early-interleaving by the click-mouse template later in the task.
;; It's possible that the click-mouse and the
;; perceive-binary-without-eyemovement might be better off combined into
;; a single template???? BEJ 7nov01

   (step s1 (use resource memory for (50 ms)))
   (step s2 (hold-resource right-hand-block :ancestor 2) (waitfor ?s1))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1 ?s2))
)


;INITIATE-HORIZONTAL
(procedure
   (index (initiate-horizontal ?key))
   (profile memory typing-cognition-block)
;; Right now this is just using the typing-cognition-block because the
;; CAD model doesn't type anything after this but some day it might need
   ;; to hold it and the initiate-keystroke release it
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;INITIATE-KEYSTROKE
(procedure
   (index (initiate-keystroke ?key))
   (profile memory)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
)


;INITIATE-MANUAL-RESPONSE
(procedure
  (index (initiate-manual-response))
  (profile memory)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;INITIATE-MOVE-AND-TAP-FINGER
(procedure
   (index (initiate-move-and-tap-finger ?target))
   (profile memory right-hand-block)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INITIATE-MOVE-CURSOR
(procedure
   (index (initiate-move-cursor ?target))
   (profile memory right-hand-block)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INITIATE-PRESS-SHIFT
(procedure
   (index (initiate-press-shift)) ;used for a shift-click of the mouse button
   (profile memory left-hand-block typing-cognition-block)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INIITIATE-RELEASE-SHIFT
(procedure
   (index (initiate-release-shift)) ;used for a shift-click of the mouse button
   (profile memory left-hand-block typing-cognition-block)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;INITIATE-VOCAL-RESPONSE
(procedure
  (index (initiate-vocal-response))
  (profile memory)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;NO-OP
;: This is a dummy action needed when a Decide operator decides not to
;; do anything A GOMS Decide operator is implemented with a PDL step
;; with a select clause.  A PDL step with a select clause MUST select
;; and execute a procedure, so this no-op procedure was created to take
;; no time and do nothing.

(procedure
  (index (no-op))
  (profile memory)
  (step s1 (use resource memory for (0 ms)))
  (step t (terminate) (waitfor ?s1))
  )


;REMEMBER
(procedure
   (index (remember ?something))
   (profile memory)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;SELECT-RESPONSE
(procedure
  (index (select-response))
  (profile memory)
  (step s1 (use resource memory for (50 ms)))
  (step t (terminate) (waitfor ?s1))
  )


;VERIFY ;; The following primitive was made for the fox task to do the
;; verify-edit that the CMN-GOMS model does after executing the
;; task. BEJohn is not sure ;; that a simple 50 msec cognitive operator
;; is appropriate here, but it will ;; remain until we reason more about
;; it or find or generate some data to test ;; against it. (The ATM data
;; that Rick Lewis and Alonso Vera have been working ;; on may help to
;; set a highly-practiced verify time.)


(procedure
  (index (verify ?type ?item))
  (profile memory)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;VERIFY-CURSOR-AT-TARGET
(procedure
  (index (verify-cursor-at-target ?target))
  (profile memory)
  (step s1 (use resource memory for (50 ms)))
  (step r (reset +this-task+) (waitfor (resumed +this-task+)))
  (step t (terminate) (waitfor ?s1))
  )


;VERIFY-SIGNAL
(procedure
   (index (verify-signal ?signal))
   (profile memory)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )


;VERIFY-TARGET-POSITION
(procedure
   (index (verify-target-position ?target))
   (profile memory)
   (step s1 (use resource memory for (50 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate) (waitfor ?s1))
   )
