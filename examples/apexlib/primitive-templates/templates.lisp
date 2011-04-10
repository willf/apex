;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/apexlib/primitive-templates/templates.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: templates.lisp,v 1.6 2006/01/15 03:42:53 dalal Exp $

;; TEMPLATES
;; BEJ 02aug02


#|
    This file contains the templates
    The parameters of the templates have been labeled
|#



;SLOW-MOVE-CLICK
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (slow-move-click
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (slow-move-click-R-hand-on-mouse
    	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)



;SLOW-MOVE-CLICK-R-HAND-ON-MOUSE
;;;; Identical to the older versions of slow-move-click
(procedure
   (index (slow-move-click-R-hand-on-mouse
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
   (step c1 (initiate-move-cursor ?target))
   (step hvr1 (hold-resource right-hand-block :ancestor 1) (waitfor ?c1))
   (step m1 (move-cursor ?target) (waitfor ?c1))
   (step w2 (WORLD 0 new-cursor-location ?target)  (waitfor ?m1))
   (step c2 (attend-target ?target))
   (step hvr2 (hold-resource vision-block :ancestor 1) (waitfor ?c2))
   (step c3 (initiate-eye-movement ?target) (waitfor ?c2))
   (step m2 (eye-movement ?target) (waitfor ?c3))
   (step p1 (perceive-target-complex ?target)
                      (waitfor ?m2 (variable ?target visible)))
                                   ;the target becomes visible either when it has
                                   ;always been on the display and the eye moves to it
                                   ;or when the eye gets to the location before it
                                   ;appears and then it appears on the display,
                                   ; often because of some motor action in another template
   (step c4 (verify-target-position ?target) (waitfor ?c3 ?p1))
   (step c5 (attend-cursor-at-target ?target)  (waitfor ?c4))
   (step p2 (perceive-cursor-at-target ?target)  (waitfor ?p1 ?c5 ?w2))
   (step rvr2 (release-resource vision-block :ancestor 1) (waitfor ?p2))
   (step c6 (verify-cursor-at-target ?target)  (waitfor ?c5 ?p2))
   (step c7 (initiate-click ?target) (waitfor ?c6 ?m1))
   (step m3 (mouse-down ?target) (waitfor ?m1 ?c7))
   (step w3 (WORLD 0 ?effected-object ?effect) (waitfor ?m3))
   (step m4 (mouse-up ?target) (waitfor ?m3))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m4))
   (step t (terminate) (waitfor ?m4 ?rvr1 ?rvr2))
)



;SLOW-MOVE-DOUBLE-CLICK
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (slow-move-double-click
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (slow-move-double-click-R-hand-on-mouse
    	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)



;SLOW-MOVE-DOUBLE-CLICK-R-HAND-ON-MOUSE
(procedure
   (index (slow-move-double-click-r-hand-on-mouse
   	    :target ?target
   	    :effected-object ?effected-object
   	    :effect-on-object ?effect))
   (step c1 (initiate-move-cursor ?target))
   (step hvr1 (hold-resource right-hand-block :ancestor 1) (waitfor ?c1))
   (step m1 (move-cursor ?target) (waitfor ?c1))
   (step c2 (attend-target ?target))
   (step hvr2 (hold-resource vision-block :ancestor 1) (waitfor ?c2))
   (step c3 (initiate-eye-movement ?target) (waitfor ?c2))
   (step m2 (eye-movement ?target) (waitfor ?c3))
   (step p1 (perceive-target-complex ?target) (waitfor ?m2))
   (step w1 (WORLD 0 ?target fixated-upon) (waitfor ?p1))
   (step c4 (verify-target-position ?target) (waitfor ?c3 ?p1))
   (step c5 (attend-cursor-at-target ?target)  (waitfor ?c4))
   (step w2 (WORLD 0 new-cursor-location ?target)  (waitfor ?m1))
   (step p2 (perceive-cursor-at-target ?target)  (waitfor ?p1 ?c5 ?w2))
   (step rvr2 (release-resource vision-block :ancestor 1) (waitfor ?p2))
   (step c6 (verify-cursor-at-target ?target)  (waitfor ?c5 ?p2))
   (step c7 (initiate-double-click ?target) (waitfor ?c6 ?m1))
   (step m3 (mouse-down ?target) (waitfor ?m1 ?c7))
   (step m4 (mouse-up ?target) (waitfor ?m3))
   (step m5 (mouse-down ?target) (waitfor ?m4))
   (step w3 (WORLD 0 ?effected-object ?effect) (waitfor ?m5))
   (step m6 (mouse-up ?target) (waitfor ?m5))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m6))
   (step t (terminate) (waitfor ?m6 ?rvr1 ?rvr2))
)




;SLOW-MOVE-SHIFT-CLICK
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (slow-move-shift-click
   	      :target ?target
   	      :trigger-object ?trigger-object
   	      :trigger-event ?trigger
   	      :effected-object ?effected-object
   	      :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (slow-move-shift-click-R-hand-on-mouse
   	       :target ?target
   	       :trigger-object ?trigger-object
   	       :trigger-event ?trigger
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)


;SLOW-MOVE-SHIFT-CLICK-R-HAND-ON-MOUSE
(procedure
   (index (slow-move-shift-click-r-hand-on-mouse
   	    :target ?target
   	    :trigger-object ?trigger-object
   	    :trigger-event ?trigger
   	    :effected-object ?effected-object
   	    :effect-on-object ?effect))
   (step c1 (initiate-move-cursor ?target))
   (step hvr1 (hold-resource right-hand-block :ancestor 1) (waitfor ?c1))
   (step m1 (move-cursor ?target) (waitfor ?c1))
   (step c2 (attend-target ?target))
   (step hvr2 (hold-resource vision-block :ancestor 1) (waitfor ?c2))
   (step c3 (initiate-eye-movement ?target) (waitfor ?c2))
   (step m2 (eye-movement ?target) (waitfor ?c3))
   (step p1 (perceive-target-complex ?target) (waitfor ?m2 (variable 
?target visible)))
   (step w1 (WORLD 0 ?target fixated-upon) (waitfor ?p1))
   (step c4 (verify-target-position ?target) (waitfor ?c3 ?p1))
   (step c5 (attend-cursor-at-target ?target)  (waitfor ?c4))
   (step w2 (WORLD 0 new-cursor-location ?target)  (waitfor ?m1))
   (step p2 (perceive-cursor-at-target ?target)  (waitfor ?p1 ?c5 ?w2))
   (step rvr2 (release-resource vision-block :ancestor 1) (waitfor ?p2))
   (step c6 (verify-cursor-at-target ?target)  (waitfor ?c5 ?p2))
   (step c7 (initiate-press-shift) (waitfor (variable ?trigger-object 
?trigger)))
   (step hvr3 (hold-resource left-hand-block :ancestor 1) (waitfor ?c7))
   (step m3 (press-shift) (waitfor ?c7))
   (step c8 (initiate-click ?target) (waitfor ?c6 ?m1))
   (step m4 (mouse-down ?target) (waitfor ?m1 ?c8))
   (step m5 (mouse-up ?target) (waitfor ?m4))
   (step w3 (WORLD 0 ?effected-object ?effect) (waitfor ?m5))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m5))
   (step c9 (initiate-release-shift) (waitfor ?m4))
   (step m6 (release-shift) (waitfor ?c9 ?m5))
   (step rvr3 (release-resource left-hand-block :ancestor 1) (waitfor ?m6))
   (step t (terminate) (waitfor ?m6 ?rvr1 ?rvr2 ?rvr3))
)




;FAST-MOVE-CLICK
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (fast-move-click
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (fast-move-click-R-hand-on-mouse
    	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)



;FAST-MOVE-CLICK-R-HAND-ON-MOUSE
(procedure
   (index (fast-move-click-R-hand-on-mouse
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
   (step c1 (initiate-move-cursor ?target))
   (step hvr1 (hold-resource right-hand-block :ancestor 1) (waitfor ?c1))
   (step m1 (move-cursor ?target) (waitfor ?c1))
   (step c2 (attend-target ?target))
   (step hvr2 (hold-resource vision-block :ancestor 1) (waitfor ?c2))
   (step c3 (initiate-eye-movement ?target) (waitfor ?c2))
   (step m2 (eye-movement ?target) (waitfor ?c3))
   (step p1 (perceive-target-complex ?target) (waitfor ?m2 (variable 
?target visible)))
   (step rvr2 (release-resource vision-block :ancestor 1) (waitfor ?p1))
   (step w1 (WORLD 0 ?target fixated-upon) (waitfor ?p1))
   (step c4 (verify-target-position ?target) (waitfor ?c3 ?p1))
   (step c5 (initiate-click ?target) (waitfor ?c4 ?m1))
   (step m3 (mouse-down ?target) (waitfor ?m1 ?c5))
   (step w2 (WORLD 0 ?effected-object ?effect) (waitfor ?m3))
   (step m4 (mouse-up ?target) (waitfor ?m3))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m4))
   (step t (terminate) (waitfor ?m4 ?rvr1 ?rvr2))
)




;FAST MOVE CHORD-CLICK
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (fast-move-chord-click
   	       :target ?target
   	       :trigger-object ?trigger-object
   	       :trigger-event ?trigger
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (fast-move-chord-click-R-hand-on-mouse
   	       :target ?target
   	       :trigger-object ?trigger-object
   	       :trigger-event ?trigger
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)



;; FAST MOVE CHORD-CLICK-R-HAND-ON-MOUSE
(procedure
   (index (fast-move-chord-click-R-hand-on-mouse
    	       :target ?target
  	       :trigger-object ?trigger-object
   	       :trigger-event ?trigger
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
   (step c1 (initiate-move-cursor ?target))
   (step hvr1 (hold-resource right-hand-block :ancestor 1) (waitfor ?c1))
   (step m1 (move-cursor-without-mousetime ?target)
                      (waitfor ?c1))
   (step c2 (attend-target ?target))
   (step hvr2 (hold-resource vision-block :ancestor 1) (waitfor ?c2))
   (step c3 (initiate-eye-movement ?target)
                      (waitfor ?c2))
   (step m2 (eye-movement ?target)
                      (waitfor ?c3))
   (step p1 (perceive-target-complex ?target)
                      (waitfor ?m2 (variable ?target visible)))
   (step rvr2 (release-resource vision-block :ancestor 1) (waitfor ?p1))
   (step w1 (WORLD 0 ?target fixated-upon) (waitfor ?p1))
   (step c4 (verify-target-position ?target)
                      (waitfor ?c3 ?p1))
   (step c5 (initiate-chord-click ?target)
                      (waitfor ?c4 ?m1))
   (step m3 (mouse-chord-down ?target)
                      (waitfor ?m1 ?c5))
   (step w2 (WORLD 0 ?effected-object ?effect)
                      (waitfor ?m3))
   (step m4 (mouse-chord-up ?target)
                      (waitfor ?m3))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m4))
   (step t (terminate)
                      (waitfor ?m4 ?rvr1 ?rvr2))
)





#|
;; HOME-TO ?DEVICE
(procedure
   (index (home-to
   			:device ?device))
   (step c1 (initiate-home-to ?device))
   (step hvr1 (hold-resource right-hand-block :ancestor 1) (waitfor ?c1))
   (step m1 (right-hand-home-to ?device) (waitfor ?c1))
   (step w (WORLD 0 *R-hand-on* ?device) (waitfor ?m1))
   (step t (terminate) (waitfor ?m1 ?w ?hvr1))
)
|#


;HOME-TO-MOUSE
(procedure
   (index (home-to-mouse))
   (step home-c1 (initiate-home-to mouse))
   (step hvr1 (hold-resource right-hand-block :ancestor 2) (waitfor ?home-c1))
   (step home-m1 (right-hand-home-to mouse) (waitfor ?home-c1))
   (step home-w (WORLD 0 *R-hand-on* mouse) (waitfor ?home-m1))
   (step t (terminate) (waitfor ?home-w)))


;HOME-TO-KEYBOARD
(procedure
   (index (home-to-keyboard))
   (step home-c1 (initiate-home-to keyboard))
   (step hvr1 (hold-resource right-hand-block :ancestor 2) (waitfor ?home-c1))
   (step home-m1 (right-hand-home-to keyboard) (waitfor ?home-c1))
   (step home-w (WORLD 0 *R-hand-on* keyboard) (waitfor ?home-m1))
   (step t (terminate) (waitfor ?home-w)))



;; PERCEIVE-VISUAL-WITHOUT-EYEMOVEMENT-BINARY
(procedure
   (index (perceive-visual-without-eyemovement-binary
   			:signal ?signal
   			:location ?location))
   (step c1 (attend-target ?location) (waitfor (variable ?location 
fixated-upon)))
   (step hvr1 (hold-resource vision-block :ancestor 1) (waitfor ?c1))
   (step p1 (perceive-target-binary ?location) (waitfor ?c1 (variable 
?signal ?location)))
   (step c2 (verify-signal ?signal) (waitfor ?p1))
   (step t (terminate) (waitfor ?c2 ?p1 ?hvr1))
)

#| CLICK-MOUSE BEJ 07nov01 this uses mouse-down-without-world, but it
;; then immediately sets a variable in the world
;;so it is in effect interacting with the world at this level, not the primitive level
(procedure
   (index (click-mouse
     		:target ?target
   			:effected-object ?effected-object
   			:effect-on-object ?effect))
   (step c1 (initiate-click ?target))
   (step m1 (mouse-down-without-world ?target) (waitfor ?c1))
   (step w1 (WORLD 0 ?effected-object ?effect) (waitfor ?m1))
   (step m2 (mouse-up-without-world ?target) (waitfor ?m1))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m2))
   (step t (terminate) (waitfor ?c1 ?m1 ?m2 ?w1 ?rvr1))
)
|#


;CLICK-MOUSE
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (click-mouse
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (click-mouse-R-hand-on-mouse
    	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)



;; CLICK-MOUSE-R-HAND-ON-MOUSE BEJ 07nov01 this uses
;; mouse-down-without-world, but it then immediately sets a variable in
;; the world so it is in effect interacting with the world at this
;; level, not the primitive level
(procedure
   (index (click-mouse-R-hand-on-mouse
     		:target ?target
   			:effected-object ?effected-object
   			:effect-on-object ?effect))
   (step c1 (initiate-click ?target))
   (step m1 (mouse-down-without-world ?target) (waitfor ?c1))
   (step w1 (WORLD 0 ?effected-object ?effect) (waitfor ?m1))
   (step m2 (mouse-up-without-world ?target) (waitfor ?m1))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m2))
   (step t (terminate) (waitfor ?c1 ?m1 ?m2 ?w1 ?rvr1))
)




;MOVE-CURSOR
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (move-cursor
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (move-cursor-R-hand-on-mouse
    	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)


;; MOVE CURSOR-R-HAND-ON-MOUSE
(procedure
   (index (move-cursor-R-hand-on-mouse
   		:target ?target
   		:effected-object ?effected-object
   		:effect-on-object ?effect))
   (step hVr1 (hold-resource right-hand-block :ancestor 1))
   (step c1 (initiate-move-cursor ?target) (waitfor ?hVr1))
   (step m1 (move-cursor ?target)
                      (waitfor ?c1))
   (step rr1 (release-resource right-hand-block :ancestor 1)
         (waitfor ?m1))
   (step w1 (WORLD 0 ?effected-object ?effect) (waitfor ?m1))
   (step t1 (terminate)
                      (waitfor ?w1 ?rr1))
)




;; SINGLE KEYSTROKE-WITH-ANTICIPATION
(procedure
   (index (single-keystroke-with-anticipation
   		:key ?key
   		:precondition-object ?precondition-object
   		:precondition-state ?precondition-state))
   (step c1 (initiate-horizontal ?key))
   (step m1 (horizontal-movement ?key) (waitfor ?c1))
   (step c2 (initiate-keystroke ?key) (waitfor ?m1
                                               (variable 
?precondition-object ?precondition-state)))
   (step m2 (type-key-down ?key) (waitfor ?c2))
   (step m3 (type-key-up ?key) (waitfor ?m2))
   (step t (terminate) (waitfor ?m3))
)



;; AUDITORY-TONE-JUDGEMENT-AND-VOCAL-RESPONSE
(procedure
  (index (auditory-tone-judgement-and-vocal-response))
  (step p1 (perceive-auditory-stimulus))
  (step c1 (attend-auditory-stimulus) (waitfor ?p1))
  (step hvr1 (hold-resource audition-block :ancestor 1) (waitfor ?c1))
  (step c2 (identify-auditory-stimulus) (waitfor ?c1))
  (step c3 (select-response) (waitfor ?c2))
  (step c4 (initiate-vocal-response) (waitfor ?c3))
  (step hvr2 (hold-resource speech-block :ancestor 1) (waitfor ?c4))
  (step m1 (vocal-response) (waitfor ?c4))
  (step rvr1 (release-resource audition-block :ancestor 1) (waitfor ?m1))
  (step rvr2 (release-resource speech-block :ancestor 1) (waitfor ?m1))
  (step t (terminate) (waitfor ?m1 ?rvr1 ?rvr2)))



;; VISUAL-LETTER-IDENTIFICATION-AND-MANUAL-RESPONSE
;; This one allows you to specify the duration
;; of the perceptual stage -- 2A

(procedure
  (index (visual-letter-identification-and-manual-response ?t2a_dur))
  (step p1 (perceive-visual-stimulus ?t2a_dur))
  (step c1 (attend-visual-stimulus) (waitfor ?p1))
  (step hvr1 (hold-resource vision-block :ancestor 1) (waitfor ?c1))
  (step c2 (identify-visual-stimulus) (waitfor ?c1))
  (step c3 (select-response) (waitfor ?c2))
  (step c4 (initiate-manual-response) (waitfor ?c3))
  (step hvr2 (hold-resource right-hand-block :ancestor 1) (waitfor ?c4))
  (step m1 (manual-response) (waitfor ?c4))
  (step rvr1 (release-resource vision-block :ancestor 1) (waitfor ?m1))
  (step rvr2 (release-resource right-hand-block :ancestor 1) (waitfor ?m1))
  (step t (terminate) (waitfor ?m1 ?rvr1 ?rvr2)))


;MANUAL-RESPONSE
(procedure
  (index (manual-response))
  (step s1 (push-key))
  (step s2 (release-key) (waitfor ?s1))
  (step t (terminate) (waitfor ?s1 ?s2))
  )




;SLOW-MOVE-CLICK-AND-DRAG
;;;;;;;;;;;;;;;;;;;;
;; The procedure below that uses the mouse first decides whether it has to move
;; the R-hand to the mouse or if it is already there
(procedure
   (index (slow-move-click-and-drag
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
  (step s1 (?this-step)
  	      (select ?this-step
  		          (depends-on '*R-hand-on*
  		                      '((mouse no-op)
  		       	                (keyboard home-to-mouse))))
  	      (rank 1))
  (step s2 (slow-move-click-and-drag-R-hand-on-mouse
    	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect)
	      (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)



;SLOW-MOVE-CLICK-AND-DRAG-R-HAND-ON-MOUSE
;;;; Identical to the older versions of slow-move-click
(procedure
   (index (slow-move-click-and-drag-R-hand-on-mouse
   	       :target ?target
   	       :effected-object ?effected-object
   	       :effect-on-object ?effect))
   (step c1 (initiate-click ?target))
   (step hvr1 (hold-resource right-hand-block :ancestor 1) (waitfor ?c1))
   (step m1 (mouse-down ?target) (waitfor ?c1))


   (step c2 (attend-target ?target))

   (step c3 (initiate-move-cursor ?target) (waitfor ?c1))
   (step m2 (move-cursor ?target) (waitfor ?c3 ?m1))
   (step w1 (WORLD 0 new-cursor-location ?target)  (waitfor ?m1))


   (step hvr2 (hold-resource vision-block :ancestor 1) (waitfor ?c2))
   (step c4 (initiate-eye-movement ?target) (waitfor ?c2))
   (step m3 (eye-movement ?target) (waitfor ?c4))
   (step p1 (perceive-target-complex ?target)
                      (waitfor ?m2 (variable ?target visible)))
                                   ;the target becomes visible either when it has
                                   ;always been on the display and the eye moves to it
                                   ;or when the eye gets to the location before it
                                   ;appears and then it appears on the display,
                                   ; often because of some motor action in another template
   (step c5 (verify-target-position ?target) (waitfor ?c4 ?p1))
   (step c6 (attend-cursor-at-target ?target)  (waitfor ?c5))
   (step p2 (perceive-cursor-at-target ?target)  (waitfor ?p1 ?c6 ?w1))
   (step rvr2 (release-resource vision-block :ancestor 1) (waitfor ?p2))
   (step c7 (verify-cursor-at-target ?target)  (waitfor ?c6 ?p2))

   (step m4 (mouse-up ?target) (waitfor ?m3 ?c7))
   (step w2 (WORLD 0 ?effected-object ?effect) (waitfor ?m4))
   (step rvr1 (release-resource right-hand-block :ancestor 1) (waitfor ?m4))
   (step t (terminate) (waitfor ?w2 ?rvr1 ?rvr2))
)






;; STUDENT TEMPLATES


;;PRESS-BUTTON
(procedure
  (index (press-button
  				:target ?target))
  (step hvr1 (hold-resource right-hand-block :ancestor 1))
  (step c1 (initiate-move-and-tap-finger ?target)
		    (waitfor ?hvr1))
  (step hvr2 (hold-resource vision-block :ancestor 1))
  (step c2 (attend-target ?target)
		    (waitfor ?hvr2))
  (step c3 (initiate-eye-movement ?target)
                     (waitfor ?c2))
  (step m2 (eye-movement ?target)
                     (waitfor ?c3))
  (step p1 (perceive-target-binary ?target)
                     (waitfor ?m2))
  (step rvr2 (release-resource vision-block :ancestor 1)
        		    (waitfor ?p1))
  (step c4 (verify-target-position ?target)
                     (waitfor ?c3 ?p1))
  (step m1 (move-and-tap-finger ?target)
                     (waitfor ?c1 ?c4))
  (step rvr1 (release-resource right-hand-block :ancestor 1)
        		    (waitfor ?c4 ?m1))
  (step w1 (WORLD 0 new-finger-location ?target)
                     (waitfor ?m1))
  (step t (terminate)
		    (waitfor ?rvr1 ?rvr2 ?w1)))



;;MOVE LONG CLICK (PRESS AND HOLD A BUTTON)
(procedure
  (index (move-long-click
		 	:target ?target
   			:duration ?duration))
  (step c1 (initiate-move-cursor ?target))
  (step hvr1 (hold-resource right-hand-block :ancestor 1)
                     (waitfor ?c1))
  (step m1 (move-cursor ?target)
                     (waitfor ?c1))
  (step c2 (attend-target ?target))
  (step hvr2 (hold-resource vision-block :ancestor 1)
                     (waitfor ?c2))
  (step c3 (initiate-eye-movement ?target)
                     (waitfor ?c2))
  (step m2 (eye-movement ?target)
                     (waitfor ?c3))
  (step p1 (perceive-target-complex ?target)
                     (waitfor ?m2))
  (step rvr2 (release-resource vision-block :ancestor 1)
                     (waitfor ?p1))
  (step c4 (verify-target-position ?target)
                     (waitfor ?c3 ?p1))
  (step w1 (WORLD 0 new-cursor-location ?target)
                     (waitfor ?m1))
  (step c5 (initiate-click ?target)
                     (waitfor ?c4 ?m1))
  (step m3 (mouse-down-long ?target ?duration)
                     (waitfor ?m1 ?c5))
  (step m4 (mouse-up ?target)
                     (waitfor ?m3))
  (step rvr1 (release-resource right-hand-block :ancestor 1)
                     (waitfor ?m4))
  (step t (terminate)
                     (waitfor ?rvr1 ?rvr2))
)





