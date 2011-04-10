;;PERCEPTION PRIMITIVES
;;MJB: AS OF 07/29/02


#|
  The following three procedures visually perceive a target on screen.
  Their durations are the same as the John & Gray, 1992, CPM-GOMS tutorial.

  Perceive-target-binary is for perceiving only whether a signal is
  present or not, e.g., in Milliseconds Matter (100 ms for the perception).
  This primitive corresponds to what psychologists might call "detection".

  Perceive-target-complex is for something perceiving
  a six letter word or well learned code (290 ms for the perception).
  This primitive corresponds to what psychologists might call "identification".

  Perceive-cursor-at-target is a special-purpose perceive-target-binary
  that comes up with using the mouse. It is specifically for perceiving
  that the cursor has arrived at the target you want to click on. It is no
  different than perceive-target-binary, but it comes up so often that we
  gave it its own name because BEJohn hypothesized that it would easier to
  read the traces. This hypothesis may be proved incorrect and
  perceive-cursor-at-target may be eliminated after further experience modeling.
|#


;PERCEIVE-AUDITORY-STIMULUS
(procedure :sequential
  (index (perceive-auditory-stimulus))
  (use resouce audition for (250 ms)))


;PERCEIVE-CURSOR-AT-TARGET
(procedure :sequential
  (index (perceive-cursor-at-target ?target))
  (use resource vision for (100 ms)))


;PERCEIVE-TARGET-BINARY
(procedure
   (index (perceive-target-binary ?target))
   (step s1 (use resource vision for (100 ms)))
   (step r (reset +this-task+) (waitfor (interrupted +this-task+)))
   (step t (terminate)
         (waitfor ?s1))
  )


;PERCEIVE-TARGET-COMPLEX
(procedure :sequential
   (index (perceive-target-complex ?target))
   (use resource vision for (290 ms)))


;PERCEIVE-VISUAL-STIMULUS
(procedure :sequential
  (index (perceive-visual-stimulus ?t2a_dur))
  ;; assuming unit is millisecond
  (use resource vision for (?t2a_dur ms)))
