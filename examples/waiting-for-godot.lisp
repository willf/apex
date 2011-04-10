;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/waiting-for-godot.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: waiting-for-godot.lisp,v 1.6 2006/01/15 03:42:52 dalal Exp $

(in-package :cl-user)

(defapplication "Waiting for Godot"
    :init (initialize-sim))

(defun initialize-sim ()
  (let* ((locale (make-instance 'locale
		   :name "Apex Theatre"))
	 (visual-field 
	  (make-instance 'ps-router 
	    :name "Visual Field"))
	 (auditory-field
	  (make-instance 'ps-router
	    :name "Auditory Field"))
	 (estragon
	  (make-instance 
	      'agent 
	    :name "Estragon"
	    :initial-task '(act)
	    :locale locale
	    :use-bundles '(:actor :estragon)
	    :routers (list visual-field auditory-field)))
	 (vladimir
	  (make-instance 
	      'agent 
	    :name "Vladimir"
	    :initial-task '(act)
	    :locale locale
	    :use-bundles '(:actor :vladimir)
	    :routers (list visual-field auditory-field)))
	 (mc
	  (make-instance
	      'agent
	    :name "MC"
	    :initial-task '(direct-ceremonies)
	    :use-bundles '(:actor :mc)
	    :locale locale
	    :routers (list visual-field auditory-field)))
	 (audience
	  (make-instance
	      'agent
	    :name "Audience"
	    :initial-task '(watch-play)
	    :use-bundles '(:audience)
	    :locale locale
	    :routers (list visual-field auditory-field)))
	 (light-crew
	  (make-instance
	      'agent
	    :name "Light Crew"
	    :initial-task '(do-crew)
	    :use-bundles '(:crew)
	    :locale locale
	    :routers (list visual-field auditory-field))))
    ;; (show said)
    ;;(show walked-to)
    ;;(show spotlight-off)
    ;;(show spotlight-on)
    ;; (unshow)
    (list estragon vladimir light-crew)))

(in-apex-bundle :actor)

(primitive
 (index (say ?utterance))
 (profile voice)
 (duration `(,(floor (* 187 (length ?utterance))) ms))
 (on-completion
  ;; (format t "~a: ~a~%" (name +self+) ?utterance)
  (inform `(said (agent ,(name +self+)) (utterance ,?utterance)) 
	  :router (router-named "Auditory Field")
	  :author +self+ )))

(primitive 
    (index (walk-to ?mark))
  (profile legs)
  (duration p21s)
  (on-completion
   (inform `(walked-to (agent ,(name +self+)) (mark ,?mark))
	  :router (router-named "Visual Field")
	  :author +self+)))


(primitive 
    (index (turn-towards ?mark))
  (profile head)
  (duration p1.20s)
  (on-completion
   (inform `(turned-towards (agent ,(name +self+)) (mark ,?mark))
	  :router (router-named "Visual Field")
	  :author +self+)))


(procedure :sequential
    (index (speak ?line with cue ?cue from ?from))
  (step (say ?line)
	(waitfor (said (agent ?agent) (utterance ?cue)))))

(procedure :sequential
    (index (speak ?line))
    (step (say ?line)))

(in-apex-bundle :estragon)

(procedure 
    :sequential (act)
    (step (walk-to stage-center)
	  (waitfor (curtain raised)))
    (step (speak "Nothing to be done."))
    (step (speak "Am I?" with cue "So there you are again." from "Vladimir"))
    (step (speak "Me too." with cue "I thought you were gone forever." from "Vladimir"))
    (step (speak "Not now, not now." with cue "Get up till I embrace you." from "Vladimir"))
    (step (speak "In a ditch." with cue "May one inquire where His Highness spent the night?" from "Vladimir"))
    (step (speak "Over there." with cue "A ditch! Where?" from "Vladimir"))
    (step (speak "Beat me?" with cue "And they didn't beat you?" from "Vladimir"))
    (step (speak "Certainly they beat me."))
    (step (speak "The same?" with cue "The same lot as usual?" from "Vladimir"))
    (step (speak "I don't know."))
    (step (speak "And what of it?" with cue "You'd be nothing more than a little heap of bones at the present minute, no doubt about it." from "Vladimir"))
    (step (speak "Ah stop blathering and help me off with this bloody thing." with cue "We should have thought of it a million years ago, in the nineties." from "Vladimir"))    
    (step (walk-to exit)))

(in-apex-bundle :Vladimir)

(procedure 
    :sequential (act)
    (step (walk-to stage-left))
    (step (speak "I'm beginning to come round to that opinion." with cue "Nothing to be done." from "Estragon")
	  (waitfor (curtain raised)))
    (step (walk-to stage-center))
    (step (speak "All my life I've tried to put it from me, saying Vladimir, be reasonable, you haven't yet tried everything."))
    (step (speak "And I resumed the struggle."))
    (step (turn-towards "Estragon"))
    (step (speak "So there you are again."))
    (step (speak "I'm glad to see you back." with cue "Am I?" from "Estragon"))
    (step (speak "I thought you were gone forever."))
    (step (speak "Together again at last!" with cue "Me too." from "Estragon"))
    (step (speak "We'll have to celebrate this."))
    (step (speak "But how?"))
    (step (speak "Get up till I embrace you."))
    (step (speak "May one inquire where His Highness spent the night?" with cue "Not now, not now." from "Estragon"))
    (step (speak "A ditch! Where?" with cue "In a ditch." from "Estragon"))
    (step (speak "And they didn't beat you?" with cue "Over there." from "Estragon"))
    (step (speak "The same lot as usual?" with cue "Certainly they beat me." from "Estragon"))
    (step (speak "When I think of it . . . all these years . . . but for me . . . where would you be . . ." with cue "I don't know." from "Estragon"))
    (step (speak "You'd be nothing more than a little heap of bones at the present minute, no doubt about it."))
    (step (speak "It's too much for one man." with cue "And what of it?" from "Estragon"))
    (step (speak "On the other hand what's the good of losing heart now, that's what I say."))
    (step (speak "We should have thought of it a million years ago, in the nineties."))
    (step (walk-to exit)))

(in-apex-bundle :mc)

(procedure 
    :sequential (direct-ceremonies)
    (step (walk-to stage-front))
    (step (speak "Welcome to Apex Theatre's Production of 'Waiting for Godot'"))
    (step (speak "with text from samuel-beckett.net"))
    (step (speak "Enjoy the show"))
    (step (walk-to exit))
    (step (walk-to stage-front) 
	  (waitfor (:and
		    (spotlight-off 'stage-center)
		    (walked-to (agent "Vladimir") (mark exit))
		    (walked-to (agent "Estragon") (mark exit))
		    )))
    (step (speak "Let's hear it for our actors and crew!"))
    )

(in-apex-bundle :crew)

(primitive
    (index (spotlight-on ?mark))
  (duration p2s)
  (on-completion
   (inform `(spotlight-on ?mark)
	   :router (router-named "Visual Field")
	  :author +self+)))


(primitive
    (index (spotlight-off ?mark))
  (duration p2s)
  (on-completion
   (inform `(spotlight-off ?mark)
	   :router (router-named "Visual Field")
	  :author +self+)))

(primitive 
    (index (raise curtain))
  (duration p30s)
  (on-completion
   (inform `(curtain raised)
	   :router (router-named "Visual Field")
	  :author +self+)))

(primitive 
    (index (lower curtain))
  (duration p30s)
  (on-completion
   (inform `(curtain lowered)
	   :router (router-named "Visual Field")
	  :author +self+)))

(procedure :sequential
  (do-crew)
  (step (spotlight-on stage-front)
	(waitfor (walked-to (agent "MC") (mark stage-front))))
  (step (spotlight-off stage-front)
	(waitfor (walked-to (agent "MC") (mark exit))))
  (step (raise curtain))
  (step (spotlight-on stage-center)
  	(waitfor (walked-to (agent ?agent) (mark stage-center))))
  (step (spotlight-off stage-center)
	(waitfor 
	 (:and
	  (walked-to (agent "Vladimir") (mark exit))
	  (walked-to (agent "Estragon") (mark exit))
	  )))
  (step (lower curtain)))
	  
(in-apex-bundle :audience)

(primitive
 (index (applaud))
 (profile hands)
 (duration p20s)
 (on-completion
  (inform `(applause)
	  :router (router-named "Auditory Field")
	  :author +self+)))

(procedure (index (watch-play))
  (step s1 (applaud)
	(waitfor (said (agent ?agent) (utterance ?line)))
	)
  (step s2 (applaud)
	(waitfor (said (agent "MC")
		       (utterance "Let's hear it for our actors and crew!"))))
  (step (terminate) (waitfor ?s1 ?s2)))




