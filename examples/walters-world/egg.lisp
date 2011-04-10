;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/egg.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: egg.lisp,v 1.2 2006/01/15 03:42:56 dalal Exp $

(in-package :common-lisp-user)

;;;  --- Eggs  -------------------------------------------------

;;; Eggs are tricky to represent.  For example, when one cracks and 
;;; separates an egg, causing the yolk/white (eggmass) to come out, one 
;;; is left with two objects (shell and eggmass).  One could represent 
;;; this as the destruction of the old (egg) object and the creation of 
;;; two new ones (shell and eggmass). But then, how is this mundane act 
;;; different from a "magical" transformation -- e.g. an egg into a pair
;;; of birds?  Another idea is to have one of the new objects be the 
;;; continuation of the old one, so that only one new object is created. 
;;; Which should be the continuation?  The eggmass seems sensible since 
;;; it's the important part of the egg.  But this produces the awkward
;;; situation in which a previously held object drops out of the hand 
;;; while a newly created one (the shell) "appears" in the hand from 
;;; nowhere.  The alternative used here is to treat the shell as the 
;;; original object.  

;;; ---- egg  -------------------------------------------------

;;; The egg is represented as a combination of two objects -- shell and 
;;; eggmass. Initially, only the shell can be seen or manipulated.  By 
;;; creating the eggmass along with the shell, it is possible, e.g., to 
;;; model what happens to the eggmass if it is heated while still in the 
;;; shell.  An egg is used here only as a holder for its components.  
;;; Agent actions such as grasping are performed on the shell (if the 
;;; shell is moved, the insides move with it). Cooking effects take 
;;; place on the eggmass.

(defclass egg (physob)
  ((eggshell 
    :accessor eggshell 
    :initarg 
    :eggshell)
   (eggmass 
    :accessor eggmass 
    :initarg 
    :eggmass)))

(defmethod assemble ((egg-1 egg) &key component-of)
  (let* ((eggmass (make-instance 'eggmass
                    :temp 0 
                    :mass 1 
                    :cookstate 0 
                    :cookrate (normRand 30 10) 
                    :yolk 'intact 
                    :shape '(eggmass) 
                    :texture 'liquid 
                    :component-of (component-set egg-1)))
	 (shell (make-instance 'eggshell 
                  :soundness 'intact 
                  :color 'white
                  :shape '(egg eggshell ovoid) 
                  :texture 'smooth 
                  :component-of (component-set egg-1))))
    (setx (eggshell egg-1) shell)
    (setx (eggmass egg-1) eggmass)
    (assert-physob-relation `(in ,eggmass ,shell))
    (assemble eggmass :component-of (component-set egg-1))
    (assemble shell :component-of (component-set egg-1))
    egg-1))

(defun normRand (mean sd)
  ;;return a normally-distributed variable using Box-Muller Transformation
  (let* ((x1 (random 1.0))
         (x2 (random 1.0))
         (z1 (* (sqrt (* -2 (log x1))) (cos (* 2 pi x2)))))
    (+ (* z1 sd) mean)))

;;; --- eggshell -------------------------------------------------

(defclass eggshell (physob)
  ((soundness 
    :accessor soundness 
    :initarg 
    :soundness)
   (temp 
    :initform 0))
  );; possible values: intact, cracked, open

;;; models the effect of striking an eggshell against another object.  
;;; This assumes that the object <against> will be massive and hard
;;; enough to crack the shell.

(defmethod struck ((shell eggshell) against)
  (declare (ignore against)) 
  (setx (texture shell) 'cracked)
  (setx (soundness shell) 'cracked))

;;; This function defines what happens to a cracked egg/shell when it is 
;;; opened -- i.e. split apart by hand.  This function assumes that when 
;;; the eggmass drops out of the shell, it doesn't change x,y position, 
;;; but merely becomes decoupled from the shell -- i.e. it will no longer
;;; move with the shell.

(defmethod pulled-apart ((shell eggshell))
  (let* ((contents (find-all-matches `(in ?obj ,shell)))
	 (eggmass (if contents (first contents))))
    (when eggmass
      ;; eggmass becomes visible and no longer inside shell
      (make-visible eggmass)     
      (retract-physob-relation `(in ,eggmass ,shell)) 
      ;; This is a temporary change to simplify this part of the 
      ;; physob database logic.
      (dolist (obj (find-all-matches `(pan ?object)))
        (assert-physob-relation `(in ,obj ,eggmass)))
      ;; change shell soundness state and appearance
      (setx (texture shell) 'open)
      (setx (soundness shell) 'open))))

;;; --- eggmass  -------------------------------------------------

;;; the "egg mass" is the combined yolk and white stuff (albumen)

(defclass eggmass (food)
  ((yolk :accessor yolk :initarg :yolk)
   (mass :initform 1))
  );; values: intact, broken, partly-mixed, mixed

;;; Causes eggmass to begin/continue cooking.  In some cases, this 
;;; results in a change to the visual texture of the eggmass.

(defmethod cookstate-changed ((em eggmass))
  (let ((start-texture (texture em))
	(end-texture (compute-eggmass-texture em)))
    ;;(when (not (equal start-texture end-texture)))
      (setx (texture em) end-texture)))

(defun compute-eggmass-texture (eggmass)
  (let ((ec (cookstate eggmass)))
    (cond ((< ec 2.0) 'liquid)
	  ((< ec 4.0) 'runny)
	  ((< ec 6.0) 'spongy)
	  ((< ec 7.0) 'crisp)
	  ((< ec 8.0) 'very-crisp)
	  (t 'ashen))))
