;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/goms/atm/common.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: common.lisp,v 1.3 2006/01/15 03:42:53 dalal Exp $

(in-package :user)

;;; Code shared by the ATM simworld examples.

(require-apex-library "atm")

(defun initialize-atm-common (&optional (diagram-type 'wireframe))
  (let* 
      ((world
        (make-instance 'locale
          :name 'world
          :graphical-object
          (make-instance 'file-svg-object
            :filename (translate-logical-pathname
                       "Apex:examples;goms;atm;atm.svg"))))
       (human (make-instance 'human :name 'agent :locale world
                             :initial-task '(get 80 from atm)
                             :location '(0 0 400)))
       (atm (make-instance 'interface-object :name 'atm :locale world))
       (money-slot (make-instance 'money-slot :name 'money-slot :locale world))
       (screen (make-instance 'screen :name 'screen :locale world))
       (checking-key (make-instance 'button :name 'checking-key :locale world))
       (withdraw-key (make-instance 'button :name 'withdraw-key :locale world))
       (correct-key (make-instance 'button :name 'correct-key :locale world))
       (no-key (make-instance 'button :name 'no-key :locale world))
       (keypad (create-keypad '(43 104) '(13 11) '(4 2) 'down world diagram-type))
       (ok-key (make-instance 'button :name 'ok-key :locale world))
       (card-slot (make-instance 'card-slot :name 'card-slot :locale world))
       (mouse (create-mouse world))
       )
    ;; assemble top level objects
    (mapc #'assemble (list human atm))  
    ;; assemble components of ATM
    (mapc #'(lambda (obj) (assemble obj :component-of atm)) 
          (list screen keypad card-slot money-slot
                withdraw-key checking-key correct-key mouse no-key ok-key))

    (set-visual-vals 
     '(
       (KEY-0 :pos (60 143) :dim (13 11))
       (KEY-1 :pos (43 104) :dim (13 11))
       (KEY-2 :pos (60 104) :dim (13 11))
       (KEY-3 :pos (77 104) :dim (13 11))
       (KEY-4 :pos (43 117) :dim (13 11))
       (KEY-5 :pos (60 117) :dim (13 11))
       (KEY-6 :pos (77 117) :dim (13 11))
       (KEY-7 :pos (43 130) :dim (13 11))
       (KEY-8 :pos (60 130) :dim (13 11))
       (KEY-9 :pos (77 130) :dim (13 11))
       (ATM          :pos (0 0) :dim (182 164))
       (SCREEN       :pos (21 23) :dim (108 68))
       (KEYPAD       :pos (43 104) :dim (47 50))
       (CARD-SLOT    :pos (131 127) :dim (43 2))
       (MONEY-SLOT   :pos (11 9) :dim (98 2))
       (CHECKING-KEY :pos (131 51) :dim (15 11))
       (WITHDRAW-KEY :pos (131 66) :dim (15 11))
       (CORRECT-KEY  :pos (131 66) :dim (15 11))
       (NO-KEY       :pos (131 81) :dim (15 11))
       (OK-KEY       :pos (95 129) :dim (15 11))))

    ;;initialize the graphical objects for the atm based on diagramType
    ;;diagramType is oneof (wireframe, hierarchical, file)
    ;;wireframe shows rectangles for atm, screen, keypad etc
    ;;hierarchical shows colored rectangles, some circles, etc for demo purposes
    ;;file uses svg file containing image of ATM plus hotspots for keys
    (cond ((eq diagram-type 'hierarchical) 
	   (setf (graphical-object atm)
		 (make-instance 'rect 
				:x (get-obj-x atm) :y (get-obj-y atm)
				:width (get-obj-width atm) :height (get-obj-height atm)
				:stroke "black"))
	   (add-label (graphical-object atm) "ATM Demo" 13 7 :font-size 6 :font-style "italic" :fill "white")
	   (setf (graphical-object money-slot) 
		 (make-instance 'rect 
				:x (get-obj-x money-slot)
				:y (get-obj-y money-slot)
				:width (get-obj-width money-slot) 
				:height (get-obj-height money-slot)
				:rx 2 :ry 2
				:fill "green" :fill-opacity 1 :stroke "black"))
	   (setf (graphical-object screen)
		 (make-instance 'rect 
				:x (get-obj-x screen) 
				:y (get-obj-y screen)
				:width (get-obj-width screen)
				:height (get-obj-height screen)
				:fill "grey" :fill-opacity 1 :stroke "black"))
	   (setf (graphical-object correct-key)
		 (make-instance 'circle 
				:cx (+ (get-obj-x correct-key) (/ (get-obj-width correct-key) 2))
				:cy (+ (get-obj-y correct-key) (/ (get-obj-height correct-key) 2)) :r (/ (get-obj-width correct-key) 2)
				:fill "#88FF00" :fill-opacity 1 :stroke "black"))
	   (auto-label (graphical-object correct-key) "Yes" :font-size 4)
	   (setf (graphical-object no-key)
		 (make-instance 'circle 
				:cx (+ (get-obj-x no-key) (/ (get-obj-width no-key) 2))
				:cy (+ (get-obj-y no-key) (/ (get-obj-height no-key) 2)) :r (/ (get-obj-width no-key) 2)
				:fill "red" :fill-opacity 1 :stroke "black"))
	   (auto-label (graphical-object no-key) "No" :font-size 4)
	   (setf (graphical-object ok-key)
		 (make-instance 'rect 
				:x (get-obj-x ok-key) 
				:y (get-obj-y ok-key)
				:width (get-obj-width ok-key)
				:height (get-obj-height ok-key)
				:rx 2 :ry 2
				:fill "yellow" :fill-opacity 1 :stroke "black"))
	   (auto-label (graphical-object ok-key) "Ok" :font-size 4)
	   (setf (graphical-object card-slot)
		 (make-instance 'rect 
				:x (get-obj-x card-slot) 
				:y (get-obj-y card-slot)
				:width (get-obj-width card-slot)
				:height (get-obj-height card-slot)
				:fill "white" :fill-opacity 1 :stroke "black"))
	   )
	  ((eq diagram-type 'wireframe)
	   (create-wireframe-graphical-object atm)
	   (create-wireframe-graphical-object money-slot)
	   (create-wireframe-graphical-object screen)
	   (create-wireframe-graphical-object correct-key)
	   (create-wireframe-graphical-object no-key)
	   (create-wireframe-graphical-object ok-key)
	   (create-wireframe-graphical-object card-slot)
	   )
	  ((eq diagram-type 'file)
	   (setf (graphical-object atm)
		 (make-instance 'file-svg-object :filename (translate-logical-pathname "Apex:examples;goms;atm;atm.svg")))
	   ))))

;;; Procedures that are :special invoke Lisp directly.  The following
;;; procedures return values that are bound to local variables using the
;;; => operator.

(primitive
 (index (mouse-object))
 (return (object *mouse*)))

(primitive
 (index (mouse-time ?object))
 (return (list (atm-fitts-time (pointer *mouse*) ?object) 'ms)))


