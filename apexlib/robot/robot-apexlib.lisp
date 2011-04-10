;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Robot library
;;; apex/apexlib/robot/robot-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: robot-apexlib.lisp,v 1.3 2006/01/15 03:42:51 dalal Exp $

(in-package :user)

;;; Dependencies

(require-apex-library "physob")

(require-apex-file "robot")
(require-apex-file "vision" )
(require-apex-file "gaze" )
(require-apex-file "audition" )
(require-apex-file "radio" )
(require-apex-file "gripper" )
(require-apex-file "memory" )
(require-apex-file "navigation")

;;; The following taken out of obsoleted locale-apexlib.  Not sure if
;;; this is the best place.

(defun set-visual-vals (lis)
  ;;lis= ( (<ob-name> :pos (x y) :dim (x y)) ...)
  (dolist (i lis)
    (set-visual-val (visual-find (first i))
                    (third i) (fifth i))))

(defun visual-find (x)
  (dolist (i (all-other-obs *agent*))
    (if (or (equal x (name i)) (equal x (id i))) (return i))))


(defun all-other-obs (agent)
  (if agent
      (remove agent
              (remove (right-hand agent)
                      (remove (left-hand agent) (vfield (locale agent)))))))
  
;;; Visual Field (Preview) related

(defun preview (agent)
  (dolist (i (mapcar #'visual-val (all-other-obs agent)))
    (when (and (second i) (third i))
      (format t "~a ~a ~a ~a ~a~&" 
              (first i) 
              (first (second i))
              (second (second i))
              (first (third i)) 
              (second (third i))))))
