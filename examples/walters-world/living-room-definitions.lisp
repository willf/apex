;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/living-room-definitions.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: living-room-definitions.lisp,v 1.5 2006/01/15 03:42:57 dalal Exp $

(in-package :user)

;;------------------------------------------------------
;;
;;
;;  LIVING ROOM OBJECTS
;;
;;
;;------------------------------------------------------  
;;; This is for definitions of misc objects in the living-room
;;; of Walter's World.

;;; --- lamp -----------------------------------------------------

(defclass lamp (physob) 
  ((power 
    :accessor power 
    :initarg 
    :power 
    :initform 'on)
   (shape 
    :accessor shape 
    :initarg shape 
    :initform '(bright-lamp))))

;;; --- Window ----------------------------------------------------

;;; weather outside should have an effect on something, but I don't 
;;; know what.

(defclass window (physob)
  ((weather-outside 
    :accessor weather-outside 
    :initform 'sunny)
   (opened 
    :accessor opened 
    :initarg 
    :opened 
    :initform 'open)
   (shape 
    :initform '(window)))) 

(defmethod assemble ((w window) &key component-of)
  (let ((index (random 4)))
    (cond
     ((= index 0) (setf (weather-outside w) 'sunny))
     ((= index 1) (setf (weather-outside w) 'cloudy))
     ((= index 2) (setf (weather-outside w) 'rain))
     (t           (setf (weather-outside w) 'windy)))))

;;; Object Hierarchy hacks (explained in Apex human library)

(defmethod pos ((x dog))
  (slot-value x 'pos))

(defmethod pos ((x cd-player))
  (slot-value x 'pos))

(defmethod pos ((x computer))
  (slot-value x 'pos))



