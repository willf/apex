;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/plant.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: plant.lisp,v 1.2 2006/01/15 03:42:57 dalal Exp $

(in-package :user)

;;;  -------  Houseplant ------------------------------------

(defclass plant (agent physob)
  ((flower 
    :type flower 
    :accessor flower 
    :initarg 
    :flower)
   (thirst 
    :accessor thirst 
    :initarg 
    :thirst 
    :initform 0)
   (shape 
    :accessor shape 
    :initform '(plant) 
    :initarg 
    :shape)))

(defclass flower (physob)
  ((state 
    :accessor state 
    :initarg :state 
    :initform '(fresh))))

(in-apex-bundle :plant)

(primitive
 (index (be plant))
 (on-start
  (schedule '(100 ms) 
	    (inform (list 'ready-to-pick (flower +self+) +self+) 
                    :router *walter-router* )))
 (update (200 ms)
	 (incf (thirst +self+) (random 3))
	 (cond 
	  ((< (thirst +self+) 20) 
	   nil)
	  ((< (thirst +self+) 25) 
	   (inform `(getting-dry ,+self+) 
                   :router *walter-router*))
	  ((< (thirst +self+) 40) 
	   (inform `(i-should-water ,+self+) 
                   :router *walter-router*)  
	   (unless (null (flower +self+))
             (setx (state (flower +self+)) '(droopy))))
	  ((< (thirst +self+) 50) 
	   (inform (list 'going-to-wilt +self+) 
                   :router *walter-router*))
	  (t 
	   (unless (null (flower +self+))
	     (setx (state (flower +self+)) '(wilted)))))))

;;; Object Hierarchy hacks (explained in Apex human library)

(defmethod pos ((x plant))
  (slot-value x 'pos))