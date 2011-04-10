;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/tv.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: tv.lisp,v 1.2 2006/01/15 03:42:57 dalal Exp $

(in-package :user)

;;;  -------  Television

(defclass television (agent physob)
  ((channel 
    :accessor channel 
    :initarg 
    :channel 
    :initform 7)
   (volume 
    :accessor volume 
    :initarg 
    :volume 
    :initform 10)))

;;; ---- television
(in-apex-bundle :tv)

(primitive
 (index (be tv))
 (on-start (setf (volume +self+) 10))
 (update (200 ms)
	 (if (= (random 30) 0)  
	     (inform `(interesting ,+self+ ,(+ 10 (random 500)))
                     :router *walter-router*))))

;;; Object Hierarchy hacks (explained in Apex human library)

(defmethod pos ((x television))
  (slot-value x 'pos))
