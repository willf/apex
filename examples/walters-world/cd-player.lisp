;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/cd-player.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: cd-player.lisp,v 1.2 2006/01/15 03:42:56 dalal Exp $

;;; --- CD player
;;; after a while, the music stops.  simple.

(in-package :common-lisp-user)

(defclass cd-player (agent physob) ())

;;; --- CD-player
(in-apex-bundle :CD)

(primitive
 (index (be CD))
 (on-start
  (schedule '(200 ms) 
	    (inform `(music-stopped ,+self+) 
                    :router *walter-router*)
            (inform  `(wake-up-dog)
                     :router *walter-router*))))
                    