;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/front-porch-pdl.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: front-porch-pdl.lisp,v 1.6 2006/01/15 03:42:56 dalal Exp $

;;;Procedures for the front porch, which is, at the moment, pretty barren.
(in-package :user)

(procedure
 (index (front-porch-tasks))
 (step stemp (get-some-fresh-air))
 (step st (terminate) 
       (waitfor ?stemp)))

(procedure
 (index (get-some-fresh-air))
 (profile presence attention)
 (step move-to (move-to front-porch))
 (step on-resume (move-to front-porch) 
       (waitfor (resumed +this-task+)))
 (step s1 (breathe-some-air) 
       (waitfor ?move-to))
 (step st (terminate) 
       (waitfor ?s1)))

(primitive
 (index (breathe-some-air))
 (duration (200 ms)))
