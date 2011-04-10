;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Image building code
;;; apex/build-image.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: build-image.lisp,v 1.5 2006/03/13 17:18:24 will Exp $

;;; Allegro must be in the 'apex' directory before loading this file.  


;;; Set up the logical host
(setf (logical-pathname-translations "apex")
  `(("**;*.*.*" ,(make-pathname 
		  :directory (append (pathname-directory *load-truename*)
				    (list :wild-inferiors))
		  :name :wild
		  :type :wild))))


(eval-when (load eval compile)
  ;; Defined in system/loading/load-support.lisp
  (setq *restart-app-function* 'main)
  (generate-application
   "apex" (format nil "~a~a/" (current-directory) "dist")
   '(:loop :process :sock :eli :debug :tpl-debug :list2
     :toplevel :trace :inspect :frame :acldns :aserve 
     "load.lisp")
   ;;
   ;; This doesn't work -- Franz is working on it.
   ;; :ignore-command-line-arguments nil
   ;;
   ;; No license for this (yet)
   ;; :runtime :dynamic
   :include-compiler t
   :discard-compiler t)
  (exit))
