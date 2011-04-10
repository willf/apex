;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/load.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: load.lisp,v 1.12 2006/03/13 17:18:24 will Exp $

;;; Load this file to load Apex.


#+allegro (setq cl:*print-case* :downcase)
#+allegro (tpl:setq-default *print-case* :downcase)

(defparameter *apex-version-name* "3.0 Beta")

;;; When testing or developing Apex add :test-apex to *features* to
;;; include test code into the build/load.

(push :test-apex *features*)

#+(:and :test-apex :allegro)
;;; (require :tester) -- we're using our own utility now.
;;;#+(:and :test-apex :allegro)
;;;    (use-package :util.test)


;;; ! This is probably not the best place for these flags.  Can't put in
;;; user prefs file because that's loaded after patches and extensions.
;;; Need to resolve this dilemna.

(defvar *load-extensions* nil)
(defvar *load-patches* nil)
(defvar *load-preferences* nil)
  
;;; Set up the logical host
(setf (logical-pathname-translations "apex")
  `(("**;*.*.*" ,(make-pathname 
		  :directory (append (pathname-directory *load-truename*)
				    (list :wild-inferiors))
		  :name :wild
		  :type :wild))))

;;; Load Extra Debugging Support 


(load "apex:system;utility;debug")

;;; Load 


(load "apex:system;loading;xsdf.lisp")
(load "apex:system;loading;intrinsic.xsdf")
(load "apex:system;loading;sim-engine.xsdf")
(load "apex:system;loading;sherpa.xsdf")
(xsdf:load-system *apex.intrinsic.files*)
(xsdf:load-system *apex.sim-engine.files*)
(xsdf:load-system *apex.sherpa.files*)

(startup-terminal)
(terpri)

;; test line
