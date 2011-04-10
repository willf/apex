;;-*- Mode: Lisp; Package: :user -*-
;;; ------------------------------------------------------------------------
;;; File:           $Id: app-waiting-for-godot.lisp,v 1.2 2005/02/01 16:58:41 will Exp $
;;; Created:        2004-08-25T14:28:55.891Z
;;; Author:         Will Fitzgerald
;;;
;;; Description: Regression test application
;;; 
;;;

(in-package :user)

;;;(defparameter *app-timeout* (* 10 60)) ;; i.e. no more than 10 minutes to finish
;;;
;;;(apex.utility.unit-test:with-tests (:name "Application: Waiting for Godot")
;;;  (apex.utility.unit-test:test-no-error
;;;   (without-redefinition-warnings
;;;    (load-application-file "Apex:examples;waiting-for-godot")))
;;;
;;;  (apex.utility.unit-test:test-with-timeout 
;;;   (*app-timeout*) 
;;;   (startapp))
;;;
;;;  (apex.utility.unit-test:test-no-error
;;;   (verify-hierarchy *application*)))


(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: Waiting for Godot"  "Apex:examples;waiting-for-godot" :test-agents T)