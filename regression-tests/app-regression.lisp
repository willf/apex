;;-*- Mode: Lisp; Package: :user -*-
;;; ------------------------------------------------------------------------
;;; File:           $Id: app-regression.lisp,v 1.3 2005/02/01 16:58:41 will Exp $
;;; Created:        2004-09
;;; Author:         Will Fitzgerald
;;;
;;; Description: Regression test application
;;; 
;;;

(in-package :user)

;;;(defparameter *app-timeout* (* 10 60)) ;; i.e. no more than 10 minutes to finish
;;;
;;;(apex.utility.unit-test:with-tests (:name "Application: Regression World")
;;;  (apex.utility.unit-test:test-no-error
;;;   (without-redefinition-warnings
;;;    (load-application-file "Apex:examples;regression")))
;;;
;;;  (apex.utility.unit-test:test-with-timeout 
;;;   (*app-timeout*) 
;;;   (startapp))
;;;
;;;  (apex.utility.unit-test:test-no-error
;;;   (verify-hierarchy *application*)))


(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: Regression World"  "Apex:examples;regression" :test-agents T)

