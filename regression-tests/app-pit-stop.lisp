;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/app-pit-stop.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: app-pit-stop.lisp,v 1.4 2006/01/15 03:42:59 dalal Exp $
;;; Description: Regression test application

(in-package :user)

;;;(defparameter *app-timeout* (* 10 60)) ;; i.e. no more than 10 minutes to finish
;;;
;;;(apex.utility.unit-test:with-tests (:name "Application: Pit-Stop World")
;;;  (apex.utility.unit-test:test-no-error
;;;   (without-redefinition-warnings
;;;    (load-application-file "Apex:examples;pit-stop")))
;;;
;;;  (apex.utility.unit-test:test-with-timeout 
;;;   (*app-timeout*) 
;;;   (startapp))
;;;
;;;  (apex.utility.unit-test:test-no-error
;;;   (verify-hierarchy *application*)))


(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: Pit Stop World"  "Apex:examples;pit-stop" :test-agents NIL)
