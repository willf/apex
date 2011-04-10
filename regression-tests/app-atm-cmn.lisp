;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/app-atm-cmn.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: app-atm-cmn.lisp,v 1.7 2006/01/15 03:42:58 dalal Exp $
;;; Description: Regression test application

(in-package :user)

;;;(defparameter *app-timeout* (* 10 60)) ;; i.e. no more than 10 minutes to finish
;;;
;;;(apex.utility.unit-test:with-tests (:name "Application: ATM Worlds: CMN")
;;;  (apex.utility.unit-test:test-no-error
;;;   (without-redefinition-warnings
;;;    (load-application-file "Apex:examples;goms;atm;cmn")))
;;;
;;;  (apex.utility.unit-test:test-with-timeout 
;;;   (*app-timeout*) 
;;;   (startapp))
;;;  
;;;  (apex.utility.unit-test:test-no-error
;;;   (verify-hierarchy *application*)))

(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: ATM Worlds: CMN"  "Apex:examples;goms;atm;cmn" :test-agents NIL)