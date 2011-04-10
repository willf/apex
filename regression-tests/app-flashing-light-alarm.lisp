;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/app-flashing-light-alarm.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: app-flashing-light-alarm.lisp,v 1.3 2006/01/15 03:42:58 dalal Exp $
;;; Description: Regression test application

(in-package :user)

;;;(defparameter *app-timeout* (* 10 60)) ;; i.e. no more than 10 minutes to finish
;;;
;;;(apex.utility.unit-test:with-tests (:name "Application: Flashing light alarm")
;;;  (apex.utility.unit-test:test-no-error
;;;   (without-redefinition-warnings
;;;    (load-application-file "Apex:examples;flashing-light-alarm")))
;;;
;;;  (apex.utility.unit-test:test-with-timeout 
;;;   (*app-timeout*) 
;;;   (startapp))
;;;
;;;  (apex.utility.unit-test:test-no-error
;;;   (verify-hierarchy *application*)))


(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: Flashing light alarm"  "Apex:examples;flashing-light-alarm"  :test-agents T)
