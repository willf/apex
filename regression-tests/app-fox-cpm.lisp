;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/app-fox-cpm.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: app-fox-cpm.lisp,v 1.9 2006/01/15 03:42:58 dalal Exp $
;;; Description: Regression test application

(in-package :user)

;;;(defparameter *app-timeout* (* 3 60)) ;; i.e. no more than 10 minutes to finish
;;;
;;;(apex.utility.unit-test:with-tests (:name "Application: Fox CPM")
;;;  (apex.utility.unit-test:test-no-error
;;;   (without-redefinition-warnings
;;;    (load-application-file "Apex:examples;goms;fox-cpm;fox-cpm.lisp")))
;;;
;;;  (apex.utility.unit-test:test-with-timeout 
;;;   (*app-timeout*) 
;;;   (startapp))
;;;
;;;  (apex.utility.unit-test:test-no-error
;;;   (verify-hierarchy *application*)))


(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: Fox CPM"  "Apex:examples;goms;fox-cpm;fox-cpm" :test-agents NIL)
