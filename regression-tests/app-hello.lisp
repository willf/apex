;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/app-hello.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: app-hello.lisp,v 1.6 2006/01/15 03:42:58 dalal Exp $
;;; Description: Regression test application

(in-package :user)

;;;(defparameter *app-timeout* (* 10 60)) ;; i.e. no more than 10 minutes to finish
;;;
;;;(apex.utility.unit-test:with-tests (:name "Application: Hello World")
;;;  (apex.utility.unit-test:test-no-error
;;;   (without-redefinition-warnings
;;;    (load-application-file "Apex:examples;hello")))
;;;
;;;  (apex.utility.unit-test:test-with-timeout 
;;;   (*app-timeout*) 
;;;   (startapp))
;;;
;;;  (apex.utility.unit-test:test-no-error
;;;   (verify-hierarchy *application*)))

(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: Hello, World"  "Apex:examples;hello" :test-agents NIL)
