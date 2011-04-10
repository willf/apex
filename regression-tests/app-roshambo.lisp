;;-*- Mode: Lisp; Package: :user -*-
;;; ------------------------------------------------------------------------
;;; File:           $Id: app-roshambo.lisp,v 1.6 2005/05/05 01:28:04 dalal Exp $
;;; Created:        2004-08-25T14:21:22.827Z 
;;; Author:         Will Fitzgerald
;;;
;;; Description: Regression test for roshambo application
;;; 
;;;

(in-package :user)

(load "Apex:system;utility;unit-test-application")

(unit-test-application  "Application: Roshambo"  "Apex:examples;roshambo;roshambo8" :test-agents T)