;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/goms/atm/cpm-hier.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: cpm-hier.lisp,v 1.4 2006/01/15 03:42:53 dalal Exp $

(in-package :user)

;;we load cpm to avoid duplicating code here
(load-application-file "Apex:examples;goms;atm;cpm")

(defapplication "ATM-CPM World HierDiagram"
;;    :files ("common.lisp")
;;    :libraries ("atm")
    :init (initialize-atm-common 'hierarchical))








