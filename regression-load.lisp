;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-load.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: regression-load.lisp,v 1.5 2006/01/16 16:59:12 will Exp $

;;; Loads Apex in a fashion suited for regression testing.

(defparameter *regression-testing* t)   ; unbound otherwise

(load "load")
(terpri)
;;; Don't update load history when loading tests.
(setq *update-load-history* nil)
