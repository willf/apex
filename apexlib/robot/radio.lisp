;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Radio class
;;; apex/apexlib/robot/radio.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: radio.lisp,v 1.4 2006/01/15 03:42:51 dalal Exp $

(in-package :common-lisp-user)

;;; Radio class
;;;

;;(print "Start")

(defclass radios (physob resource)
  ((frequency :initarg :frequency :accessor frequency :initform 100.0)))

(defparameter *comm-field* nil)

