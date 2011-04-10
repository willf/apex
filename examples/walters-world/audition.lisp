;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/audition.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: audition.lisp,v 1.2 2006/01/15 03:42:56 dalal Exp $


(in-package :common-lisp-user)

;;; ------------------------------------------------------------------------
;;; ----- Audition 

;;; Audition class
;;;

;;; This file creates a class of sound objects corresponding roughly
;;; to visobs used in vision. 

(defclass audob (appob)
  ((name :accessor name :initarg :name :initform nil)
   (pitch :accessor pitch :initarg :pitch :initform nil)
   (loudness :accessor loudness :initarg :loudness :initform nil)
   (contrast :accessor contrast :initarg :contrast :initform nil)
   (texture :accessor texture :initarg :texture :initform nil)
   (elements :accessor elements :initarg :elements :initform nil)
   (contains :accessor contains :initarg :contains :initform nil)
   (contained-by :accessor contained-by :initarg :contained-by 
		 :initform nil)
   (spcl :accessor spcl :initarg :spcl :initform nil)))

(defclass audition (resource) nil)

(defmethod parent ((x audob))
  (contained-by x))
