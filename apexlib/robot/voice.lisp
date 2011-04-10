;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Robot voice
;;; apex/apexlib/robot/voice.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: voice.lisp,v 1.2 2006/01/15 03:42:51 dalal Exp $

(in-package :common-lisp-user)

(apex-info :version "2.4")

;;; ------------------------------------------------------------------------
;;; ----- Voice

;;; Voice class
;;;

;;; The class has no state parameters other than those inherited from
;;; resource and those implicit in its activities.

(defclass voice (resource) ())

;;; At present, the methods are only place holders for routines that
;;; provide real functionality.

(defvar *aud-field* nil)

(defclass speaking (resource-activity)
  ((utterance :initarg :utterance :accessor utterance)
   (aud-field :initarg :aud-field :accessor aud-field)))

(defmethod initialize-activity ((act speaking) (voice voice))
  ;; ! also should compute default time required based on length of utterance
  (setf (aud-field act) *aud-field*))

(defmethod complete-activity ((act speaking) (voice voice))
  ;; do nothing if utterance can't be published and made hearable in
  ;; some auditory field
  (when (aud-field act)  
    (publish-to (utterance act) (aud-field act))))

(defconstant given-aud-attribute-types    
    '(pitch loudness contrast texture contains elements)) 
