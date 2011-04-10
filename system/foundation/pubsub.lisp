;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/pubsub.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: pubsub.lisp,v 1.8 2006/01/15 03:43:01 dalal Exp $

;;; Publish-Subscribe mechanism

;;; This is a mechanism for handling interactions between agents and
;;; other subclasses of PS-Mixin, the property mixin class defined
;;; herein.  The basic idea is that actions of one entity are observed
;;; (or otherwise) affect other entities through intermediate "media" --
;;; e.g. the auditory-field is a medium that allows sounds emitted by
;;; one agent to be detected by others.  When an entity does something
;;; that could be observed through a given medium, it "publishes" to a
;;; medium-specific router.  Entities that "subscribe" to that router
;;; are immediately messaged that an event has occurred.  Thus, all
;;; agents in a room with hearing might subscribe to an auditory-field
;;; router linked to the room.  When anything makes a sound, this is
;;; published to the auditory field router which then signals all
;;; subscribers (via cogevents) that the event has occurred.

(in-package :user)

;;; The router

(defclass ps-router (appob)
  ((subscribers                         ; list(ps-mixin)
    :initform nil
    :accessor subscribers)))

;;; A mixin class for enabling entities to publish and subscribe.

(defclass ps-mixin () ())  ; effectively just a type for now

;;; Predicates used by type-check macro.

(defparameter tc-ps-mixin
    #'(lambda (x) (typep x 'ps-mixin)))

(defparameter tc-ps-router
    #'(lambda (x) (typep x 'ps-router)))

;;; The messaging method, which sends published messages to each
;;; subscriber.  Specific subclasses of ps-mixin must specialize this
;;; method; the default method raises an error.
;
(defmethod deliver ((recipient ps-mixin) message
                    &key trigger-asa suppress-log attributes)
  (declare (ignore message trigger-asa suppress-log attributes))
  (error "No DELIVER method defined for recipients of type ~a"
         (type-of recipient)))


;;; Initialization and reset

(defmethod reset ((r ps-router))
  (setf (subscribers r) nil))

;;; Subscription

(defmethod subscribe ((x ps-mixin) (r ps-router))
  (push x (subscribers r)))

;;; Publication

;;; Default publish/subscribe router
(defparameter *default-ps-router*
    (make-instance 'ps-router :name 'default-router))

(defun inform (eventform
               &key (router *default-ps-router*)
                    author suppress-log attributes)
  (type-check inform
    (eventform (list any))
    (router ps-router)
    (author (opt ps-mixin))
    (suppress-log bool)
    (attributes (list any))) ; ! improve this check
  (dolist (subscriber (subscribers router))
    (when (or (null author)
              (not (equal author subscriber)))
      (deliver subscriber eventform
               :suppress-log suppress-log
               :trigger-asa t
               :attributes attributes))))

(defun router-named (name)
  (find-if #'(lambda (r)
	       (equal name (name r)))
	   (routers *application*)))

