;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/appob.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: appob.lisp,v 1.9 2006/01/15 03:43:01 dalal Exp $

;;; The "Application Object" class.

(in-package :user)

;;; This is the supertype of all objects (agents, etc) in an Apex
;;; application.  Its main function is as a type.  It also stores the
;;; event history for the object (! This functionality is perhaps
;;; vestigial and not used in a useful way).

(defclass appob (id-mixin)
  ((history                             ; event history
    :type list                          ; list of Event instances
    :initform nil                       
    :accessor history)
   (activities                          ; associated activities
    :type list                          ; list(Activity)
    :initform nil 
    :accessor activities)
   ;; Moved here from Physob (library) because now used by Agent, and
   ;; Appob is their least common ancestor.
   (func-relations :accessor func-relations :initform nil)
   (locale                              ; containing Locale object
    :accessor locale
    :initform nil
    :initarg :locale)
   (graphical-object
    :accessor graphical-object
    :initform nil
    :initarg :graphical-object)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(func-relations))
   (parent-slot
    :allocation :class
    :type symbol
    :reader parent-slot
    :initform 'locale)
   ))

(defmethod (setf  graphical-object) :after (new-value (obj appob))
  ;;initialize id slot of graphical-object to be the same as appob id
  (if (and new-value (slot-exists-p new-value 'id) (not (slot-boundp new-value 'id)) )
      (setf (id new-value) (id obj))))

(defmethod initialize-instance :after ((obj appob) &rest initargs)
  ;;if graphical-object slot is non-nil, set id for graphical-object to be the same as that of appob
  (if (slot-value obj 'graphical-object)
      (setf (id (graphical-object obj)) (id obj))))
