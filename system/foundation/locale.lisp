;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/locale.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: locale.lisp,v 1.9 2006/01/15 03:43:01 dalal Exp $

;;; The Locale class.

(in-package :user)

;;; A Locale is a kind of Appob and serves two purposes.  First, it is a
;;; conceptual environment in which any number of other Appobs can
;;; exist.  It might be said that a locale "surrounds" a Appob, though
;;; it not necessary a physical entity.  Second, a Locale continues to
;;; serve as the host of a "visual field", which is a set of Appobs
;;; (formerly Visobs) said to be currently visible.  !! KMD thinks the
;;; visual field concept needs reworking and refactoring to a lower
;;; level (e.g. human).

(defclass locale (appob)
  ((contents                            ; All Appobs in this locale.
    :type list                          ; list(Appob)
    :accessor contents
    :initform nil)
   (vfield                              ; Visible Appobs in this locale.
    :type list                          ; list(Appob)
    :accessor vfield
    :initarg :vfield
    :initform nil)
   (extent
    :type list
    :accessor extent
    :initarg :extent
    :initform '((0 0 0) (1000 1000 1000))))) ;; min xyz and max xyz

(defmethod initialize-instance :after ((x locale) &rest initargs)
  (if *application* (pushnew x (locales *application*))))

;;; Returns the list of "top level objects" in a locale.
;;; ! In current applications the CONTENTS slot of locale contains more
;;; than just top level objects.
;
(defmethod appob-contents ((this locale))  ; locale -> list(appob)
  (filter
   #'(lambda (x) (equal this (appob-parent x)))
   (contents this)))
