;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Robot gaze
;;; apex/apexlib/robot/gaze.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: gaze.lisp,v 1.3 2006/01/15 03:42:51 dalal Exp $

(in-package :common-lisp-user)

;;;---------------------------------------------------------------------
;;; ----- Gaze and Visual Attention

;;; Gaze represents two resources: fixation, which described the
;;; position of the fovea and thus the location of greatest visual
;;; acuity, and visual-attention which is covert and, in principle,
;;; partially independent of fixation.  In the current simplified
;;; model, fixation is slaved to attention.  Attention itself has two
;;; components: a locus (the object or region being attended), and a
;;; feature specifying what aspect is of current interest.  For
;;; example, one might be interested in all of the green objects in
;;; region-5, or the first datafield of textblock-7.  The feature
;;; component defaults to NIL, meaning all visual properties are
;;; equally interesting.


(defclass gaze (resource)
  ((fixation :accessor fixation :initarg :fixation :initform '(0.0 0.0))
   (locus :accessor locus :initarg :locus :initform nil)
   (feature :accessor feature :initarg :feature :initform nil)
   (t-held :accessor t-held :initarg :t-held :initform 0)))

(defclass gaze-activity (resource-activity) ())

;;; --- fixating a visob

(defclass fixating (gaze-activity)
  ((object :accessor object :initarg :object :initform nil)))

;;; primary object is the gaze resource that is doing the fixating
(defmethod complete-activity ((act fixating) (gaze-1 gaze))
  (signal-event (fixated gaze-1 (object act))))

;; Error keyword list (:cause cause) should only contain keys
;; (:attributes :agent) ignored, back to top level.
(defmethod fixated (gaze-resource visob &key cause)
  ;; (setx (locus gaze-resource) visob :cause cause)
  (setx (locus gaze-resource) visob :attributes cause)
  ;;(setx (fixation gaze-resource) (pos visob) :cause cause)
  (setx (fixation gaze-resource) (pos visob) :attributes cause)
  (start-activity 'holding-gaze gaze-resource :update-interval 100
		  :task nil :cause cause)
  )

;;; Note that holding-gaze will be stopped (not completed) whenever 
;;; any new resource-activity involving gaze is started.

(defclass holding-gaze (gaze-activity) ())

(defmethod update-activity ((act holding-gaze) (gaze-1 gaze))
  (let ((hold-time (- (current-time) (start-time act))))
    (cogevent `(held-gaze ,(locus gaze-1) ,hold-time)
	      (agent-of gaze-1))))  ;; the human agent


;;; ! may be desirable to be able to set fixation to point without
;;; specifying visob, then have attention either go broad or set to
;;; (closest-visobfile <pos> <allobjects>)

;;; --- Setting visual interest

;;; Sets the interest parameter of the gaze resource, causing vision to
;;; create a list of objects in the currently  attended region (any visob 
;;; with vertices) that have a specified interest feature.  This is a 
;;; precursor to doing a sequential search.  

(defclass setting-visual-interest (gaze-activity)
  ((feature :accessor feature :initarg :feature)))

(defmethod complete-activity ((act setting-visual-interest) (gaze-1 gaze) )
  (signal-event (set-interest gaze-1 (feature act)))
  (cogevent `(set-interest ,(locus gaze-1) ,(feature gaze-1))
	    (agent-of gaze-1) :trigger-asa t))

(defun set-interest (gaze-resource feature &key cause)
;;;  (traceevent 'worldevent `(set-interest ,gaze-resource ,feature))
  (setx (interest gaze-resource) feature :cause cause))

;;; ---- Closest-visob
;;;
;;; Finds the nearest visob to a given position.  Used to set
;;; attention locus following a position-specified gaze shift.

(defun closest-visobfile (pos objset)
  (if (null objset)
      nil  ;; no visobfiles known
    (let* ((best (first objset)) 
	   (best-dist (distance pos (pos (visobfile-visob best)))))
      (mapc #'(lambda (obj)
		(let ((d (distance pos (pos (visobfile-visob obj)))))
		  (when (< d best-dist)
		    (setf best obj) (setf best-dist d))))
	    (rest objset))
      best)))

