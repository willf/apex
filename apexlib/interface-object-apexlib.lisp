;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Interface object 
;;; apex/apexlib/interface-object-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: interface-object-apexlib.lisp,v 1.4 2006/01/15 03:42:49 dalal Exp $


(in-package :user)

(require-apex-library "physob")

(defclass interface-object (physob)
  ((name
    :initarg :name
    :accessor name
    :initform nil)
   (state
    :initarg :state
    :accessor state
    :initform nil)
   (device
    :initarg :device
    :accessor device
    :initform nil)
   (dimensions
    :initarg :dimensions
    :accessor dimensions
    :initform nil)))

(defmethod set-visual-val ((io interface-object) pos dim)
  (setf (pos io) (copy-list pos))
  (setf (dimensions io) (copy-list dim)))

(defmethod visual-val ((io interface-object))
  (list (or (name io) (id io)) (pos io) (dimensions io)))


;;;**********************************************************************
;;; support for creating & updating graphical objects
;;;**********************************************************************
(defmethod create-wireframe-graphical-object ((obj interface-object) &key (auto-update nil) (view-type 'xy))
  (update-graphical-object obj :auto-update auto-update :view-type view-type))

(defmethod update-graphical-object ((obj interface-object) &key (auto-update nil) (view-type 'xy))
  ;;interface-objects use pos and dimension to define rectangle
  ;;update coordinates of graphical object, or create new graphical-object if none exists
  (let* ((gobj (graphical-object obj))
	 (x (get-obj-x obj))
	 (y (get-obj-y obj))
	 (width (get-obj-width obj))
	 (height (get-obj-height obj)))
  (cond ((or (null gobj) (not (typep gobj 'rect)))
	 ;;create a new graphical object
	 (setq gobj (make-instance 'rect
				   :auto-update auto-update
				   :x x :y y 
				   :width width
				   :height height
				   :fill "white" :stroke "black"))
	 (setf (graphical-object obj) gobj))
	(t
	  ;;update existing graphical object
	 (setf (auto-update gobj) auto-update
	       (x gobj) x
	       (y gobj) y
	       (width gobj) width
	       (height gobj) height)))
  gobj))


(defmethod get-obj-x ((obj interface-object))
  (let ((pos (pos obj)))
    (first pos)))

(defmethod get-obj-y ((obj interface-object))
  (let ((pos (pos obj)))
    (second pos)))

(defmethod get-obj-width ((obj interface-object))
  (let ((dim (dimensions obj)))
    (first dim)))

(defmethod get-obj-height ((obj interface-object))
  (let ((dim (dimensions obj)))
    (second dim)))

