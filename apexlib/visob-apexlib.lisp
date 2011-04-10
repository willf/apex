;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Visob library
;;; apex/apexlib/visob-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: visob-apexlib.lisp,v 1.16 2006/01/15 03:42:50 dalal Exp $


(in-package :common-lisp-user)

(require-apex-library "geometry")

;;; -------------------------------------------------------------------------
;;;    Representing the visual field
;;; -------------------------------------------------------------------------

;;; The "objective" visual environment is represented as a set of
;;; VISOB structures, each implicitly a set of propositions about the
;;; appearance of a potentially perceivable object.  Visual attributes
;;; are processed by the vision resource according to attribute-type.
;;; Thus, the rules determining whether, e.g., the color of an object
;;; is seen may be different from the rules for shape or orientation.
;;; The set of visual objects in a locale form the "visual field."

;;; --- No-info
;;;
;;; This value is used for attribute values that are undefined.

(defconstant no-info nil) 

;;; ----- Visual objects

(defclass visob (appob)
  ((name :accessor name :initarg :name :initform no-info)
   ;; unique, meaningful name for object (e.g. george-jetson)
   ;;   (pos :accessor pos :initarg :pos :initform no-info)
   ;; (x y) pair specifying location of object centroid within locale
   ;; RPB 12/06/04 (x y z) triple
   (Pos :accessor pos :initarg :pos :initform '(0 0 0))
   (dimensions :accessor dimensions :initarg :dimensions :initform '(1 1 1))
;;; For 3D rendering in sherpa.  faces is a list of lists of xyz coords as
;;; follows:
;;; (
;;; (four xyz coords for the top face)
;;; (four xyz coords for the bottom face) 
;;; (four xyz coords for the front face)
;;; (four xyz coords for the back face)
;;; (four xyz coords for the left face)
;;; (four xyz coords for the right face)
;;; )
   (faces :accessor faces :initarg :faces :initform no-info)
   (shape :accessor shape :initarg :shape :initform no-info)
   ;; list of possible shape descriptions
   (color :accessor color :initarg :color :initform no-info)
   ;; attribute values from r5 color database name list
   (orientation :accessor orientation :initarg :orientation :initform no-info)
   (contrast :accessor contrast :initarg :contrast :initform no-info)
   (texture :accessor texture :initarg :texture :initform no-info)
   (blink :accessor blink :initarg :blink :initform no-info)
   (elements :accessor elements :initarg :elements :initform no-info)
   ;; list of object's visual components
   (vertices :accessor vertices :initarg :vertices :initform no-info)
   ;; list of (x Y) pairs; offsets from pos attribute
   ;; RPB 12/06/04 (x y z) triples
;;   (vertices :accessor vertices :initarg :vertices :initform 
;;	     '((1 1 1) (1 1 -1) (1 -1 -1) (1 -1 1) ;; fwd, right up/dn : fwd, left dn/up
;;	       (-1 1 1) (-1 1 -1) (-1 -1 -1) (-1 -1 1) ;; back, right up/dn : back, left dn/up
;;	       ))
   ;;(Refpos :accessor refpos :initarg :refpos :initform no-info)
   ;; (x y) location of first listed vertex; used for initialization
   ;; RPB 12/06/04 (x y z) location of first listed vertex
   (Refpos :accessor refpos :initarg :refpos :initform '(0 0 0))
   (contains :accessor contains :initarg :contains :initform no-info)
   ;; list of visobs contained within vertices boundaries
   (contained-by :accessor contained-by :initarg :contained-by 
		 :initform no-info)
   (spcl :accessor spcl :initarg :spcl :initform no-info)
   ))


(defmethod assemble ((obj visob) &key component-of)
  (declare (ignore component-of))
  obj)


;;; --- Attribute types for Visob
;;;
;;; Given attributes are specified directly for a visob.  Other types
;;; may be added if they become useful incl., e.g., size, motion, 
;;; opacity, facing (v.v. standard view of object)

;;; ! should this include contained-by.  Also, not clear why contain
;;; info not considered derived.

(defconstant given-vis-attribute-types    
    '(pos color orientation contrast texture shape blink 
      contains elements)) 

;;; Derived attributes are computed dynamically
(defconstant derived-vis-attribute-types '(count searchmap))

;;; Conjunctive attributes are used to abbreviate multiple 
;;; attribute relations.  E.g. (contains X (A B)) means that
;;; (contains X A) and (contains X B).

(defconstant conjunctive-vis-attribute-types '(shape contains))

(defconstant native-vis-attribute-types
    (append given-vis-attribute-types derived-vis-attribute-types
	    conjunctive-vis-attribute-types)
)

;;; --- Application-specific attributes
;;;
;;; Support for application-specific visual attributes.  E.g. an 
;;; application might need an attributes such as touching, above,
;;; or sparkle that are not supported.  To determine whether vision
;;; detects the attribute, the default current-vprop-value method
;;; is used.

(defparameter special-vis-attribute-types nil)

(defun initialize-special-vis-attributes ()
  (setf special-vis-attribute-types nil))

(defun declare-special-vis-attribute (att)
  (push att special-vis-attribute-types))

(defun get-special-vis-attribute-value (att visob)
  (let* ((entry (assoc att (spcl visob)))
	 (value (and entry (second entry))))
    (if value value no-info)))

;;; ------------------------------------------------------------------
;;; ----- Creating, modifying and updating visual objects

;;; --- Set-initial-appearance

;;; SETX calls LOG-EVENT which calls this function.  Logs the a visob
;;; appearance change with the locale (the basic method logs with the
;;; changed object itself) and also causes containment properties of
;;; the object to be recomputed.

;;; !! what is update-continment-info doing here?

(defmethod add-to-history ((e cogevent) (v visob))
  (let* ((event-type (first (content e)))
	 (changed-visob (second (content e)))
	 (locale (ignore-errors (locale changed-visob))))
    (call-next-method)
    (if locale (add-to-history e locale))
    (if (equal event-type 'pos)
	(update-containment-info changed-visob))))

;;; ------------------------------------------------------------------
;;; ----- Interacting with visual fields

;;; --- Add-to-vfield
;;;
;;; This function adds a new transient visob to the vfield associated
;;; with a particular locale.  It is typically called when the world
;;; is initialized, but may also be called when new objects come into
;;; existence mid-simulation.

;;; !! need to force full vision update

(defmethod add-to-vfield ((visob visob) (locale locale))
  (push visob (vfield locale))
  (setx (locale visob) locale)
  (update-containment-info visob))

;;; --- Remove-from-vfield

(defmethod remove-from-vfield ((visob visob) (locale locale))
  (setx (vfield locale) (remove visob (vfield locale)))
  (remove-invertable-link-info visob 'contains 'contained-by))

;;; Convenient interface for above two

(defmethod make-visible ((visob visob))
  (add-to-vfield visob (locale visob)))

(defmethod make-invisible ((visob visob))
  (remove-from-vfield visob (locale visob)))

;;; Geometry related

;;; computes object centroid based on vertices and reference position
;;; value equal to the absolute coordinates of the first listed vertex 
;;;
(defmethod refpos->pos ((visob visob))
  (let ((relative-centroid (compute-centroid (vertices visob))))
    (add-coordinates relative-centroid (refpos visob))))

 ;;; computes refpos given pos (absolute centroid) and relative vertices

(defmethod pos->refpos ((visob visob))
  (let* ((relative-centroid (compute-centroid (vertices visob)))
	 (reference-vertex (first (vertices visob)))
	 (relative-displacement 
	  (compute-displacement relative-centroid reference-vertex)))
    (add-coordinates relative-displacement (pos visob))))

 ;;; --- Deriving and maintaining containment information

;;; Currently, only containment info needs to be derived from other
;;; visob attributes.  In the future, other derived info may be
;;; represented including especially other spatial properties such as
;;; above/below and left-of/right-of.  Note that both contains and its
;;; inverse (contained-by) are represented; this requires special
;;; support.

(defmethod update-containment-info ((visob visob))
  (let ((pos (pos visob))
	(contains nil)
	(contained-by nil)
	(vfield (vfield (locale visob))))
    (when pos
      (loop for v in vfield
	  when (and (vertices v) 
		    (in-polygonp pos (vertices v)))
	  do (push v contained-by)
	  when 
	    (and (vertices visob) (pos v)
		 (in-polygonp (pos v) 
		   (compute-vertices (vertices visob) (pos visob))))
	  do (push v contains))
      (update-invertable-link visob 'contains contains 'contained-by)
      (update-invertable-link visob 'contained-by contained-by 'contains)
      )))

(defmethod update-invertable-link ((visob visob) link newval inverse) 
  (let ((oldval (slot-value visob link)))
    (when (not (equal newval oldval))
      (let ((adds (set-difference newval oldval :test #'equal))
	    (dels (set-difference oldval newval :test #'equal)))
	(mapc #'(lambda (v) 
		  (push v (slot-value visob link))
		  (push visob (slot-value v inverse)))
	      adds)
	(mapc #'(lambda (v)
		  (setx (slot-value visob link)
			(remove v (slot-value visob link)))
		  (setx (slot-value v inverse)
			(remove visob (slot-value v inverse))))
	      dels)))))

;;; Modifies conjunctive attributes with inverses to reflect deletion
;;; of all specific relations.  (e.g. removing contains information
;;; from A which contains C and D, and is contained-by F and G requires
;;; modifying all of these objects.

(defun remove-invertable-link-info (visob link inverse)
  (mapc 
   #'(lambda (v) 
       (setx (slot-value v link) (remove visob (slot-value v link))))
   (slot-value visob inverse))
  (setx (slot-value visob inverse) nil)
  (mapc 
   #'(lambda (v) 
       (setx (slot-value v inverse) (remove visob (slot-value v inverse))))
   (slot-value visob link))
  (setx (slot-value visob link) nil))


;;;**********************************************************************
;;; support for creating & updating graphical objects
;;;**********************************************************************

(defmethod create-wireframe-graphical-object ((obj visob) &key (auto-update nil) (view-type 'xy))
  (update-graphical-object obj :auto-update auto-update :view-type view-type))


(defmethod update-graphical-object ((obj visob) &key (auto-update nil) (view-type 'xy))
  ;;update coordinates of polygon graphical-object, or create new graphical-object if none exists
  (let* ((gobj (graphical-object obj))
	 (x-offset (get-obj-x obj)) ;; (first (pos obj)
	 (y-offset (get-obj-y obj)) ;; (second (pos obj)
	 (z-offset (get-obj-z obj))  ;; (third (pos obj))
	 (path-data (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string 
      (s path-data)
      (loop for face in (faces obj)
	    do 
	    (format s "M")
	    (loop for pt in face
			    ;;; RPB 1/31/05 robot apps will update the
			    ;;; faces from the location/pos of the robot
; 		  for x = (+ x-offset (first pt))
; 		  for y = (+ y-offset (second pt))
; 		  for z = (+ z-offset (third pt))
		  for x = (first pt)
		  for y = (second pt)
		  for z = (third pt)
		  do
		  (cond ((eq view-type 'xz)
			 (format s " ~a ~a" x z))
			((eq view-type 'yz)
			 (format s " ~a ~a" y z))
			(t
			 (format s " ~a ~a" x y))))
	    (format s " z ")))
    (cond ((or (null gobj) (not (typep gobj 'path)))
	   ;;create a new graphical object
	   (setq gobj (make-instance 'path 
				     :auto-update auto-update
				     :d path-data
				     :fill "white" :stroke "black"))
	   (setf (graphical-object obj) gobj))
	  (t
	   ;;update existing graphical object
	   (setf (auto-update gobj) auto-update)
	   (setf (d gobj) path-data)))
    gobj))
					   

(defmethod get-obj-x ((obj visob))
  (let ((pos (pos obj)))
    (first pos)))

(defmethod get-obj-y ((obj visob))
  (let ((pos (pos obj)))
    (second pos)))

(defmethod get-obj-z ((obj visob))
  (let ((pos (pos obj)))
    (third pos)))

;;(print "end visob")
