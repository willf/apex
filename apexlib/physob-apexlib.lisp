;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Physob library
;;; apex/apexlib/physob-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: physob-apexlib.lisp,v 1.19 2006/01/15 03:42:50 dalal Exp $

;;; Physob (physical object) library class and support


(in-package :user)

(require-apex-library "visob")
(require-apex-library "geometry")


;;; Container class and related methods to support object tree.

(defclass component-set (appob)
  ((components
    :type list                          ; list(physob)
    :accessor components
    :initform nil)
   (containing-physob                   ; physob
    :accessor containing-physob)
   (component-slots
    :allocation :class
    :type list
    :initform '(components)
    :reader component-slots)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(containing-physob))
   (parent-slot
    :allocation :class
    :reader parent-slot
    :initform 'containing-physob)))

(defmethod pos ((x component-set))
  (pos (containing-physob x)))

(defmethod locale ((x component-set))
  (locale (containing-physob x)))


;;; ------------------------------------------------------------------
;;; ----- Physobs
;;; ------------------------------------------------------------------

;;; One of the main reasons to have this, given that mass and temp are
;;; usually not relevant, is to have a better default name for
;;; application objects than would result from "visob-" or "appob-"
;;; prefixes.

(defclass physob (visob)
  ;; should inherit from other perception-linked classes besides visob
  ;; inherits appearance slots plus: locale, pos, activities,...
  ((component-slots
    :allocation :class
    :type list
    :initform '(component-set)
    :reader component-slots)
   (parent-slot
    :allocation :class
    :initform 'component-of
    :reader parent-slot)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(component-of))
   (component-set                       ; component-set
    :accessor component-set
    :initform (make-instance 'component-set :name "Components"))
   (component-of                        ; appob
    :accessor component-of
    :initarg :component-of
    :initform nil)
   (temp :accessor temp :initarg :temp :initform nil)
   (mass :accessor mass :initarg :mass :initform .01))) ;small default

(defmethod initialize-instance :after ((x physob) &rest initargs)
  (setf (containing-physob (component-set x)) x))

    
;;; ------------------------------------------------------------------
;;; --- Physob creation and assembly

;;; Every physical object included in the simulation environment should
;;; be assmbled, even if there is no custom assembly method for its
;;; class.

;;; ! note: there are standard things to do with components such as set the
;;; components and component-of slots, inherit locale,.. maybe this needs to be
;;; automated somewhere...
;;; ! remove from vfield if invisible or contained-by other (opaque) object


(defmethod make-sub-component ((obj physob) (parent appob))
   ;; set parent only; assume other code makes obj child of parent
   (setf (component-of obj) parent))

(defmethod make-sub-component ((obj physob) (parent component-set))
   ;; set children
   (push obj (components parent))
   ;; set parent
   (setf (component-of obj) parent))

(defmethod make-sub-component ((obj physob) (parent physob))
  (make-sub-component obj (component-set parent)))

(defmethod assemble :before ((obj physob) &key component-of)
  ;; physob * opt(appob) -> ()
  (if component-of (make-sub-component obj component-of))
  ;; If parent object is not a locale, put this object in the locale of
  ;; the parent.  ! I question the soundness of this -- KMD
  (if (and (component-of obj) (not (locale obj)))
      (setf (locale obj) (locale (component-of obj))))
  (add-to-vfield obj (locale obj)) 
  (pushnew obj (contents (locale obj)))
  (cond  ;; infer pos/refpos where possible
   ((and (vertices obj) (refpos obj) (not (pos obj)))
    (setx (pos obj) (refpos->pos obj)))
   ((and (not (pos obj)) (component-of obj))
    (setx (pos obj) (pos (component-of obj))))
   ((and (vertices obj) (pos obj) (not (refpos obj)))
    (setx (refpos obj) (pos->refpos obj)))))

(defmethod visual-val ((p physob))
  (list (or (name p) (id p)) (pos p) '(10 10)))


;;; ------------------------------------------------------------------
;;; --- Object movement

;;; When an object moves to a new location, all the objects that are
;;; affixed to it also move.  An object is considered affixed if it is
;;; in the moved object as indicated by a proposition in *physob-db*.
;;; The objects which are 'on' the moved object are not affixed.

(defmethod moved-to-object ((obj physob) target &key cause)
  (let ((new-pos (pos target))
	(attached (append (find-all-matches `(in ?obj ,obj))
                          ;;(find-all-matches `(on ?obj ,obj))
                          )))
    (moved-to-position obj new-pos :cause cause)
    (assert-physob-relation `(on ,obj ,target))
;;;    (dolist (i attached) 
;;;      (format t "Attached ~a (~a) ~a (~a)~%" obj (pos obj) i (pos i)))
    (dolist (obj2 (union attached (components obj)))
      (when (equal (pos obj) (pos obj2))
	(assert-physob-relation `(on ,obj2 ,target))))
    )
  *physob-db*)

(defmethod moved-to-position ((object physob) new-pos &key cause)
  (let ((attached (append (find-all-matches `(in ?obj ,object))
                          ;;(find-all-matches `(on ?obj ,object))
                          ))
        (original-position (pos object)))
    (reposition-object object new-pos :cause cause)
    (dolist (obj2 (remove-duplicates (union attached (components object))
                                     :test #'equal))
      (let ((adjustment (compute-displacement original-position (pos obj2))))
        (reposition-object obj2 new-pos
                           ;; Adjustment is buggy at the moment.
                           ;;:adjust adjustment 
                           :secondary-p 
                           t :cause cause))))
  *physob-db*)

;;; optional adjust argument used when the object is attached to but
;;; displaced from some other object that is being moved to
;;; NEW-POS. Setting SECONDARY-P to T indicates that this object is
;;; moving becasue the surface it is on is moving so we will not alter
;;; the relations.
;;;

(defmethod reposition-object ((object physob) new-pos &key adjust cause secondary-p)
  (let* ((old-pos (pos object))
	 (displacement (compute-displacement new-pos old-pos))
	 (adjustment (or adjust '(0 0))))
    (setx (pos object) (add-coordinates new-pos adjustment))
    (if (refpos object) ;;update refpos if it is defined
	(setx (refpos object) (add-coordinates (refpos object) displacement)))
    (unless nil ;; was secondary-p
      (dolist (surface (find-all-matches `(on ,object ?surface)))
        (retract-physob-relation `(on ,object ,surface)))
      )))
  

;;; ------------------------------------------------------------------
;;; --- Object containment

;;; returns first component of given type for specified object
(defmethod get-component-type ((physob physob) type)
  (find-if #'(lambda (c) (typep c type)) (components physob)))

(defmethod components ((p physob))      ; physob -> list(physob)
  (components (component-set p)))
