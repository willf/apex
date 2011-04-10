;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/graphical-object.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: graphical-object.lisp,v 1.5 2006/01/15 03:43:01 dalal Exp $

(in-package :user)

;;hashtable of all graphical-objects  (key=id, value=object instance)
(defvar *graphical-instances* (make-hash-table))

(defclass graphical-object ()
  ((id
    :accessor id
    :initarg :id)
   ;;list of labels for object (each item is a text object)
   (label-list
    :accessor label-list
    :initform nil
    :initarg :label-list)
   (auto-update
    :accessor auto-update
    :initform nil
    :initarg :auto-update)
   )
  
  )
(defmethod initialize-instance :after ((gob graphical-object) &rest initargs)
  ;;initialize instance and add to global table iff id is bound
  (let ((class (class-of gob)))
    (unless (clos:class-finalized-p class) (mop:finalize-inheritance class))
    (if (slot-boundp gob 'id)
	(setf (gethash (id gob) *graphical-instances*) gob))
    ))

(defmethod get-slot-names ((instance graphical-object))
  "Given an INSTANCE, returns a list of the slots in the instance's class."
  (mapcar #'clos:slot-definition-name
          (clos:class-slots (class-of instance))))


(defmethod print-object ((gob graphical-object) stream)
  (format stream "~a" (to-svg gob)))

(defmethod to-svg ((gob graphical-object))
  ;;Convert to SVG string
  (let ((slots (get-slot-names gob))
	(outstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string 
      (s outstr)
      (format s "<~a " (type-of gob))
      (loop for slot in slots 
	    do
	    (cond ((and (is-printable-slot gob slot) (slot-boundp gob slot))
		   (format s "~a=\"~a\" "  (slot-print-name gob slot) (slot-print-value gob slot)))))

      (cond ((slot-exists-p gob 'value)
	     (format s ">~a" (slot-print-value gob 'value))
	     (format s "</~a>~%" (type-of gob))
	     )
	    (t
	     (format s "/>~%")))


      (if (slot-boundp gob 'label-list)
	  (loop for label in (label-list gob)
		do
		(format s "~a~%" (to-svg label))))
      )
    
    outstr))

(defmethod is-printable-slot ((gob graphical-object) slot)
  ;;return t iff slot should be printed when converting to SVG
  ;;(most slots map directly to XML attribute, but not all)
  (not (member  slot '(label-list value auto-update))))

(defmethod slot-namespace ((gob graphical-object) slotname)
  ;;returns XML namespace for slot
  (declare (ignore slotname))
  nil)

(defmethod slot-print-name ((gob graphical-object) slotname)
  ;;returns print representation for slotname
  ;;used to convert to svg string
  (let ((namespace (slot-namespace gob slotname)))
    (if namespace
	(format nil "~a:~a" namespace slotname)
      slotname)))

(defmethod slot-print-value ((gob graphical-object) slotname)
  ;;returns printed version of value
  ;; can be used to convert from slot syntax to svg syntax
  (let ((val (slot-value gob slotname)))
    (if (typep val 'ratio)
	(float val)
      val)))

;;;----------------------------------------------------------------------
;;; models SVG Paint.attrib attribute set
;;;----------------------------------------------------------------------
(defclass paint-attrib ()
  ((color
    :accessor color
    :initarg :color)
   (fill
    :accessor gfill   ;;can't call it fill cause fill funct already exists.... doh!
    :initarg :fill)
   (fill-rule
    :accessor fill-rule
    :initarg :fill-rule)
   (stroke
    :accessor stroke
    :initarg :stroke)
   (stroke-dasharray
    :accessor stroke-dasharray
    :initarg :stroke-dasharray)
   (stroke-dashoffset
    :accessor stroke-dashoffset
    :initarg :stroke-dashoffset)
   (stroke-linecap
    :accessor stroke-linecap
    :initarg :stroke-linecap)
   (stroke-linejoin
    :accessor stroke-linejoin
    :initarg :stroke-linejoin)
   (stroke-miterlimit
    :accessor stroke-miterlimit
    :initarg :stroke-miterlimit)
   (stroke-width
    :accessor stroke-width
    :initarg :stroke-width)
   (color-rendering
    :accessor color-rendering
    :initarg :color-rendering)
   ))

;;;----------------------------------------------------------------------
;;;models SVG Opacity.attrib attribute set
;;;----------------------------------------------------------------------
(defclass opacity-attrib ()
  ((opacity
    :accessor opacity
    :initarg :opacity)
   (stroke-opacity
    :accessor stroke-opacity
    :initarg :stroke-opacity)
   (fill-opacity
    :accessor fill-opacity
    :initarg :fill-opacity)
   ))

;;;----------------------------------------------------------------------
;;;models SVG Text.attrib attribute set
;;;----------------------------------------------------------------------
(defclass text-attribe ()
  ((writing-mode
    :accessor writing-mode
    :initarg :writing-mode)))

;;;----------------------------------------------------------------------
;;;models SVG TextContent.attrib attribute set
;;;----------------------------------------------------------------------
(defclass text-content-attrib ()
  ((alignment-baseline
   :accessor alignment-baseline
   :initarg :alignment-baseline)
  (baseline-shift
   :accessor baseline-shift
   :initarg :baseline-shift)
  (direction
   :accessor direction
   :initarg :direction)
  (dominant-baseline
   :accessor dominant-baseline
   :initarg :dominant-baseline)
  (glyph-orientation-horizontal
   :accessor glyph-orientation-horizontal
   :initarg :glyph-orientation-horizontal)
  (glyph-orientation-vertical
   :accessor glyph-orientation-vertical
   :initarg :glyph-orientation-vertical)
  (kerning
   :accessor kerning
   :initarg :kerning)
  (letter-spacing
   :accessor letter-spacing
   :initarg :letter-spacing)
  (text-anchor
   :accessor text-anchor
   :initarg :text-anchor)
  (text-decoration
   :accessor text-decoration
   :initarg :text-decoration)
  (unicode-bidi
   :accessor unicode-bidi
   :initarg :unicode-bidi)
  (word-spacing
   :accessor word-spacing
   :initarg :word-spacing)))
  
;;;----------------------------------------------------------------------
;;;models SVG Font.attrib attribute set
;;;----------------------------------------------------------------------
(defclass font-attrib ()
  ((font-family
    :accessor font-family
    :initarg :font-family)
   (font-size
    :accessor font-size
    :initarg :font-size)
   (font-size-adjust
    :accessor font-size-adjust
    :initarg :font-size-adjust)
   (font-stretch
    :accessor font-stretch
    :initarg :font-stretch)
   (font-style
    :accessor font-style
    :initarg :font-style)
   (font-variant
    :accessor font-variant
    :initarg :font-variant)
   (font-weight
    :accessor font-weight
    :initarg :font-weight)))

;;;----------------------------------------------------------------------
;;;models SVG GraphicalEvents.attrib  attribute set
;;;----------------------------------------------------------------------
(defclass graphical-events-attrib ()
  ((onfocusin
    :accessor onfocusin
    :initarg onfocusin)
   (onfocusout
    :accessor onfocusout
    :initarg onfocusout)
   (onactivate
    :accessor onactivate
    :initarg onactivate)
   (onclick
    :accessor onclick
    :initarg onclick)
   (onmousedown
    :accessor onmousedown
    :initarg onmousedown)
   (onmouseup
    :accessor onmouseup
    :initarg onmouseup)
   (onmouseover
    :accessor onmouseover
    :initarg onmouseover)
   (onmousemove
    :accessor onmousemove
    :initarg onmousemove)
   (onmouseout
    :accessor onmouseout
    :initarg onmouseout)
   (onload
    :accessor onload
    :initarg onload)
   ))


   
;;;----------------------------------------------------------------------
;;; SVG File based graphical-object
;;; subclass of graphical-object used when graphical representation is an SVG file
;;;----------------------------------------------------------------------

(defclass file-svg-object (graphical-object)
  ((filename
    :accessor filename
    :initarg :filename
    :initform nil))
  )

;;;----------------------------------------------------------------------
;;; Shapes and Text objects
;;;----------------------------------------------------------------------

(defclass rect (graphical-object graphical-events-attrib paint-attrib opacity-attrib)
  ((x
    :accessor x
    :initarg :x 
    :initform 0)
   (y
    :accessor y
    :initarg :y 
    :initform 0)
   (height 
    :accessor height
    :initarg :height 
    :initform 0)
   (width  
    :accessor width  
    :initarg :width 
    :initform 0)
   (rx
    :accessor rx
    :initarg :rx 
    :initform 0)
   (ry
    :accessor ry
    :initarg :ry 
    :initform 0)))

(defclass circle (graphical-object graphical-events-attrib paint-attrib opacity-attrib)
  ((cx
    :accessor cx
    :initarg :cx 
    :initform 0)
   (cy
    :accessor cy
    :initarg :cy 
    :initform 0)
   (r
    :accessor r
    :initarg :r 
    :initform 0)))

(defclass path (graphical-object graphical-events-attrib paint-attrib opacity-attrib)
  ((d
    :accessor d
    :initarg :d 
    :initform 0)))

(defclass polygon (graphical-object graphical-events-attrib paint-attrib opacity-attrib)
  ((points
    :accessor points
    :initarg :points 
    :initform 0)))

(defmethod slot-print-value ((gob polygon) slotname)
  ;;overrides method for graphical-object
  ;;(converts points from list syntax to string it necessary)
  (cond ((eq slotname 'points)
	 (if (listp (points gob))
	     (string-right-trim " " (format nil "~:{~A,~A ~}" (points gob)))
	   (slot-value gob slotname)))
	(t
	 (slot-value gob slotname))))



(defclass line (graphical-object graphical-events-attrib paint-attrib opacity-attrib)
    ((x1
      :accessor x1
      :initarg :x1 
      :initform 0)
     (y1
      :accessor y1
      :initarg :y1 
      :initform 0)
     (x2
      :accessor x2
      :initarg :x2 
      :initform 0)
     (y2
      :accessor y2
      :initarg :y2 
      :initform 0)))

(defclass image (graphical-object graphical-events-attrib paint-attrib opacity-attrib)
  ((x
    :accessor x
    :initarg :x 
    :initform 0)
   (y
    :accessor y
    :initarg :y 
    :initform 0)
   (height 
    :accessor height
    :initarg :height 
    :initform 0)
   (width  
    :accessor width  
    :initarg :width 
    :initform 0)
   (href
    :accessor href  
    :initarg :href))) 

(defmethod slot-namespace ((img image) slotname)
  (if (eq slotname 'href)
      "xlink"))


(defclass text (graphical-object graphical-events-attrib paint-attrib opacity-attrib text-attribe text-content-attrib font-attrib)  
  ((x
    :accessor x
    :initarg :x 
    :initform 0)
   (y
    :accessor y
    :initarg :y 
    :initform 0)
   (dx
    :accessor dx
    :initarg :dx 
    :initform 0)
   (dy
    :accessor dy
    :initarg :dy 
    :initform 0)
   (rotate
    :accessor rotate
    :initarg :rotate)
   (textLength
    :accessor textLength
    :initarg :textLength)
   (textAdjust
    :accessor textAdjust
    :initarg :textLength)
   (value 
    :accessor value
    :initarg :value)

   ))


(defun set-label (graphical-obj label)
  (setf (label-list graphical-obj) (list label)))

(defun graphical-find (id)
  (gethash id *graphical-instances*))

(defun set-graphical-vals (lis)
  ;; list contains items of form (id arg val arg val ...)
  (loop for item in lis
	for gobj = (graphical-find (first item))
	for all-args = (rest item)
	do
	(cond (gobj
	       (loop for args = all-args then (cddr args) 
		     while args
		     for key = (first args)
		     for val = (second args)
		     do
		     (cond ((eq key :label)
			    ;;set single label
			    (set-label gobj val))
			   (t
			    (eval `(reinitialize-instance ,gobj ,key ,val)))))))))
			    
		     
;;**********************************************************************
;;  convenience functions
;;**********************************************************************
(defmethod add-label ((gobj graphical-object) label x y 
		      &key (font-size 8) (font-family "Arial") (font-weight 'normal) (font-style 'normal)
		      (text-anchor "start") (fill "black"))
  (let ((my-label (make-instance 'text :value label 
				 :x x :y y
				 :text-anchor text-anchor
				 :font-size (font-size-string font-size)
				 :font-family font-family
				 :font-weight font-weight
				 :font-style font-style
				 :fill fill 
				 )))
    (setf (label-list gobj) (push my-label (label-list gobj)))
    my-label))

(defmethod auto-label ((gobj graphical-object) label 
		       &key (font-size 8) (font-family "Arial") (font-weight 'normal) (font-style 'normal)
		       (text-anchor "middle")  (fill "black"))
  ;;don't know how to auto label generic graphical object
  (declare (ignore label font-size font-family font-weight font-styletext-anchor fill))
  )

(defmethod auto-label ((my-rect rect) label 
		       &key (font-size 8) (font-family "Arial") (font-weight 'normal) (font-style 'normal)
		       (text-anchor "middle")  (fill "black"))
  (let ((my-label (make-instance 'text :value label 
				 :text-anchor text-anchor
				 :font-size (font-size-string font-size)
				 :font-family font-family
				 :font-weight font-weight
				 :font-style font-style
				 :fill fill)))
    ;;set label coords to center of rect
    (setf (x my-label) (+ (x my-rect) (floor (width my-rect) 2)))
    (setf (y my-label) (+ (y my-rect) (floor (height my-rect) 2) (floor font-size 2)))
    (set-label my-rect my-label)
    my-label))

(defmethod auto-label ((my-circle circle) label 
		       &key (font-size 8) (font-family "Arial") (font-weight 'normal) (font-style 'normal) (text-anchor "middle"))
  (let ((my-label  (make-instance 'text :value label 
				 :text-anchor text-anchor
				 :font-size (font-size-string font-size)
				 :font-family font-family
				 :font-weight font-weight
				 :font-style font-style)))
     ;;set label coords to center of circle
    (setf (x my-label) (cx my-circle))
    (setf (y my-label) (+ (cy my-circle) (floor font-size 2)))
    (set-label my-circle my-label)))

(defun font-size-string (s)
  (cond ((numberp s)
	 (format nil "~apt" s))
	(t 
	 s)))

(defun update-appob-graphical-object (appobj &key view-type)
  ;;update the graphical object for appobj
  ;;(currently only works for subclasses of visob or interface-object
  (let ((gobj (graphical-object appobj)))
    (if (or (and (find-class 'visob nil) (typep appobj 'visob)) 
	    (and (find-class 'interface-object nil) (typep appobj 'interface-object)))
	(setq gobj (update-graphical-object appobj :view-type view-type)))
    gobj))