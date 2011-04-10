;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Geometry library
;;; apex/apexlib/geometry-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: geometry-apexlib.lisp,v 1.10 2006/01/15 03:42:49 dalal Exp $

;;; Geometry functions and Physob database.


(in-package :common-lisp-user)

;;; ------------------------------------------------------------------
;;; ----- Geometry

;;; These functions are useful for modeling initial placement and
;;; mid-simulation movement of simulated physical objects on a 2d
;;; surface.

;;; ! These functions need serious cleanup
;;; RPB 12/06/04 Adapted the following to handle 3D.
;;; All try to do the right thing if only 2D.
;;; If pos, vertices or refpos slots are involved,
;;; 3D computation is used.


(defun distance (p1 p2)
  (sqrt (+ (expt (- (first p1) (first p2)) 2) 
	   (expt (- (second p1) (second p2)) 2)
	   (if (and (third p1)(third p2))
	       (expt (- (third p1) (third p2)) 2)
	     0))))

(defun add-coordinates (c1 c2)
  `(,(+ (first c1) (first c2)) 
    ,(+ (second c1) (second c2))
    ,@(if (and (third c1)(third c2))
	      (list (+ (third c1)(third c2))))))

(defun compute-displacement (new old)
  `(,(- (first new) (first old)) 
    ,(- (second new) (second old))
    ,@(if (and (third new)(third old))
	      (list (- (third new)(third old))))))

;;; Computes vertices given specified vertex-offsets and a reference
;;; position (usually the centroid of the enclosure).

(defun compute-vertices (vset pos)
  (let ((x (first pos)) (y (second pos)) (z (third pos)))
    (mapcar #'(lambda (v) 
		`(,(+ x (first v)) 
		  ,(+ y (second v))
		  ,@(if (and z (third v))
			(list (+ z (third v))))))
	    vset)))

(defun compute-centroid (v-list)
  (let ((xsum 0) (ysum 0) (zsum 0) 
	(3D? (third (first v-list)))
	(n (length v-list)))
    (dolist (v v-list)
      (setf xsum (+ xsum (first v))) 
      (setf ysum (+ ysum (second v)))
      (if 3D?
	  (setf zsum (+ zsum (third v)))))
    `(,(floor (/ xsum n)) 
      ,(floor (/ ysum n)) 
      ,@(if 3D? (list (floor (/ zsum n)))))))

;;; In 3D in-polygonp becomes 
;;; in-3D-polyp.

;;; Still under construction.

(defun ltoe (p1 p2)
  (and (<= (first p1)(first p2))
       (<= (second p1)(second p2))
       (if (and (third p1)(third p2))
	   (<= (third p1)(third p2))
	 t)))
	   
;;;(defun in-polygonp (p poly)
;;;    (loop for i from 0 to (1- (length poly))
;;;	if (not (ltoe p (nth i poly)))
;;;	return nil
;;;	       finally (return t)))

;;; Original preserved and rewritten with
;;; loop.

(defun in-polygonp (p poly)
  (let (c (n (length poly)))
    (loop for i from 0 to (1- n)
	do
	  (loop for j from (1- n) downto (+ i 2)
	      do
		(let* ((x (first p)) (y (second p)) (z (third p))
		       (vi (nth i poly)) (xi (first vi)) (yi (second vi)) (zi (third vi))
		       (vj (nth j poly)) (xj (first vj)) (yj (second vj)) (zj (third vj)))
		  (if
		      (and
			(or (and (<= yi y) (< y yj)) (and (<= yj y) (< y yi)))
			(< x (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi)))))
		      (setf c (not c))))))
    c))

;;; Generate vertices from the dimensions of
;;; an object, act as if the lower left rear corner
;;; is on (0 0 0). Then go around ccw, then up z
;;; and ccw again.

(defun generate-rectangular-vertices (dims)
  (let ((x (first dims)) (y (second dims)) (z (third dims)))
;;; Old 2D =>    `((0 0) (,x 0) (,x ,y) (0 ,y))
    (if z
	`((0 0 0) (,x 0 0) (,x ,y 0) (0 ,y 0)
		  (0 0 ,z) (,x 0 ,z) (,x ,y ,z) (0 ,y ,z))
      `((0 0) (,x 0) (,x ,y) (0 ,y)))))

;;; This version generates the
;;; vertices from rectangular dimensions and
;;; a reference point

(defun ref->vertices (dims ref)
  (let ((x (first ref)) (y (second ref)) (z (third ref))
	(dx (/ (first dims) 2.0)) (dy (/ (second dims) 2.0)) 
	(dz (if (third dims) (/ (third dims) 2.0))))
    (if z
	`((,(+ x dx) ,(+ y dy) ,(+ z dz)) 
	  (,(+ x dx) ,(+ y dy) ,(- z dz)) 
	  (,(+ x dx) ,(- y dy) ,(- z dz)) 
	  (,(+ x dx) ,(- y dy) ,(+ z dz))
	  (,(- x dx) ,(+ y dy) ,(+ z dz)) 
	  (,(- x dx) ,(+ y dy) ,(- z dz)) 
	  (,(- x dx) ,(- y dy) ,(- z dz)) 
	  (,(- x dx) ,(- y dy) ,(+ z dz))
	  )
      `((,(+ x dx) ,(+ y dy)) 
	(,(+ x dx) ,(- y dy)) 
	(,(- x dx) ,(+ y dy)) 
	(,(- x dx) ,(- y dy))))))

;;; See rotate-faces below

(defun rotate-vertices (hdg vertices)
  (loop for (x y z) in vertices
      with angle = (deg->rad hdg)
      collect
	`(,(xrot angle x y) ,(yrot angle y x) ,z)))

;; For 3D stuff only
;; +x is forward, +y is right, +z is up
(defun ref->faces (dims ref)
  (let ((x (first ref)) (y (second ref)) (z (third ref))
	(dx (/ (first dims) 2.0)) (dy (/ (second dims) 2.0)) 
	(dz (/ (third dims) 2.0)))
    (if z
	`(
	  ((,(- x dx) ,(- y dy) ,(+ z dz))   ;; top face:rear, upper left
	   (,(+ x dx) ,(- y dy) ,(+ z dz))   ;; top face:forward, upper left
	   (,(+ x dx) ,(+ y dy) ,(+ z dz))   ;; top face:forward, upper right
	   (,(- x dx) ,(+ y dy) ,(+ z dz)))  ;; top face:rear, upper right 
	  ((,(- x dx) ,(- y dy) ,(- z dz))   ;; bottom face:rear, lower left
	   (,(- x dx) ,(+ y dy) ,(- z dz))   ;; bottom face:rear, lower right
	   (,(+ x dx) ,(+ y dy) ,(- z dz))   ;; bottom face:forward, lower right
	   (,(+ x dx) ,(- y dy) ,(- z dz)))  ;; bottom face:forward, lower left 
	  ((,(+ x dx) ,(- y dy) ,(- z dz))   ;; front face:forward, lower left
	   (,(+ x dx) ,(+ y dy) ,(- z dz))   ;; front face:forward, lower right
	   (,(+ x dx) ,(+ y dy) ,(+ z dz))   ;; front face:forward, upper right
	   (,(+ x dx) ,(- y dy) ,(+ z dz)))  ;; front face:forward, upper left 
	  ((,(- x dx) ,(- y dy) ,(- z dz))   ;; back face:rear, lower left
	   (,(- x dx) ,(- y dy) ,(+ z dz))   ;; back face:rear, upper left
	   (,(- x dx) ,(+ y dy) ,(+ z dz))   ;; back face:rear, upper right
	   (,(- x dx) ,(+ y dy) ,(- z dz)))  ;; back face:rear, lower right
	  ((,(- x dx) ,(- y dy) ,(- z dz))   ;; left face:rear, lower left
	   (,(+ x dx) ,(- y dy) ,(- z dz))   ;; left face:forward, lower left
	   (,(+ x dx) ,(- y dy) ,(+ z dz))   ;; left face:forward, upper left
	   (,(- x dx) ,(- y dy) ,(+ z dz)))  ;; left face:rear, upper left
	  ((,(- x dx) ,(+ y dy) ,(- z dz))   ;; right face:rear, lower right
	   (,(- x dx) ,(+ y dy) ,(+ z dz))   ;; right face:rear, upper right
	   (,(+ x dx) ,(+ y dy) ,(+ z dz))   ;; right face:forward, upper right
	   (,(+ x dx) ,(+ y dy) ,(- z dz)))  ;; right face:forward, lower right
	  )

      )))

;;; 1/24/05 A heading will change the xy coords of the faces
;;; Formulae:
;;;  xnew = x cos(a) + y sin(a)
;;;  ynew = y cos(a) - x sin(a)
;;; where a is +radians when moving clockwise

(defun xrot (a x y) (+ (* x (cos a)) (* y (sin a))))
(defun yrot (a y x) (- (* y (cos a)) (* x (sin a))))

;; `(,(xrot (deg->rad 90.0) 1 1) ,(yrot (deg->rad 90.0) 1 1)) => (1.000796 -0.99920344)
;; `(,(xrot (deg->rad 45.0) 1 1) ,(yrot (deg->rad 45.0) 1 1)) => (1.4142134 5.630255e-4)

;; Use 0 if no heading is involved
;; `(,(xrot (deg->rad 0) 1 1) ,(yrot (deg->rad 0) 1 1)) => (1.0 1.0)

;; This will rotate the faces about the
;; origin.

(defun rotate-faces (hdg faces)
  (loop for face in faces
      with angle = (deg->rad hdg)
      collecting
	(loop for (x y z) in face
	    collect
	      `(,(xrot angle x y) ,(yrot angle y x) ,z))))

(defun ang-dist (point1 point2)	;each point a tuple (theta phi) in degrees
  (let* ((t1 (deg->rad (first point1))) (p1 (deg->rad (second point1)))
	 (t2 (deg->rad (first point2))) (p2 (deg->rad (second point2)))
	 (x1 (* (sin p1) (cos t1))) (x2 (* (sin p2) (cos t2)))
	 (y1 (* (sin p1) (sin t1))) (y2 (* (sin p2) (sin t2)))
	 (z1 (cos p1)) (z2 (cos p2)))
    (rad->deg (acos (+ (* x1 x2) (* y1 y2) (* z1 z2))))))

; returns the angular distance between 2 points from a given reference
; point, all points in xyz coordinates

;; We're gtting divide by zero errors here, probably because
;; we have real z's and not 0 z's.
;; ref is the reference point (pos agent), a is the agent's position,
;; b is the gaze.

(defun ang-dist2 (ref a b)
  (realpart
   (let ((x1 (first ref)) (y1 (second ref)) (z1 (third ref))
	 (x2 (first a)) (y2 (second a)) (z2 (third a))
	 (x3 (first b)) (y3 (second b)) (z3 (third b)))
     (let ((t1 (- x2 x1)) (t2 (- x3 x1)) (t3 (- y2 y1)) (t4 (- y3 y1))
	   (t5 (- z2 z1)) (t6 (- z3 z1)))
       (rad->deg (acos 
		  (if (equal 0.0 (* (sqrt (+ (expt t1 2) (expt t3 2) (expt t5 2)))
				    (sqrt (+ (expt t2 2) (expt t4 2) (expt t6 2)))))
		      0.0
		       (/ (+ (* t1 t2) (* t3 t4) (* t5 t6))
			  (* (sqrt (+ (expt t1 2) (expt t3 2) (expt t5 2)))
			     (sqrt (+ (expt t2 2) (expt t4 2) (expt t6 2))))))))))))

(defun rad->deg (r)
  (/ (* r 180) 3.14))

(defun deg->rad (d)	
  (/ (* d 3.14) 180))


;;; ------------------------------------------------------------------
;;; --- Physob relations database

;;; These functions are used to represent how simulated physical
;;; objects (physobs) relate to each other spatially and functionally.
;;; For example, it is useful to represent that one object is touching
;;; another or that one object controls whether another is on/off.

;;; The global variable *physob-db* is the "database" -- a list of
;;; propositions describing current state for the simulated world.
;;; Each proposition is a list whose first item is a relations
;;; predicate, and whose subsequent items are (typically) physobs.
;;; For example, (on egg-1 pan-1) is a proposition that could be used
;;; to represent that a particular egg (egg-1) is on top of a
;;; particular pan (pan-1).

(defvar *physob-db* nil)

(defun initialize-physob-database ()
  (setf *physob-db* nil))

;;; The functions assert-physob-relation and retract-physob-relation
;;; are used to add and delete propositions from the database.
;;; Find-physob-relation is used to confirm that a specific
;;; proposition is present in the database.

(defun assert-physob-relation (proposition)
  (log-event proposition)
  (push proposition *physob-db*)) ;; ! use setx?

(defun retract-physob-relation (proposition)
  (let ((match (find-physob-relation proposition)))
    (when match
      (setf *physob-db* (remove match *physob-db* :test #'equal))
      (log-event `(not ,proposition))))
  *physob-db*)

(defun find-physob-relation (proposition)
  (find-if #'(lambda (item) (pat-match proposition item)) *physob-db*))

;;; The function find-all-matches generates a list of propositions in
;;; the db that match a specified propositional form.  A form is just
;;; like a proposition except that it can contain a variable.
;;; Variables are symbols beginning with a question mark.  Thus, (on
;;; ?object pan-1) represents all propositions describing things that
;;; are on pan-1.  Assume that the following are in the database: (on
;;; egg-1 pan-1) (on egg-2 pan-1).  Calling find-all-matches with the
;;; form (on ?object pan-1) will yield the list (egg-1 egg-2).

(defun find-all-matches (form)
  (let ((matches nil))
    (dolist (item *physob-db*)
      (let ((bind (pat-match form item)))
	(if bind (push (binding-val (first bind)) matches))))
     matches))

;;(print "geom-end")

