;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Flight Geometry Library
;;; apex/apexlib/flightgeo-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: flightgeo-apexlib.lisp,v 1.2 2006/01/15 03:42:49 dalal Exp $

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; abstract point ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

(defclass point () ())


;;;;;;;;;;;;;;
;;          ;;
;; 2d point ;;
;;          ;;
;;;;;;;;;;;;;;

(defclass point2d (point)
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)))

;;
;; print method
;;

(defmethod print-object ((o point2d) s)
  (format s "#{PNT ~a,~a}" 
          (pfp (x o))
          (pfp (y o))))

;;
;; are two point2d are the same
;;

(defmethod same? ((p1 point2d) (p2 point2d))
  (and (~= (x p1) (x p2))
       (~= (y p1) (y p2))))

;;
;; create 2d point
;;

(defmethod create-point2d ((x number) (y number))
  (make-instance 'point2d :x (float x) :y (float y)))

;;
;; distance between two 2d points
;;

(defmethod distance ((p1 point2d) (p2 point2d))
    (sqrt (+ (square (- (x p1) (x p2)))
             (square (- (y p1) (y p2))))))

;;
;; sort list of points in order of distance to given point
;;

(defmethod sort-by-distance! ((point point2d) (points list))
  (sort points 
        (lambda (p1 p2) (< (distance point p1) (distance point p2)))))

;;
;; compute centroid of some points
;;

(defmethod centroid ((points list))
  (create-point2d
   (sdiv (loop for p in points sum (x p)) (length points))
   (sdiv (loop for p in points sum (y p)) (length points))))

;;;;;;;;;;;;;;
;;          ;;
;; 3d point ;;
;;          ;;
;;;;;;;;;;;;;;

(defclass point3d (point)
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)
   (z :accessor z :initarg :z :initform 0.0)))

;;
;; print method
;;

(defmethod print-object ((o point3d) s)
  (format s "#{PNT ~a,~a,~a}" 
          (pfp (x o))
          (pfp (y o))
          (pfp (z o))))

;;
;; are two point2d are the same
;;

(defmethod same? ((p1 point3d) (p2 point3d))
  (and (~= (x p1) (x p2))
       (~= (y p1) (y p2))
       (~= (z p1) (z p2))))
;;
;; create 3d point
;;

(defun create-point3d (x y z)
  (make-instance 'point3d :x x :y y :z z))

;;
;; distance between two 3d points
;;

(defmethod distance ((p1 point3d) (p2 point3d))
    (sqrt (+ (square (- (x p1) (x p2)))
             (square (- (y p1) (y p2)))
             (square (- (z p1) (z p2))))))

;;;;;;;;;;;;;;;;;;
;;              ;;
;; line segment ;;
;;              ;;
;;;;;;;;;;;;;;;;;;

(defclass segment ()
  ((p1 :accessor p1 :initarg :p1 :initform nil)
   (p2 :accessor p2 :initarg :p2 :initform nil)))

;;
;; print method
;;

(defmethod print-object ((o segment) s)
  (format s "#{SEG ~a ~a}" (p1 o) (p2 o)))

;;
;; are two segments the same
;;

(defmethod same? ((l1 segment) (l2 segment))
  (same? (list (p1 l1) (p2 l1)) (list (p1 l2) (p2 l2))))

;;
;; create a segment from two points
;;

(defmethod create-segment ((p1 point) (p2 point))
  (make-instance 'segment :p1 p1 :p2 p2))

;;
;; length of a segment (this should be called length GRRRR...)
;;

(defmethod seg-length ((s segment))
  (distance (p1 s) (p2 s)))

;;
;; test of two segments intersect
;;

(defmethod intersect? ((s1 segment) (s2 segment))
  
  ;; compute point slope from each segment
  
  (let ((line1 (segment-to-line s1))
        (line2 (segment-to-line s2)))
    
    ;; if segments lie on the same line, then they intersect if they overlap
    
    (cond ((same? line1 line2) (values (xoverlaps? s1 s2) nil))
     
          ;; if segments parallel, then they do not intersect
     
          ((parallel? line1 line2) nil)
     
          ;; compute point where the segments intersect if extended to lines
          
          (t (let ((p (intersect-at line1 line2)))
                   
               ;; if p is bounded by both segements
                   
               (if (and (xcontains? p s1)
                        (ycontains? p s1)
                        (xcontains? p s2)
                        (ycontains? p s2))
                   
                   ;; the segments intersect, return t and the point
                   
                   (values t p)))))))
;;
;; compute slope of segment
;;

(defmethod slope ((seg segment))
  (sdiv (- (y (p1 seg)) (y (p2 seg)))
        (- (x (p1 seg)) (x (p2 seg)))))
        
;;
;; compute y intercept, given a point and a slope
;;

;(defun intercept (p slope)
;  (- (y p) (* slope (x p))))

;;
;; checks if the x-value of a point lies in the x-value range of a segment
;;

(defmethod xcontains? ((p point2d) (seg segment))
  (between (x p) (list (x (p1 seg)) (x (p2 seg)))))

;;
;; checks if the y-value of a point lies in the y-value range of a segment
;;

(defmethod ycontains? ((p point2d) (seg segment))
  (between (y p) (list (y (p1 seg)) (y (p2 seg)))))

;;
;; checks if the x-axis components of two segments overlap
;;

(defmethod xoverlaps? ((s1 segment) (s2 segment))
  (or (xcontains? (p1 s1) s2)
      (xcontains? (p1 s2) s1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; line in point slope form ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass line ()
  ((slope     :accessor slope     :initarg :slope     :initform 0.0)
   (intercept :accessor intercept :initarg :intercept :initform 0.0)))
;;
;; print method
;;

(defmethod print-object ((o line) s)
  (format s "#{LINE s: ~a i: ~a}"
           (slope o)
           (intercept o)))
;          (pfp (slope o))
;          (pfp (intercept o))))

;;
;; are two lines the same
;;

(defmethod same? ((l1 line) (l2 line))
  (and (~= (slope     l1) (slope     l2))
       (~= (intercept l1) (intercept l2))))

;;
;; create line from a slope and y intercept
;;

(defmethod create-line ((slope number) (intercept number))
  (make-instance 'line
    :slope slope
    :intercept intercept))

;;
;; create line line from a point and an angle
;;

(defmethod create-line ((p point2d) (angle-rad number))
  (make-instance 'line
    :slope (tan angle-rad)
    :intercept (float (+ (* (- (tan angle-rad)) (x p)) (y p)))))

;;
;; create line which passes through p1 and p2
;;

(defmethod create-line ((p1 point2d) (p2 point2d))
  (let ((slope (sdiv (- (y p1) (y p2)) (- (x p1) (x p2)))))
    (make-instance 'line
      :slope (float slope)
      :intercept (float (- (y p1) (* slope (x p1)))))))
;;
;; create line which overlays line segment
;;

(defmethod segment-to-line ((s segment))
  (create-line (p1 s) (p2 s)))

;;
;; given x compute point on line where x is
;;

(defmethod find-y ((l line) (x number))
  (create-point2d x (+ (* (slope l) x) (intercept l))))


;;
;; given x compute point on line where x is
;;

(defmethod find-x ((l line) (y number))
  (create-point2d (sdiv (- (intercept l) y) (- (slope l)))  y))

;;
;; create line from p which is perpendicualr to an angle
;;

(defmethod create-perpendicualr ((p point2d) angle-rad)
  (let ((slope (sdiv -1 (tan angle-rad))))
    (make-instance 'line
      :slope (float slope)
      :intercept (float (+ (* (- slope) (x p)) (y p))))))
;;
;; create a line which passes through p and is perpendicular l
;;

(defmethod create-perpendicular ((l line) (p point2d))
  (create-perpendicualr p (atan (slope l))))

;;
;; distance between a line and a 2d point
;;

(defmethod distance ((l line) (p point2d))
  (distance p (intersect-at l (create-perpendicular l p))))

;;
;; find the intersection of two lines
;;

(defmethod intersect-at ((l1 line) (l2 line))
  
  ;; slope of l1 may not be 0 so swap them if it is
    
  (when (~= (slope l1) 0)
    (let ((tmp l1))
      (setf l1 l2)
      (setf l2 tmp)))
    
  ;; if lines are parallel return nil
  
  (if (~= (slope l1) (slope l2))
      nil
    
    ;; otherwise get the Ms and Bs
    
    (let* ((b1 (intercept l1))
           (m1 (slope     l1))
           (b2 (intercept l2))
           (m2 (slope     l2))
           
           ;; compute parts 1 & 2
           
           (part1 (- b2 (* (sdiv m2 m1) b1)))
           (part2 (sdiv (- m1 m2) m1))
           
           ;; compute y
           
           (y (sdiv part1 part2))
           
           ;; compute x
           
           (x (sdiv (- y  b1) m1)))
      
      ;; return 2d point
      
      (create-point2d x y))))

;;
;; establish if two points are on the same side of a line
;; point1 point2 line -> boolean
;;

(defmethod on-same-side ((p1 point2d) (p2 point2d) (l line))
  (let* ((slope (- (slope l)))
         (y-int (intercept l))
         (y-int1 (+ (* slope (x p1)) (y p1)))
         (y-int2 (+ (* slope (x p2)) (y p2))))
    (or 
     (and (< y-int y-int1) (< y-int y-int2))
     (and (> y-int y-int1) (> y-int y-int2)))))

;;
;; takes a point, an angle in radnians and a radius and computes
;; the radial point from the given point, heading and radius.
;; from that radial point compute the point slop form
;; for the line perpendicular to the heading
;; p heading radius -> line
;;

(defmethod compute-radial-tan ((p point2d) angle-rad radius)
  (create-perpendicualr 
   (circular-point p radius angle-rad)
   angle-rad))

;;
;; takes a point, a heading and a radius and an adjustement for x and y
;; and computes the radial point from the given point, heading and radius.
;; from that radial point compute the point slop form
;; for the line perpendicular to the heading
;; p adj heading radius -> line
;;

(defmethod compute-radial-tan-adjusted ((p point2d) (adj point2d)
                                        (angle-rad number) (radius number))
  (create-perpendicualr 
   (circular-point-adjusted p adj radius angle-rad)
   angle-rad))

;;
;; compute a point in the circle around a given point p,
;; an azimuth in radians, and a radius
;;

(defmethod circular-point ((p point2d) (radius number) (azimuth number))
  (create-point2d
   (+ (x p) (* radius (cos azimuth)))
   (+ (y p) (* radius (sin azimuth)))))

;;
;; compute a point in sphere around a given point p,
;; an azimuth & polar-angle in radians, and a radius
;;

(defmethod spherical-point ((p point3d) (radius number)
                            (azimuth number) (polar-angle number))
  (create-point3d
   (+ (x p) (* radius (sin polar-angle) (cos azimuth)))
   (+ (y p) (* radius (sin polar-angle) (sin azimuth)))
   (+ (z p) (* radius (cos polar-angle)))))

;;
;; compute a point on a radial from another point
;; with an adjustment in both x & y.
;; p adj heading radius -> p
;;

(defmethod circular-point-adjusted ((p point2d) (adj point2d) 
                                    (radius number) (azimuth number))
  (create-point2d
   (+ (x p) (* (sdiv radius (x adj)) (cos azimuth)))
   (+ (y p) (* (sdiv radius (y adj)) (sin azimuth)))))

;;
;; are two lines parallel
;;

(defmethod parallel? ((l1 line) (l2 line))
  (~= (slope l1) (slope l2)))

;;
;; are two lines perpendicular
;;

(defmethod perpendicular? ((l1 line) (l2 line))
  (~= (slope l1) (sdiv -1.0 (slope l2))))

;;
;; test of a segment and a line (in point slope form) intersect
;;

(defmethod intersect? ((s segment) (l line))
  
  ;; compute point slope from the segment
  
  (let ((line (segment-to-line s)))
    
    ;; if segments lie on the same line, then they intersect
    
    (cond ((same? line l) (values t nil))
     
          ;; if segments are parallel, then they do not intersect
     
          ((parallel? line l) nil)
     
          ;; compute point where the intersect if segment extended to a line
          
          (t (let ((p (intersect-at line l)))

               ;; if p is bounded by the segement
                   
               (if (and (xcontains? p s)
                        (ycontains? p s))
                   
                   ;; the line and segment intersect, return t and the point
                   
                   (values t p)))))))

;;
;; given a line a point and a distance return true if the point is 
;; within the specified distance
;;

(defmethod near? ((l line) (p point2d) (distance number))
  (<= (distance l p) distance))

;;
;; given a line some points and a distance return those points
;; wich exist within the specified distance
;;

(defmethod points-near ((l line) (points list) (distance number))
  (loop for p in points if (near? l p distance) collect p))

;;
;; NOT TESTED!
;; given an line and some points return two 
;; lists of points, one set on each side of line 
;;

(defmethod cleave ((l line) (points list) &optional bin1 bin2)
  
  ;; if no more points return the bins
  
  (cond ((not points) (values bin1 bin2))
        
        ;; if  bin1 is empty put the first point in it
        
        ((not bin1) (cleave l (cdr points) (car points) bin2))
        
        ;; otherwise place the current point in the correct bin
        ;; and recurse
        
        (t (if (on-same-side (car points) (car bin1) l)
               (cleave l (cdr points) (cons bin1 (car points)) bin2)
             (cleave l (cdr points) bin1 (cons bin2 (car points)))))))

;;
;; find nearest or farthest point to or from a given point or line
;;

(defmethod nearest  ((p point2d ) (points list)) (mostest p points #'<))
(defmethod nearest  ((l line) (points list)) (mostest l points #'<))
(defmethod farthest ((p point2d ) (points list)) (mostest p points #'>))
(defmethod farthest ((l line) (points list)) (mostest l points #'>))

;;
;; given a line or point2d and a list of points, find mostest
;; (nearest or farthest) point to the given line or point
;;

(defun mostest (item points operator &optional mostest-dist mostest-point)
  
  ;; if no more points to test return nearest found so far
  
  (cond ((not points) (values mostest-point mostest-dist))
        
        ;; get distance of first point
        
        (t (let ((distance (distance item (car points))))
             
             ;; if it's moster check the rest against it
             
             (if (or (not mostest-dist) (eval (list operator distance mostest-dist)))
                 (mostest item (cdr points) operator distance (car points))
               
               ;; otherwise keep the best we got so far, and look farther
               
               (mostest item (cdr points) operator mostest-dist mostest-point))))))

;;;;;;;;;;;;
;;        ;;
;; circle ;;
;;        ;;
;;;;;;;;;;;;

(defclass circle ()
  ((center :accessor center :initarg :center :initform (create-point2d 0 0))
   (radius :accessor radius :initarg :radius :initform 1)))
;;
;; print method
;;

(defmethod print-object ((o circle) s)
  (format s "#{CIRC at: ~a radius: ~a}"
          (center o)
          (pfp (radius o))))

;;
;; are two lines the same
;;

(defmethod same? ((c1 circle) (c2 circle))
  (and (same? (center c1) (center c2))
       (~=    (radius c1) (radius c2))))

;;
;; create a circle from a point and a radius
;;

(defmethod create-circle ((center point2d) (radius number))
  (make-instance 'circle
    :center center
    :radius radius))

;;
;; compute intersection points between a line segment and a circle
;;

(defmethod intersect-at ((s segment) (c circle))
  
  ;; loop through the intersection points between the line (produced
  ;; by extending the segment) and the cirlce, return those points
  ;; which fall inside the bounds of the segment
  
  (loop for p in (intersect-at (segment-to-line s) c) 
      if (and (xcontains? p s) (ycontains? p s))
      collect p))
;;
;; compute the intersection points of a line and a circle
;; http://mathworld.wolfram.com/Circle-LineIntersection.html
;;
          
(defmethod intersect-at ((l line) (c circle))
  
  ;; compute the elements of the equation
  
  (let* ((s (reverse-translate (create-segment 
                                (find-y l 0) (find-y l 1)) (center c)))
         (dx  (- (x (p2 s)) (x (p1 s))))
         (dy  (- (y (p2 s)) (y (p1 s))))
         (dr  (sqrt (+ (square dx) (square dy))))
         (d   (- (* (x (p1 s)) (y (p2 s))) (* (x (p2 s)) (y (p1 s)))))
         (dsc (sqrt (- (* (square (radius c)) (square dr)) (square d))))
         )
             
    ;; if the descrimintator < 0, there is no intersection

    (if (or (not (realp dsc)) (< dsc 0)) nil
      (let ((p1 (create-point2d
                 (sdiv (+ (*    d  dy) (* (sign dy) dx dsc)) (square dr))
                 (sdiv (+ (* (- d) dx) (* (abs  dy)    dsc)) (square dr))))

            (p2 (create-point2d
                 (sdiv (- (*    d  dy) (* (sign dy) dx dsc)) (square dr))
                 (sdiv (- (* (- d) dx) (* (abs  dy)    dsc)) (square dr)))))
                  
          ;; if the descrimintator = 1 the line is on the tangent of the circle

        (if (~= dsc 1) 
            (list (translate p1 (center c))))
    
          ;; othwise the line intesects the cirlce at two points
          
        (list (translate p1 (center c)) (translate p2 (center c)))))))

;;;;;;;;;;;;;
;;         ;;
;; polygon ;;
;;         ;;
;;;;;;;;;;;;;

(defclass polygon ()
  ((segments :accessor segments :initarg :segments :initform nil)))

;;
;; print method
;;

(defmethod print-object ((o polygon) s)
  (format s "#{PLY")
  (loop for seg in (segments o) 
      do (format s " ~a,~a"
                 (pfp (x (p1 seg))) 
                 (pfp (y (p1 seg)))))
  (format s "}"))

;;
;; are two polygons the same
;;

(defmethod same? ((pg1 polygon) (pg2 polygon))
  (same? (segments pg1) (segments pg2)))

;;
;; create polygon from list of points
;;

(defun create-polygon (points)
  (make-instance 'polygon 
    :segments (loop for p1 in points and p2 in (rotate points)
                  collect (create-segment p1 p2))))

;;
;; return list of points from a polygon
;;

(defmethod get-points ((pg polygon))
  (loop for seg in (segments pg) collect (p1 seg)))

;;
;; return intersect points of a line and a polygon
;;

(defmethod intersect-points ((pg polygon) (l line))
  (let* ((intersect nil)
         (point nil)    
         (points-and-segs
          
          ;; loop through the segments of the polygon
          
          (loop for s in (segments pg) 
                         
                         ;; get the intersect point
                         
              do (multiple-value-bind (i p) (intersect? s l)
                   (setf intersect i)
                   (setf point p))
                 
              if (and intersect point)
              collect (list point s))))
    (values (loop for p-s in points-and-segs collect (nth 0 p-s))
            (loop for p-s in points-and-segs collect (nth 1 p-s)))))
        
;;
;; compute centroid of polygon
;;

(defmethod centroid ((pg polygon))
  (centroid (get-points pg)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; transform functions ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; a myriad of translation functions
;;

(defmethod translate ((p point2d) (trans point2d))
  (create-point2d (+ (x p) (x trans)) 
                  (+ (y p) (y trans))))

(defmethod translate ((p point3d) (trans point3d))
  (create-point3d (+ (x p) (x trans)) 
                  (+ (y p) (y trans))
                  (+ (x p) (x trans))))
            
(defmethod translate ((s segment) (trans point2d))
  (create-segment (translate (p1 s) trans)
                  (translate (p2 s) trans)))


(defmethod reverse-translate ((p point2d) (trans point2d))
  (create-point2d (- (x p) (x trans)) 
                  (- (y p) (y trans))))

(defmethod reverse-translate ((p point3d) (trans point3d))
  (create-point3d (- (x p) (x trans)) 
                  (- (y p) (y trans))
                  (- (x p) (x trans))))
            
(defmethod reverse-translate ((s segment) (trans point2d))
  (create-segment (reverse-translate (p1 s) trans)
                  (reverse-translate (p2 s) trans)))


;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;; utility functions ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;


;;
;; are two lists of objects which can be tested for sameness the same
;;

(defmethod same? ((l1 list) (l2 list))
  (cond 
   ((and (not l1) (not l2)) t)
   ((or (not l1) (not l2)) nil)
   (t (let ((item (find-same (car l1) l2)))
        (if item 
            (same? (cdr l1) (remove item l2))
          nil)))))

;;
;; find an item which is the same as one in the list
;;

(defmethod find-same (item (l list))
  (cond 
   ((not l) nil)
   ((same? item (car l)) (car l))
   (t (find-same item (cdr l)))))

;;
;; test that two numbers are very nearly equal (to 5 decimal places)
;;


(defmethod ~= ((a number) (b number))
  (= (round (* a 100000)) (round (* b 100000))))

;;
;; safely divide n1 by n2
;;

(defun sdiv (n1 n2) 
  (/ n1 (if (= n2 0) double-float-epsilon n2)))

;;
;; square a number
;;

(defun square (x)
  (* x x))

;;
;; raise x to the y
;;

(defun power (x y)
  (cond ((<= y 0) 1)
        (t (* x (power x (- y 1))))))

;;
;; round x to the nearest n decimal places
;;

(defun round-to (x n)
  (let ((factor (power 10 n)))
    (sdiv (round (* x factor)) factor)))

;;
;; converters for heading, degrees and radians
;;

(defun rad-to-deg   (radians) (/ (* radians 180) pi))
(defun rad-to-head  (radians) (delta-heading (- 360 (rad-to-deg radians)) 90))
(defun deg-to-rad   (degrees) (* (sdiv degrees 180) pi))
(defun head-to-deg  (heading) (delta-heading (- 360 heading) 90))
(defun head-to-rad  (heading) (deg-to-rad (head-to-deg heading)))

;;
;; add a delta to a heading and still get a 0 - 359 value
;;

(defun delta-heading (heading delta) (delta-degrees heading delta))
(defun delta-degrees (degrees delta) (mod (+ degrees (+ delta 360)) 360))

;;
;; given a start point and a end point return flight heading
;;

(defmethod compute-heading ((from point) (to point))
  (let ((dx (- (x from) (x to)))
        (dy (- (y from) (y to))))
    (delta-heading (rad-to-head (atan (sdiv dy dx)))
                   (if (>= dx 0) 180 0))))

;;
;; return the rotation of a list
;;

(defun rotate (items)
  (append (cdr items) (list (car items))))

;;
;; determines if a value lies in the interval between two other values
;;

(defun between (val val-pair)
  (let* ((ordered-pair (sort val-pair #'<))
         (rval (round (* 10000 val)))
         (rlow (round (* 10000 (first ordered-pair))))
         (rhgh (round (* 10000 (second ordered-pair)))))
    (and (>= rval rlow) (<= rval rhgh))))

;;
;; prepair a number for printing
;;

(defmethod pfp ((x number))
  (let* ((fv (float (round-to x 2)))
         (iv (round fv)))
    (if (= fv iv) iv fv)))
;;
;; do two numbers have the same sine?
;;

(defmethod same-sign? ((x1 number) (x2 number))
  (or (and (<  x1 0) (<  x2 0))
      (and (>= x1 0) (>= x2 0))))
;;
;; compute sign of x, x < 0 -> -1, x >= 0 -> 1
;;

(defun sign (x)
  (if (< x 0) -1 1))

;;
;; interpolate between two points in 3d space
;;

(defmethod interpolate ((p1 point3d) (p2 point3d) (steps number))
  (let ((xs (sdiv (- (x p2) (x p1)) steps))
        (ys (sdiv (- (y p2) (y p1)) steps))
        (zs (sdiv (- (z p2) (z p1)) steps)))
    (loop for i from 1 to (1- steps)
        collect (create-point3d 
                 (+ (x p1) (* xs i))
                 (+ (y p1) (* ys i))
                 (+ (z p1) (* zs i))))))


;;
;; given a list of pairs of points ensure that they meander
;;

(defun meander-path (path &optional head)
  
  ;; if no more path elements, all doen
  
  (cond ((not path) (list head))
        
        ;; if no head, tare it off and recurse
         
        ((not head) (meander-path (cdr path) (car path)))
        
        ;; recurse ordering the members of the head to cause the meander
        
        (t (let ((order-pair (antipolorize-pairs head (car path))))
             (cons head (meander-path (cdr path) order-pair))))))
;;
;; given two pairs of points (the two lines form by the point pairs are assumed
;; to be parallel) order the second pair of points so that they are in the
;; same direction as the first pair
;;
  
(defmethod polorize-pairs ((pair1 list) (pair2 list))
  (let* ((delta1-x (- (x (nth 0 pair1)) (x (nth 1 pair1))))
         (delta1-y (- (y (nth 0 pair1)) (y (nth 1 pair1))))
         (delta2-x (- (x (nth 0 pair2)) (x (nth 1 pair2))))
         (delta2-y (- (y (nth 0 pair2)) (y (nth 1 pair2))))
         (use-x    (> (abs delta1-x) (abs delta1-y)))
         (delta1   (if use-x delta1-x delta1-y))
         (delta2   (if use-x delta2-x delta2-y)))
    (if (same-sign? delta1 delta2)
         pair2
      (reverse pair2))))
;;
;; given two pairs of points (the two lines form by the point pairs are assumed
;; to be parallel) order the second pair of points so that they are in the
;; oposite direction as the first pair
;;
  
(defmethod antipolorize-pairs ((pair1 list) (pair2 list))
  (reverse (polorize-pairs pair1 pair2)))

;;;;;;;;;;;;;;;
;;           ;;
;; test code ;;
;;           ;;
;;;;;;;;;;;;;;;

;;
;; run text basted tests
;;

(defun test-all ()
  (test-sameness)
  (test-segint)
  (test-delta-heading)
  (test-intersection)
  (test-poly)
  (test-sorter)
  (test-mostest))

;;
;; test all graphit tests
;;

(defun test-all-graph ()
  (test-sweep)
  (test-upandover)
  (test-cirlce-intersect)
  (test-sphere))

;;
;; test nearest and farthest
;;

(defun test-mostest ()
  (let* ((p1 (create-point2d  1 1))
         (p2 (create-point2d  2 2))
         (p3 (create-point2d  3 3))
         (p4 (create-point2d  4 4))
         (p5 (create-point2d  5 5))
         (l1 (create-line p1 (* pi .75)))
         (l2 (create-line p5 (* pi .75)))
         (points (list p2 p3 p4)))
    
    (test-result points 'nearest-to    p1 'is (nearest  p1  points) p2)
    (test-result points 'nearest-to    p5 'is (nearest  p5  points) p4)
    (test-result points 'farthest-from p1 'is (farthest p1  points) p4)
    (test-result points 'farthest-from p5 'is (farthest p5  points) p2)
    (test-result points 'nearest-to    l1 'is (nearest  l1  points) p2)
    (test-result points 'nearest-to    l2 'is (nearest  l2  points) p4)
    (test-result points 'farthest-from l1 'is (farthest l1  points) p4)
    (test-result points 'farthest-from l2 'is (farthest l2  points) p2)
    ))

;;
;; print test result
;;

(defun test-result (item1 relationship1 item2 relationship2 result exspected)
  (format t "~a: ~a ~a ~a ~a ~a~%"
          (if (same? result exspected) "good" "BAD!")
          item1
          relationship1
          item2
          relationship2
          result))

;;  
;; test distance sorting
;;

(defun test-sorter ()
  (let* ((p1 (create-point2d  -1 -1))
         (p2 (create-point2d  2 2))
         (p3 (create-point2d  3 3))
         (p4 (create-point2d  4 4))
         (p5 (create-point2d  5 5))
         (points (list p3 p4 p5 p2))
         (result (sort-by-distance! p1 points))
         (expected (list p2 p3 p4 p5)))
    (format t "~a: ~a == ~a~%"
            (if (equal expected result) "good" "BAD!")
                       expected
                       result)))
;;
;; test line intersection code
;;

(defun test-intersection ()
  (let* ((p1  (create-point2d  0 1))
         (p2  (create-point2d  4 2))
         (p3  (create-point2d  3 4))
         (p4  (create-point2d -1 3))
         (p5  (create-point2d  200 200))
         (p6  (create-point2d  400 200))
         (p7  (create-point2d  200   0))
         (l1  (create-line p1 p2))
         (l2  (create-line p2 p3))
         (l3  (create-line p3 p4))
         (l4  (create-line p5 p6))
         (l5  (create-line p6 p7)))
      
    (test-result l1 'and l2 'intersect-at (intersect-at l1 l2) p2)
    (test-result l2 'and l3 'intersect-at (intersect-at l2 l3) p3)
    (test-result l3 'and l1 'intersect-at (intersect-at l3 l1) nil)
    (test-result l4 'and l5 'intersect-at (intersect-at l4 l5) p6)))
      
;;
;; test sameness of things
;; 


(defun test-sameness ()
  (labels ((run-same-test (a b expected)
             (let* ((test (same? a b)))
               (format t "~a: ~a == ~a: ~a~%"
                       (if (eq expected test) "good" "BAD!")
                       a
                       b 
                       (if test "yes" "no ")))))
    (let* ((p1  (create-point2d  3   1  ))
           (p2  (create-point2d  1   2  ))
           (p3  (create-point2d  2   3  ))
           (p4  (create-point2d  5   5  ))
           (p5  (create-point2d  3.0 1.0))
           (pg1 (create-polygon (list p1 p2 p3 p4 p5)))
           (pg2 (create-polygon (list p2 p3 p4 p5 p1)))
           (pg3 (create-polygon (list p3 p2 p4 p5 p1)))
           (l1  (create-line  1  1))
           (l2  (create-line p2 p3))
           (l3  (create-line p2 (deg-to-rad 45)))
           (l4  (create-line p3 p2))
           (s1  (create-segment p1 p2))
           (s2  (create-segment p2 p1))
           (s3  (create-segment p1 p3))
           )
      (run-same-test p1  p2  nil)
      (run-same-test p1  p5    t)
      (run-same-test pg1 pg2   t)
      (run-same-test pg1 pg3 nil)
      (run-same-test (get-points pg1) (list p1 p2 p3 p4 p5) t)
      (run-same-test l1  l2    t)
      (run-same-test l1  l3    t)
      (run-same-test l1  l4    t)
      (run-same-test s1  s2    t)
      (run-same-test s2  s1    t)
      (run-same-test s2  s3  nil)
      (run-same-test s3  s1  nil)
      
      (run-same-test (list p3 p4 p5) (list p5 p3 p4)   t)
      (run-same-test (list p4    p5) (list p5 p3 p4) nil)
      (run-same-test (list p3 p4 p5) (list p5 p3   ) nil)
      )))
           
;;
;; test delta heading code
;;

(defun test-delta-heading ()
  (labels ((run-dh-test (heading delta expected)
             (let* ((test (delta-heading heading delta)))
               (format t "~a: ~a + ~a = ~a~%"
                       (if (= expected test) "good" "BAD!")
                       heading 
                       delta
                       test))))
    (run-dh-test  10 -11 359)
    (run-dh-test  10 360  10)
    (run-dh-test 359   1   0)
    (run-dh-test 360 360   0)))

;;
;; test polygon and line intersection
;;

(defun test-poly ()
  (labels ((run-pg-test (l pg expected)
             (let* ((test (intersect-points pg l)))
               (format t "~a: " (if (same? expected test) "good" "BAD!"))
               (format t "~a intersects ~a at ~a~%" l pg test))))
               
    (let* ((p1 (create-point2d  3 1))
           (p2 (create-point2d  5 7))
           (p3 (create-point2d 11 3))
           (p4 (create-point2d  5 4))
           (p5 (create-point2d  4 4))
           (p6 (create-point2d  8 5))
           (pg (create-polygon (list p1 p2 p3 p4)))
           (l1 (create-line (create-point2d  0 3)
                           (create-point2d 12 6)))
           (l2 (create-line p1 p2)))

      (run-pg-test l1 pg (list p5 p6))
      (run-pg-test l2 pg (list p2 p1)))))

;;
;; test segment intersections
;;

(defun test-segint ()
  (labels ((run-si-test (s1 s2 expected)
             (let* ((s3 (create-segment (p2 s1) (p1 s1)))
                    (s4 (if (eq (type-of s2) 'segment)
                            (create-segment (p2 s2) (p1 s2))
                            nil))
                    (test1 (intersect? s1 s2))
                    (test2 (intersect? s3 s2))
                    (test3 (if s4 (intersect? s3 s4) nil)))
               (format t "~a: intersect ~a and ~a = ~a~%"
                       (if (eq expected test1) "good" "BAD!")
                       s1
                       s2
                       (if test1 "yes" "no "))
               (format t "~a: intersect ~a and ~a = ~a~%"
                       (if (eq expected test2) "good" "BAD!")
                       s3
                       s2
                       (if test2 "yes" "no "))
               (when s4
                 (format t "~a: intersect ~a and ~a = ~a~%"
                         (if (eq expected test3) "good" "BAD!")
                         s3
                         s4
                         (if test3 "yes" "no ")))
               )))
    
    ;; a load of points
    
    (let* ((p1  (create-point2d 1 1))
           (p2  (create-point2d 5 5))
           (p3  (create-point2d 2 4))
           (p4  (create-point2d 5 1))
           (p5  (create-point2d 5 9))
           (p6  (create-point2d 8 3))
           (p7  (create-point2d 4 4))
           (p8  (create-point2d 6 6))
           (p9  (create-point2d 9 9))
           (p10 (create-point2d 1 2))
           (p11 (create-point2d 5 6))
           
           ;; a load of segments
           
           (s1 (create-segment p1  p2 ))
           (s2 (create-segment p3  p4 ))
           (s3 (create-segment p3  p5 ))
           (s4 (create-segment p2  p6 ))
           (s5 (create-segment p7  p8 ))
           (s6 (create-segment p2  p8 ))
           (s7 (create-segment p8  p9 ))
           (s8 (create-segment p10 p11))
           
           ;; a load of lines
           
           (l1 (create-line p3  p4 ))
           (l2 (create-line p3  p5 ))
           (l3 (create-line p2  p6 ))
           (l4 (create-line p7  p8 ))
           (l5 (create-line p2  p8 ))
           (l6 (create-line p8  p9 ))
           (l7 (create-line p10 p11)))
      
      ;; test segments vs segments
      
      (run-si-test s1 s2 t  ) ;; basic intersect
      (run-si-test s1 s3 nil) ;; basic non-intersect
      (run-si-test s1 s4 t  ) ;; point intersect
      (run-si-test s1 s5 t  ) ;; segment overlap
      (run-si-test s1 s6 t  ) ;; point overlap
      (run-si-test s1 s7 nil) ;; on line but no overlap
      (run-si-test s1 s8 nil) ;; parallel

      ;; test segments vs lines
      
      (run-si-test s1 l1 t  ) ;; basic intersect
      (run-si-test s1 l2 nil) ;; basic non-intersect
      (run-si-test s1 l3 t  ) ;; point intersect
      (run-si-test s1 l4 t  ) ;; segment overlap
      (run-si-test s1 l5 t  ) ;; point overlap
      (run-si-test s1 l6 t  ) ;; on line but no overlap
      (run-si-test s1 l7 nil) ;; parallel
      )))


;;
;; test sweep flight maneuver, produces a graphit 
;;

(defun test-sweep ()
  (let* ((home (create-point2d  370 40))
         (p1-1 (create-point2d  100 100))
         (p1-2 (create-point2d  300 150))
         (p1-3 (create-point2d  100 200))
         (p2 (create-point2d  100 300))
         (p3 (create-point2d  300 300))
         (p4 (create-point2d  150 250))
         (p5 (create-point2d  450 150))
         (p6 (create-point2d  300 100))
         (pg (create-polygon (list p1-1 p1-2 p1-3 p2 p3 p4 p5 p6)))
         (heading 60)
         (width 50))
    
    (graphit-open "/tmp/sweep.txt")
    (graphit-config-image 400 400 5)
    (graphit "draw")
    ;(graphit "color 0 0 0 64")
    ;(graphit "grid 100 100 0 0 400 400")
    ;(graphit "color 0 0 0 32")
    ;(graphit "grid 20 20 0 0 400 400")
    (graphit "translate -70 20")
    (graphit "stroke 2 round round")
    (graphit "color 64 0 255 40")
    (graphit "color 0 0 255 96")
    (graphit "fill")
    (graphit-polygon pg)
    (graphit "draw")
    (graphit-polygon pg)
    (graphit "color 255 128 32 96")
    (graphit "draw")
    (let ((points (sweep-poly pg home heading width)))
      (graphit "stroke ~a but bevel" (+ width 2))
      (graphit "color 255 128 16 40")
      (loop for p1 in points by #'cddr for p2 in (cdr points) by #'cddr 
          do (graphit-line (list p1 p2)))
      (graphit "color 128 128 128 96")
      (graphit "stroke 10 round round")
      (graphit-line (cons home points)) 
      (graphit "fill")
      (graphit "color 255 0 0 128")
      (graphit "color 0 255 0 96")
      (graphit "circle ~a ~a 10" (x home) (y home))
      (graphit "color 0 0 0 204")
      (graphit "font-size 18")
      (graphit "font \"Courier Bold\"")
      (graphit "write start ~a ~a center top" (x home) (- (y home) 20))
      (graphit-close)
      (graphit-run "/tmp/sweep.txt" "-o sweep.png")
      )))


  
(defun test-upandover ()
  (let* ((from (create-point3d   350 280 10))
         (targ (create-point3d 200 280  0))
         (points (up-and-over from targ 100 10))
         (line (interpolate from (car points) 10)))
    
    (graphit-open "/tmp/upandover.txt")
    (graphit-config-image 400 400 5)
    (graphit "draw")
    (graphit "color 0 0 0 64")
    (graphit "grid 100 100 0 0 400 400")
    (graphit "color 0 0 0 32")
    (graphit "grid 20 20 0 0 400 400")
    (graphit "fill")
    (graphit "color 0 255 0 96")
    (graphit-point3d from 10)
    (graphit "color 0 0 255 96")
    (graphit-point3d targ 10)
    (graphit "color 255 0 0 64")
    (loop for p in line   do (graphit-point3d p 5))
    (loop for p in points do (graphit-point3d p 5))
    (graphit-close)
    (graphit-run "/tmp/upandover.txt" "")
    ))



;;
;; test sperical coordinate system
;; produces a graphit image that is spherelike
;; 

(defun test-sphere ()
  (let* ((p1 (create-point3d 200 280 0)))
    (graphit-open "/tmp/sphere.txt")
    (graphit-config-image 400 400 5)
    (graphit "draw")
    (graphit "color 0 0 0 64")
    (graphit "grid 100 100 0 0 400 400")
    (graphit "color 0 0 0 32")
    (graphit "grid 20 20 0 0 400 400")
    (graphit "color 255 0 0 64")
    (graphit "fill")
    (loop for h from -30 to 145 by 5 
        do (loop for pc from 0 to 180 by 7
               do (let ((p (spherical-point p1 190 (head-to-rad h) (deg-to-rad pc))))
                    (if (< (z p) (z p1))
                        (graphit "color 0 0 255 16")
                      (graphit "color 255 0 0 64"))
                    (graphit-point3d p (pfp (* 8.5 (sin (deg-to-rad pc))))))))

    (graphit-close)
    (graphit-run "/tmp/sphere.txt" "")
    ))

;;
;; test cirlce line and segment intersections
;;

(defun test-cirlce-intersect ()
  (let* ((c (create-circle (create-point2d 100 100) 300)))
    (graphit-open "/tmp/circle.txt")
    (graphit-config-image 800 800 5)
    (graphit "color 0 0 0 64")
    (graphit "draw")
    (graphit "grid 100 100 0 0 size-x size-y")
    (graphit "color 0 0 0 32")
    (graphit "grid 20 20 0 0 size-x size-y")
    (graphit "translate 300 300")
    (graphit "color 128 64 16 128")
    (graphit "stroke 5 round round")
    (graphit-circle c)
    (loop for angle from 0 to 359 by 45
        do (loop for range from 0 to 320 by 80
               do (let* ((l (compute-radial-tan (center c) (deg-to-rad angle) range))
                         (lo (- (x (center c)) (radius c) 50))
                         (hi (+ (x (center c)) (radius c) 50))
                         (s (if (< (abs (slope l)) 1)
                                (create-segment  (find-y l lo) (find-y l hi))
                              (create-segment  (find-x l lo) (find-x l hi))))
                         (i (intersect-at l c))
                         )
                    (graphit "color 0 0 128 128")
                    (graphit "stroke 2 round round")
                    (graphit-segment s)
                    (graphit "stroke 2 round round")
                    (loop for p in i do (graphit-point2d p 10))
                    )))
    (loop for angle from 0 to 359 by 10 for range from 295 by 2
        do (let* ((s (create-segment (center c) 
                                     (circular-point 
                                      (center c)
                                      (+ 300 (* 20 (sin angle)))
                                      (deg-to-rad (+ angle 10)))))
                  (i (intersect-at s c)))
             (graphit "color 0 128 0 128")
             (graphit "stroke 2 round round")
             (graphit-segment s)
             (graphit "stroke 2 round round")
             (loop for p in i
                 do (graphit-point2d p 10))
             ))
    (graphit-close)
    (graphit-run "/tmp/circle.txt" "-o cirlce.png")
    ))
         

;;
;; test 3d graphit capabilities
;;

(defun test-3d ()
  (let* ((p1  (create-point3d  50  50  50))
         (p2  (create-point3d  50 100  50))
         (p3  (create-point3d 100 100  50))
         (p4  (create-point3d 100  50  50))
         (p5  (create-point3d  50  50 100))
         (p6  (create-point3d  50 100 100))
         (p7  (create-point3d 100 100 100))
         (p8  (create-point3d 100  50 100))
         (p9  (create-point3d  50  50  25))
         (p10 (create-point3d  50 100  25))
         (p11 (create-point3d 100 100  25))
         (p12 (create-point3d 100  50  25)))
    (graphit-open "/tmp/3d.txt")
    (graphit-config-image 400 400 5)
    (graphit "draw")
    (graphit "grid 100 100 0 0 400 400")
    (graphit "color 0 0 0 32")
    (graphit "grid 20 20 0 0 400 400")
    (graphit "color 255 0 0 64")
    (graphit "fill")
    (graphit "color 255 0 0 64")
    (graphit-point3d p1 10)
    (graphit "color 255 255 0 64")
    (graphit-point3d p2 10)
    (graphit "color 0 255 0 64")
    (graphit-point3d p3 10)
    (graphit "color 0 255 255 64")
    (graphit-point3d p4 10)
    (graphit "color 255 0 0 64")
    (graphit-point3d p5 10)
    (graphit "color 255 255 0 64")
    (graphit-point3d p6 10)
    (graphit "color 0 255 0 64")
    (graphit-point3d p7 10)
    (graphit "color 0 255 255 64")
    (graphit-point3d p8 10)
    (graphit "color 255 0 0 64")
    (graphit-point3d p9 10)
    (graphit "color 255 255 0 64")
    (graphit-point3d p10 10)
    (graphit "color 0 255 0 64")
    (graphit-point3d p11 10)
    (graphit "color 0 255 255 64")
    (graphit-point3d p12 10)
    (graphit-close)
    (graphit-run "/tmp/3d.txt" "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; graphit output funtions ;;
;;                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; graphit stream
;;

(defvar graphit-stream nil)

;;
;; open graphit textfile
;;

(defun graphit-open (file-name)
  (setf graphit-stream
    (open file-name :direction :output :if-exists :rename-and-delete)))

;;
;; append text to open graphit file (works like format)
;;

(defmethod graphit (string &rest args)
  (eval (append (list 'format graphit-stream string) args))
  (format graphit-stream "~%"))

;;
;; append text to open graphit file without a new line (works like format)
;;

(defmethod graphit-part (string &rest args)
  (eval (append (list 'format graphit-stream string) args)))

;;
;; close graphit text file
;;

(defun graphit-close () (close graphit-stream))

;;
;; run graphit program
;;

(defun graphit-run (file-name parameters)
  (run-shell-command 
   (format nil "GraphIt -i ~a ~a" file-name parameters)
   :wait nil))

;;
;; graph a polygon
;;

(defun graphit-polygon (pg)
  (graphit-part "polygon")
  (loop for p in (get-points pg)
      do (graphit-part " ~a ~a" (pfp (x p)) (pfp (y p))))
  (graphit-part "~%"))

;;
;; graph segment
;;

(defun graphit-segment (s)
  (graphit "line ~a ~a ~a ~a"
           (pfp (x (p1 s)))
           (pfp (y (p1 s)))
           (pfp (x (p2 s)))
           (pfp (y (p2 s)))))

;;
;; graph circle
;;

(defun graphit-circle (c)
  (graphit "circle ~a ~a ~a ~%" 
           (pfp (x (center c)))
           (pfp (y (center c)))
           (pfp (radius c))))

;;
;; output a line as a set of points
;;

(defun graphit-line (points)
  (graphit-part "line")
  (loop for p in points
      do (graphit-part " ~a ~a" (pfp (x p)) (pfp (y p))))
  (graphit-part "~%"))

;;
;; 3d point
;;

(defmethod graphit-point3d ((p point3d) (radius number))
  (let ((x (x p))
        (y (z p))) ;;(/ (+ (y p) (z p)) 1.41)))
    (graphit "circle ~a ~a ~a" (pfp x) (pfp y) radius)))
  
;;  (let* ((zoom 25)
;;         (center-x 200)
;;         (center-y 200)
;;         (p2d (project2d 
;;               p
;;               (create-point3d center-x 0 center-y)
;;               (create-point3d 0 0 0)
;;               (create-point2d center-x center-y)
;;               zoom)))
;;    (graphit-point2d p2d radius)))

;;
;; project a 3d point onto a 2d screen
;;

(defmethod project2d ((p point3d) (cam point3d) (pan point3d) 
                      (center point2d) (zoom number))
  
  (let* ((x1 (+ (x p) (x cam)))
         (y1 (+ (y p) (y cam)))
         (z1 (+ (z p) (z cam)))
         
         (x2 (- (* x1 (cos (x pan))) (* z1 (sin (x pan)))))
         (z2 (+ (* x1 (sin (x pan))) (* z1 (cos (x pan)))))
         (y2 (- (* y1 (cos (y pan))) (* z2 (sin (y pan)))))

         (z3 (- (* y2 (cos (y pan))) (* z2 (sin (y pan)))))
         (x3 (- (* x2 (cos (z pan))) (* y2 (sin (z pan)))))
         (y3 (+ (* x2 (sin (z pan))) (* y2 (cos (z pan))))))
         
         
    (if (= z3 0) nil
      (create-point2d (+ (* (/ x3 z3) zoom) (x center))
                      (+ (* (/ y3 z3) zoom) (y center))))))

;;
;; 2d point
;;

(defmethod graphit-point2d ((p point) (radius number))
  (graphit "circle ~a ~a ~a" 
           (pfp (x p))
           (pfp (y p))
           radius))

;;
;; confgire graphit image size a border
;;

(defun graphit-config-image (xsize ysize border)
  (let ((xscale (/ (- xsize (* 2 border)) xsize))
        (yscale (/ (- ysize (* 2 border)) ysize)))
  (graphit "size ~a ~a" xsize ysize)
  (graphit "translate ~a ~a" border border)
  (graphit "scale ~a ~a" (float xscale) (float yscale)))
  (graphit "= size-x ~a" xsize)
  (graphit "= size-y ~a" ysize)
  (graphit "= center-x ~a" (/ xsize 2))
  (graphit "= center-y ~a" (/ ysize 2)))

         
;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; flight maneuvers ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

;; number of degrees to step during generation of arcs around things

(defvar up-and-over-steps 7)

;;
;; fly up and over a target
;;

(defmethod up-and-over ((from point3d) (target point3d) 
                        (standoff number) (rise number))

  (format t "mark 1~%")

  ;; compute heading to target
  
  (let* ((heading (compute-heading from target))
         
         ;; compute the line which is <rise> above the target
         
         (rise-line (create-line (create-point2d 0 rise) 0))
         
         ;; compute the intersect points between the cirlce projected sidways
         ;; and the rise line
         
         (pts (intersect-at 
               rise-line (create-circle (create-point2d 0 0) standoff)))
         
         ;; if there is an itersect point, compute the angle to the intersect point
         
         (rise-angle (if pts 
                         (- (/ pi 2) (atan (sdiv rise (x (car pts)))))
                       nil)))
    
    (format t "~a~%" pts)
    
    ;; if we have a rise angle,
    
    (if rise-angle
        
        ;;loop from intersect-point to intersect point over the target
        
        (loop for angle from (- rise-angle) ;;to rise-angle
            by (sdiv rise-angle (sdiv up-and-over-steps 2))
            while (<= angle rise-angle)
            collect (spherical-point target standoff 
                                     (head-to-rad heading) 
                                     angle))
    
    ;; otherwise return the point <rise> distance above the target
    
    (list (create-point3d (x target) (y target) (+ (z target) rise))))))
    

;;
;; given a polygon a staring point a heading and a swath width
;; compute a sweeping pattery to overfly the polygon
;;

(defmethod sweep-poly ((pg polygon) (from point2d) (heading number) (width number))  
  
  ;; establish the centroid of the polygon
  
  (let* ((centroid (centroid pg))
         
         ;; compute angle perpendicular to heading (in radians)
         
         (perp-angle (head-to-rad (delta-heading heading 90)))
         
         ;; half the swath width
         
         (width/2 (/ width 2))
         
         ;; find the distance of the farthest point from the centroid
         
         (range (- (loop for p in (get-points pg)
                    maximize (distance p centroid)) width/2))
         
         ;; collect all the swaths (left-edge center-line right-edge)
         
         (swaths 
          (loop for r downfrom range to (- (+ range width/2)) by width
              collect
                (list 
                 (compute-radial-tan centroid perp-angle (- r width/2))
                 (compute-radial-tan centroid perp-angle r)
                 (compute-radial-tan centroid perp-angle (+ r width/2)))))
         
         ;; filter out the swaths which don't overfly the polygon
         ;; and return the correct pairs of points to fly
         
         (raw-path (loop for swath in swaths with (points)
                       do (setf points (swath-points pg swath width/2))
                       if (and points 
                               (not (same? (car points) (car (last points)))))
                       collect points)
         
                   ;;if (and points 
                   ;;        (not (same? (car points) (car (last points)))))
                   ;;do (let* ((p1 (intersect-at 
                   ;;               (nth 0 swath) 
                   ;;               (create-line (car points) perp-angle)))
                   ;;          (p2 (intersect-at
                   ;;               (nth 2 swath) 
                   ;;               (create-line (cadr points) perp-angle)))
                   ;;          (p3 (intersect-at
                   ;;               (nth 0 swath) 
                   ;;               (create-line (cadr points) perp-angle)))
                   ;;          (p4 (intersect-at
                   ;;               (nth 2 swath) 
                   ;;               (create-line (car points) perp-angle))))
                   ;;     (graphit-polygon (create-polygon (list p1 p3 p2 p4))))
                   )

         
         ;; find the four possible starting points for this sweep
         
         (starting-points (append (car raw-path) (car (last raw-path))))


         ;; find the closest of the four possible starting points

         (starting-point (nearest from starting-points))
         
         ;; order the path from the staring point
         
         (forward-path (if (member starting-point (car raw-path))
                           raw-path
                         (reverse raw-path)))
         
         ;; enforce a meandering pattern upon the path from staring point
         
         (meander-path (meander-path 
                        (if (eq starting-point (caar forward-path))
                            forward-path
                          (cons (reverse (car forward-path))
                                (cdr forward-path)))))
         
         ;; convert path from point pairs to a flat list of points
        
         (final-path (loop for pair in meander-path append pair)))

    final-path))

;;
;; given a swath and a polygon, return points to fly
;; that swath over the plygon
;;

(defun swath-points (pg swath width/2)
  ;; break out the ceneter line and edges
      ;; otherwise get the first edge of the swath
    
  (let* ((edge1  (nth 0 swath))
         (center (nth 1 swath))
         (edge2  (nth 2 swath))
         
         ;; collect up all the points for testing
         
         (points (append
                  
                  ;; all the points on the polygon which exist inside the swath
                  
                  (points-near center (get-points pg) width/2)
                  
                  ;; the intesection between the polygon and edge1
                  
                  (intersect-points pg edge1)

                  ;; the intesection between the polygon and edge2
                  
                  (intersect-points pg edge2)))
         
         ;; project the points onto the center line
         
         (center-points (loop for p in points 
                            collect (intersect-at
                                     (create-perpendicular center p)
                                     center)))
         
         ;; sort the points by x value
         
         (sorted-points (if (and (<= (slope center) 1) (>= (slope center) -1))
                            (sort center-points (lambda (p1 p2) (< (x p1) (x p2))))
                          (sort center-points (lambda (p1 p2) (< (y p1) (y p2)))))))

    ;; return the first and last point
    (if sorted-points
        (cons (car sorted-points) (last sorted-points))
      nil)))
          
