;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/food.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: food.lisp,v 1.2 2006/01/15 03:42:56 dalal Exp $

(in-package :user)

;;; -- Food ----------------------------------------------------------

;;; This data structure defines a food as a kind of physical object that
;;;  has a cookstate and a cookrate.  Cookstate is number ranging from 
;;; 0 (raw) to 9 (badly burnt).  The most desirable cookstate will vary 
;;; for different foods. When sufficiently heated, a food object's 
;;; cookstate increases.  The cookrate specifies how many (simulated) 
;;; seconds are required for the cookstate to advance by 1 if the food's
;;; temp = 5.  In apartmentworld, the temp slot value ranges from 0 
;;; (room temperature) to 5 (very hot).

(defclass food (physob)
  ((cookstate 
    :accessor cookstate 
    :initarg 
    :cookstate)    
   ;; integer from 0 to 9 where 0 is raw and 9 is burnt to a crisp
   (cookrate 
    :accessor cookrate 
    :initarg 
    :cookrate)));; seconds of cooking needed to advance cookstate at std temp


;;;  ------- Macaroni -------------------------------------------------

(defclass macaroni (food) 
  ((texture
    :accessor texture 
    :initform 'hard)
   (shape 
    :accessor shape 
    :initform '(macaroni))
   (cookrate
    :accessor cookrate 
    :initform (normRand 10 .5))
   (cookstate 
    :accessor cookstate 
    :initform 0)))

(defmethod cookstate-changed ((mac macaroni))
  (setx (texture mac) (compute-mac-texture mac)))

(defun compute-mac-texture (mac)
  (let ((mc (cookstate mac)))
    (cond ((< mc 2.0) 'hard)
	  ((< mc 4.0) 'crunchy)
	  ((< mc 6.0) 'al-dente)
	  ((< mc 7.0) 'mushy)
	  (t 'paste))))

;;;  ------- Popcorn -------------------------------------------------

(defclass popcorn (food)
  ((texture 
    :accessor texture 
    :initform 'kernel)
   (shape 
    :accessor shape 
    :initform '(popcorn))
   (cookrate 
    :accessor cookrate 
    :initform (normRand 5 .2))
   (cookstate 
    :accessor cookstate 
    :initform 0)))

(defclass wrapper (physob)
  ((shape 
    :initform '(wrapper) 
    :initarg 
    :shape 
    :accessor shape)))

(defmethod cookstate-changed ((pop popcorn))
  (setx (texture pop) (compute-pop-texture pop)))

(defun compute-pop-texture (pop)
  (let ((pc (cookstate pop)))
    (cond ((< pc 2.0) 'kernel)
	  ((< pc 4.0) 'half-popped)
	  ((< pc 6.0) 'fluffy)
	  ((< pc 7.0) 'wilted)
	  (t 'burnt))))  

;; --- olive oil -------------------------------------------------

(defclass olive-oil (food)
  ((shape :initform '(olive-oil))
   (cookrate :initform (normRand 10 1))
   (isa :accessor isa :initform 'olive-oil :initarg :isa)))

(defmethod cookstate-changed ((o olive-oil))
  nil)

;; --- yellow onion -------------------------------------------------

(defclass yellow-onion (food)
  ((shape 
    :initform '(yellow-onion))
   (cookrate 
    :initform (normRand 9 1))
   (isa 
    :accessor isa 
    :initform 'yellow-onion 
    :initarg :isa)))

(defmethod cookstate-changed ((y yellow-onion))
  nil)

;; --- celery -------------------------------------------------

(defclass celery (food)
  ((shape 
    :initform '(celery))
   (cookrate 
    :initform (normRand 12 1))
   (isa 
    :accessor isa 
    :initform 'celery 
    :initarg :isa)))

(defmethod cookstate-changed ((c celery))
  nil)

;; --- carrots -------------------------------------------------

(defclass carrot (food)
  ((shape 
    :initform '(carrot))
   (cookrate 
    :initform (normRand 12 .5))
   (isa 
    :accessor isa 
    :initform 'carrot 
    :initarg :isa)))

(defmethod cookstate-changed ((c carrot))
  nil)

;; --- tomatoes -------------------------------------------------

(defclass tomato (food)
  ((shape :initform '(tomato))
   (cookrate :initform (normRand 15 1))
   (isa :accessor isa :initform 'tomato :initarg :isa)))

(defmethod cookstate-changed ((tom tomato))
  nil)

;; --- green beans -------------------------------------------------

(defclass green-bean (food)
  ((shape 
    :initform '(green-bean))
   (cookrate 
    :initform (normRand 14 .25))
   (isa 
    :accessor isa 
    :initform 'green-bean 
    :initarg :isa)))

(defmethod cookstate-changed ((gb green-bean))
  nil)

;; --- potatoes -------------------------------------------------

(defclass potato (food)
  ((shape 
    :initform '(potato))
   (cookrate 
    :initform (normRand 20 2))
   (isa 
    :accessor isa 
    :initform 'potato 
    :initarg :isa)))

(defmethod cookstate-changed ((p potato))
  nil)

;; --- stock -------------------------------------------------

(defclass stock (food)
  ((shape 
    :initform '(stock))
   (cookrate 
    :initform (normRand 40 3))
   (isa 
    :accessor isa 
    :initform 'stock 
    :initarg :isa)))

(defmethod cookstate-changed ((s stock))
  nil)

;;;  -------  SOUP STUFF -------------------------------------------------

;;; All contents of the soup had to have a cookstate-changed method
;;; in order for me to use the cooking functionality already
;;; implemented for the egg.  The methods don't do anything.

(defclass soup (food)
  ((contents 
    :accessor contents 
    :initarg 
    :contents 
    :initform nil)
   (cookrate 
    :accessor cookrate 
    :initform (normRand 39 5))))

;;sets the cookstate of all objects in the soup to be the same.
;;It is difficult to model how soup cooks, so this is an easy,
;;if wrong, out.

(defmethod cookstate-changed ((s soup))
  (mapcar #'(lambda (x) (setf (cookstate x) (cookstate s))) 
          (contents s)))
