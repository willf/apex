;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; ATM library
;;; apex/examples/apexlib/atm-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: atm-apexlib.lisp,v 1.9 2006/01/15 03:42:52 dalal Exp $

(in-package :user)

(require-apex-library "interface-object")
(require-apex-library "human")

(defclass atm (interface-object)
  ((password-list :initarg :password-list :accessor password-list :initform nil) 
   (entry :initarg :entry :accessor entry :initform nil)
   (keypad :initarg :keypad :accessor keypad :initform nil)
   (screen :initarg :screen :accessor screen :initform nil)
   (status :initarg :status :accessor status :initform nil)))

(defclass card (interface-object)
  ((pid :initarg :pid :accessor pid :initform nil)))

(defclass card-slot (interface-object)
  ((pid :initarg :pid :accessor pid :initform nil)))

(defclass money-slot (interface-object) nil)

(defmethod click-handler ((c card-slot))
  (pid-handler (device c) (pid c)))

(defmethod click-handler ((m money-slot))
  (retrieve-money-handler (device m)))

(defmethod pid-handler ((a atm) pid)
  (setf (status a) (list 'password
        (second (assoc pid (password-list a) :test 'equal))))
  (setf (entry a) "")
  (display-handler (screen a) '(enter password)))

(defmethod button-handler ((atm atm) name)
  (cond ((and (listp (status atm))
              (eq (first (status atm)) 'password))
         (cond ((eq name 'enter)
                (cond ((equal (entry atm) (second (status atm)))
                       (setf (entry atm) "")
                       (setf (status atm) 'amount)
                       (display-handler (screen atm) '(enter amount)))
                      (t
                       (display-handler (screen atm) '(incorrect password)))))
               (t
                (setf (entry atm) (format nil "~a~a" (entry atm) name)))))
        ((eq (status atm) 'amount)
         (cond ((eq name 'enter)
                (setf (entry atm) "")
                (setf (status atm) 'money)
                (display-handler (screen atm) '(retrieve money)))
               (t
                (setf (entry atm) (format nil "~a~a" (entry atm) name)))))))

(defmethod retrieve-money-handler ((a atm))
  (setf (status a) 'card)
  (display-handler (screen a) '(retrieve card)))

(defmethod retrieve-card-handler ((a atm))
  (setf (status a) 'done)
  (display-handler (screen a) '(thank you)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUTTON

(defclass button (interface-object) nil)


(defun create-button (name)
  (make-instance 'button :name name
                 :shape (list 'button name)
                 :elements (list 'text name)))

(defmethod push-handler ((b button))
  (button-handler (device b) (second (elements b))))

(defmethod click-handler ((b button))
  (button-handler (device b) (second (elements b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYPAD

(defclass keypad (interface-object)
  ((entry :initarg :entry :accessor entry :initform nil)
   (orientation :initarg :orientation :accessor orientation :initform nil)
   (button-list :initarg :button-list :accessor button-list :initform nil)))

(defun create-keypad (pos dim spacing orient locale  &optional (diagram-type 'wireframe))
  (let* ((dx (first dim))
	 (dy (second dim))
	 (sx (first spacing))
	 (sy (second spacing))
	 (name)
	 (tmp)
	 (x (first pos))
	 (y (second pos))
	 (pad (make-instance 'keypad :name 'keypad :shape '(keypad)
			     :pos pos :dimensions (list (+ dx sx dx sx dx)
							(+ dy sy dy sy dy sy dy))
			     :locale locale :orientation orient :entry ""))
	 key0 key1 key2 key3 key4 key5 key6 key7 key8 key9  button-list)
    (declare (special tmp key0 key1 key2 key3 key4 key5 key6 key7 key8 key9))  ;;need to do this to assign keys
    (dotimes (i 10)
      (setf name (read-from-string (format nil "key-~a" i)))
      (setf tmp
        (make-instance 'button :name name :device pad :shape `(,i)
                       :dimensions dim
                       :elements `(text ,i) :locale locale))
      (setf (symbol-value (read-from-string (format nil "key~a" i))) tmp)  ;;assign keys to use below
      (assemble tmp :component-of pad)
      )
    (setq button-list (list key0 key1 key2 key3 key4 key5 key6 key7 key8 key9))
    
    (cond ((equal orient 'up)
           (setf (pos key7) (list x y))
           (setf (pos key8) (list (+ x dx sx) y))
           (setf (pos key9) (list (+ x dx sx dx sx) y))
           (setf (pos key4) (list x (+ y dy sy)))
           (setf (pos key5) (list (+ x dx sx) (+ y dy sy)))
           (setf (pos key6) (list (+ x dx sx dx sx) (+ y dy sy)))
           (setf (pos key1) (list x (+ y dx sy dx sy)))
           (setf (pos key2) (list (+ x dx sx) (+ y dy sy dy sy)))
           (setf (pos key3) (list (+ x dx sx dx sx) (+ y dy sy dy sy)))
           )
          (t
           (setf (pos key1) (list x y))
           (setf (pos key2) (list (+ x dx sx) y))
           (setf (pos key3) (list (+ x dx sx dx sx) y))
           (setf (pos key4) (list x (+ y dy sy)))
           (setf (pos key5) (list (+ x dx sx) (+ y dy sy)))
           (setf (pos key6) (list (+ x dx sx dx sx) (+ y dy sy)))
           (setf (pos key7) (list x (+ y dy sy dy sy)))
           (setf (pos key8) (list (+ x dx sx) (+ y dy sy dy sy)))
           (setf (pos key9) (list (+ x dx sx dx sx) (+ y dy sy dy sy)))))
    (setf (pos key0) (list (+ x dx sx) (+ y dy sy dy sy dy sy)))
    (setf (button-list pad) button-list)

    (cond ((eq diagram-type 'hierarchical) 
	   ;;hierarchical diagram, rounded corner keys, colored keypad, labeled keys
	   (setf (graphical-object pad)
		  (make-instance  'rect 
				  :x x :y y 
				  :width (+ (* 3 dx) (* 2 sx))
				  :height (+ (* 4 dy) (* 3 sy))
				  :fill "red" :fill-opacity 1 :stroke "black"))
	   (dotimes (i 10)
	     (let* ((key (eval (read-from-string (format nil "key~a" i))))
		    (pos (pos key))
		    (x (first pos))
		    (y (second pos))
		    (dim (dimensions key))
		    (width (first dim))
		    (height (second dim)))

	       (setf (graphical-object key) 
		  (make-instance 'rect
				 :fill "white" :fill-opacity 1 :stroke "black" :rx 2 :ry 2
				 :x x :y y :width width :height height))
	       (auto-label (graphical-object key) (format nil "~a" i))
	       ;;(format t "key ~a :x ~a :y ~a :width ~a :height ~a~%"   i (* x scale) (* y scale) (* width scale) (* height scale))
	       ))
	   )
	  ((eq diagram-type 'wireframe)
	   ;;basic diagram, simple black and white rectangles, no labels
	   (setf (graphical-object pad)
		 (make-instance  'rect 
				 :x x :y y
				 :width (+ (* 3 dx) (* 2 sx))
				 :height (+ (* 4 dy) (* 3 sy))
				 :fill "white" :fill-opacity 1 :stroke "black"))
	   (dotimes (i 10)
	     (let* ((key (eval (read-from-string (format nil "key~a" i))))
		    (pos (pos key))
		    (x (first pos))
		    (y (second pos))
		    (dim (dimensions key))
		    (width (first dim))
		    (height (second dim)))

	       (setf (graphical-object key) 
		     (create-wireframe-graphical-object key))
		     
	       ))
	   ))
    pad))

(defun pad-print (keypad)
  (dolist (b (button-list keypad))
    (format t "~a ~a ~a~&" (name b) (dimensions b) (pos b))))

;(defun nice2 (x)
;  (read-from-string (format nil "~2,2f" x)))

(defmethod button-handler ((k keypad) name)
  (button-handler (device k) name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCREEN

(defclass screen (interface-object)
  ((obj-list :initarg :obj-list :accessor obj-list :initform nil)))

(defclass phrase (interface-object)
  ((name :initarg :name :accessor name :initform nil)))

(defun display-phrase (screen phrase pos)
  (let ((p nil)
        (newpos (convert-pos pos '(0 0) (pos screen))))
    (dolist (i (obj-list screen))
      (if (equal (pos i) newpos)
        (setf p i)))
    (when (not p)
      (setf p (make-instance 'phrase :shape '(phrase) :pos newpos
                             :locale (locale screen)))
      (assemble p :component-of (component-set screen))
      (push p (obj-list screen)))
    (setx (elements p) phrase) ; SETX allows vis update
    ))

(defmethod display-handler ((s screen) args)
  (display-phrase s args '(10 10)))

; pos local to from, from and to global
(defun convert-pos (pos from to)
  (list (+ (+ (first pos) (first from)) (first to))
        (+ (+ (second pos) (second from)) (second to))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOUSE

(defvar *mouse* nil)

(defclass pointer (visob) nil)

(defclass mouse (interface-object)
  ((status :initarg :status :accessor status :initform nil)
   (object :initarg :object :accessor object :initform nil)
   (pointer :initarg :pointer :accessor pointer :initform nil)))

(defun create-mouse (locale)
  (let ((pointer (make-instance 'pointer :locale locale :pos '(0 0))))
    (assemble pointer)
    (setf *mouse* (make-instance 'mouse :locale locale :pointer pointer))))

(defmethod mouse-move-handler ((v visob))
  (setf (object *mouse*) v)
  (setf (pos (pointer *mouse*))
        (pos v)))

(defmethod mouse-click-handler ((v visob))
;  (click-handler v)
)

(defmethod mouse-down-handler ((v visob))
  (setf (status *mouse*) 'down))

(defmethod mouse-up-handler ((v visob))
;  (click-handler v)
  (setf (status *mouse*) 'up))

(primitive (index (move-mouse to ?object with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion (signal-event (mouse-move-act ?object))))

(defmethod mouse-move-act ((p physob))
  (mouse-move-handler p))

(primitive     (index (mouse-click ?object with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion (signal-event (mouse-click-act ?object))))
 
(defmethod mouse-click-act ((p physob))
  (mouse-click-handler p))

(primitive
 (index (mouse-down on ?object with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion  (signal-event (mouse-down-act ?object))))

(defmethod mouse-down-act ((p physob))
  (mouse-down-handler p))

(primitive
 (index (mouse-up on ?object with ?hand taking ?duration))
 (profile ?hand)
 (duration ?duration)
 (on-completion  (signal-event (mouse-up-act ?object))))

(defmethod mouse-up-act ((p physob))
  (mouse-up-handler p))
  
(defmethod atm-fitts-time ((pointer visob) (obj visob))
  (let* (;;(obj (lookup-unique-name obj *agent*))
         (d (distance (pos pointer) (pos obj)))
         (s (min (first (dimensions obj))
                 (second (dimensions obj)))))
    (if obj     
        (fitts d s)
      (format t "Error: no object found~&"))))


