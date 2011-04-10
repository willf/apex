;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/random.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: random.lisp,v 1.2 2006/01/15 03:43:03 dalal Exp $
;;; Created:        September, 2004

(in-package :cl-user)


(defvar *repeatable-random-state* (make-random-state *random-state*))

(defmacro with-constant-random-state (&body body)
  `(let ((*random-state* (make-random-state *repeatable-random-state*)))
     (declare (special *random-state*))
     ,@body))

;;; returns a random number in the gaussian distribution
;;; with a mean of MEAN and a standard deviation of STDDEV
;;; based on:
;;; Generating Gaussian Random Numbers
;;; http://www.taygeta.com/random/gaussian.html
;;; by Dr. Everett (Skip) F. Carter Jr.
;;; accessed 2004-09-14.  Will Fitzgerald.
(defun random-normal (&optional (mean 0) (stddev 1) (state *random-state*))
  (declare (special *random-state*))
  (flet ((r () (- (* 2.0 (random 1.0 state)) 1))
	 (ln (x) (log x)))
    (do* ((x1 (r) (r))
	  (x2 (r) (r))
	  (w (+ (* x1 x1) (* x2 x2))
	     (+ (* x1 x1) (* x2 x2))))
	((< w 1.0)
	 (let ((w (sqrt (/ (* -2.0 (ln w)) w))))
	   (+ mean (* x1 w stddev)))))))

;;; just returns the mean 
(defun random-constant (&optional (mean 0) (diff 1) (state *random-state*))
  (declare (ignore diff state))
  mean)

;;; returns a number between mean-diff to mean+diff with equal prob.
(defun random-equal (&optional (mean 0) (diff 1) (state *random-state*))
  (declare (special *random-state*))
  (if (zerop diff) mean
      (let ((x (random (float diff) state)))
	(if (zerop (random 2 state))
	  (- mean x)
	  (+ mean x)))))

;; returns a number between mean-diff to mean+diff with equal prob, with
;; numbers tending towards the higher side
(defun random-high-pass (&optional (mean 0) (diff 1) (state *random-state*))
  (declare (special *random-state*))
  (max (random-equal mean diff state)
       (random-equal mean diff state)))

;; returns a number between mean-diff to mean+diff with equal prob, with
;; numbers tending towards the lower side
(defun random-low-pass (&optional (mean 0) (diff 1) (state *random-state*))
  (declare (special *random-state*))
  (min (random-equal mean diff state)
       (random-equal mean diff state)))

;; returns a number between mean-diff to mean+diff with equal prob, with
;; numbers tending towards the center
(defun random-triangular (&optional (mean 0) (diff 1) (state *random-state*))
  (declare (special *random-state*))
  (/ (+ (random-equal mean diff state)
	(random-equal mean diff state))
     2.0))

;; lognormal from mean 
(defun random-lognormal (&optional (mean 0) (stddev 1) (state *random-state*))
  (declare (special *random-state*))
  (let ((x (log (abs (random-normal 0.0 stddev state)))))
    (- (+ mean stddev) x)))

;; 0 or 1 with the given probabiilty of a 0
(defun random-binary (&optional (probability-of-zero 0.5) (state *random-state*))
  (declare (special *random-state*))
  (if (> (random 1.0 state) probability-of-zero) 1 0))

(defun roll-die (&optional (sides 6) (state *random-state*))
   (let ((x (random 1.0 state))
	 (diff (/ 1.0 sides)))
     (do ((sum diff (+ sum diff))
	  (n   0    (1+ n)))
	 ((<= x sum) (1+ n)))))

(defun roll-dice (n &optional (sides 6) (state *random-state*))
  (loop for i from 1 to n summing (roll-die sides state)))

;; weighted dice (list .2 .2 .2 .2 .2) ... the last number isn't strictly necessary.
(defun roll-weighted-die (weights &optional (state *random-state*))
  (let ((x (random 1.0 state)))
    (do ((sum (car weights) (+ sum (car weights)))
	 (weights weights (cdr weights))
	 (n       0       (1+ n)))
	((or (<= x sum) (null weights)) (1+ n)))))

(defun roll-weighted-dice (n weights &optional (state *random-state*))
  (loop for i from 1 to n summing (roll-weighted-die weights state)))

;;;  prob of seeing an event with prob. p at least once in k tries: 1-(1-p)^k
;;;  what is k, if we want to see the event with confidence p'?
;;;  1 - (1-p)^k = d
;;;  (1-p)^k = 1 - d
;;;  k log(1-p) = log(1-d)
;;;  k = log(1-d)/log(1-p)
;;;  round up ...

(defun trials-required (confidence probability)
  (values (ceiling (/ (log (- 1 confidence)) (log (- 1 probability))))))
