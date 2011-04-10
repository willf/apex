;;;-*- Mode: Lisp; Package:  :apex.utility.fp -*-
;;;
;;; apex/system/utility/fp.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: fp.lisp,v 1.9 2006/01/15 03:43:02 dalal Exp $

;;; Functional programming utilities


(defpackage :apex.utility.fp
  (:use :common-lisp)
  (:export
   #:compose
   #:foldr1
   #:foldr
   #:ormap
   #:zipwith
   #:take
   #:filter
   #:flatten
   #:interleave-lists
   #:mappend
   )
  )

(in-package :apex.utility.fp)

(defun compose (f g)                    ; (b -> c), (a -> b) -> (a -> c)
  (function
   (lambda (x) (funcall f (funcall g x)))))

(defun foldr1 (f lst)                   ; (a,a -> a), list(a) -> a
  ;;
  ;; Right-associative folding over lists of at least 1 element
  ;;
  (cond ((null lst)
         (error "foldr1: list must have at least one element"))
        ((= 1 (length lst))
         (car lst))
        (t (funcall f (car lst) (foldr1 f (cdr lst))))))

(defun foldr (f lst init)               ; (a,b -> b), list(a), b -> b
  ;;
  ;; Right-associative folding with given initial value
  ;;
  (cond ((null lst) init)
        (t (funcall f (car lst) (foldr f (cdr lst) init)))))

(defun ormap (f l)                      ; (a -> bool), list(a) -> bool
  ;;
  ;; Return true iff predicate f is true for at least one element of list l.
  ;;
  (cond
   ((null l) nil)
   ((funcall f (car l))
    t)
   (t (ormap f (cdr l)))))


(defun zipwith (f as bs)         ; (A, B -> C), list(A), list(B) -> list(C)
  ;;
  ;; Zip two equal-length lists together using element combiner function f.
  ;; Note on type: depending on f, lists may also be heterogeneous.
  ;;
  (if (not (= (length as) (length bs)))
      (error "zipwith: lists must be of equal length!"))
  (cond ((null as) '())
        (t (cons (funcall f (car as) (car bs))
                 (zipwith f (cdr as) (cdr bs))))))


(defun take (n xs)                      ; int, list(any) -> list(any)
  ;;
  ;; Return the first n elements of a list; if list has less than n
  ;; elements, return the list; if n is non-positive, return nil.
  ;;
  (labels ((take1 (n new old)
             (cond ((< (length old) n) old)
                   ((< n 1) new)
                   (t (take1 (- n 1) (append new (list (car old))) (cdr old))))))
    (take1 n nil xs)))

(defun filter (f xs &key (key #'identity))
  ;;
  ;; (b -> bool) * list(a) * (a -> b) -> list(a)
  ;; Return elements of a list (or of values obtained with KEY) that
  ;; satisfy the predicate F
  ;;  
  (loop for x in xs when (funcall f (funcall key x)) collect x))


(defun flatten (x)
  (cond ((null x) '())
        ((atom x) (list x))
        ;; Allow 'nil' elements to remain.
        ((null (car x)) (cons nil (flatten (cdr x))))
        (t (append (flatten (car x)) (flatten (cdr x))))))

(defun interleave-lists (xs ys) ; list * list -> list
  (cond ((null xs) ys)
        (t (cons (car xs) (interleave-lists ys (cdr xs))))))

(defun mappend (fn list) ; fn * list -> list
  (loop for x in list appending (funcall fn x)))