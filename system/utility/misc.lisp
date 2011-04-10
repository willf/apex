;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/misc.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc.lisp,v 1.4 2006/01/15 03:43:03 dalal Exp $


(in-package :common-lisp-user)

;;; Delay (ala Scheme)

(defmacro delay (exp)                   ; ? -> () -> ?
  `(function (lambda () ,exp)))

(defun force (promise)                  ; (() -> ?) -> ?
  (funcall promise))

;;; Streams  -- just lists for now

(defparameter *the-empty-stream* '())
(defun make-stream (x) (list x))
(defun stream? (x) (listp x))
(defun stream-empty? (x) (null x))
(defun stream-cons (x xs) (cons x xs))
(defun stream-car (x) (car x))
(defun stream-cdr (x) (cdr x))
(defun stream-append (&rest lists) (apply #'append lists))
(defun stream-member (x s) (member x s))
(defun stream-length (x) (length x))

(defun list->stream (x)
  (if (listp x) x (error "Argument not a list: ~a~%" x)))


;;; General support


(defun map-over-sexp (x f pred)         ; any, (any -> any), (any -> bool) -> any
  (cond ((funcall pred x) (funcall f x))
        ((null x) '())
        ((listp x) (cons (map-over-sexp (car x) f pred)
                         (map-over-sexp (cdr x) f pred)))
        (t x)))

;;;(defun insert-into-ordered-list  (item list test) 
;;;  ;;
;;;  ;; A * List(A) * (A * A -> Bool) -> List(A)
;;;  ;;
;;;  ;; Returns a list in which <item> is inserted at the proper place in a
;;;  ;; list order by <test>.  This could be more efficient... in
;;;  ;; principle, the value for <item> computed in the test only needs to
;;;  ;; be determined once and the result reused.
;;;  ;;
;;;  ;; ! Could make more efficient (e.g. use tail recursion).
;;;  ;;
;;;  (cond 
;;;   ((null list) (cons item list))
;;;   ((funcall test item (first list)) 
;;;    (cons item list)) 
;;;   (t (cons (first list) (insert-into-ordered-list item (rest list) test)))))

(defun insert-into-ordered-list (item lis pred)
  ;;
  ;; A * List(A) * (A * A -> Bool) -> List(A)
  ;;
  ;; Returns a list in which <item> is inserted at the proper place in a
  ;; list order by <test>.  
  ;;
  ;; Iterative, destructive version. Faster; incredibly less consing;
  ;; does not cause stack overflows. -- waf
  (let ((insertl (list item)))
      (if (null lis)
	insertl
	(if (funcall pred item (car lis))
	  (progn  (setf (cdr insertl) lis) insertl)
	  (do ((prev lis (cdr prev))
	       (now (cdr lis) (cdr now)))
	      ((or (null now) (funcall pred item (car now)))
	       (setf (cdr prev) insertl)
	       (setf (cdr insertl) now)
	       lis))))))


(defun first-in-common (l1 l2)
  (if (and l1 l2)
      (if (member (first l1) l2)
	  (first l1)
	(first-in-common (rest l1) l2))))

(defun enqueue (val q)
  (setf q (append q (list val))))

(defun div (n d)
  ;;
  ;; int * int -> int * int
  ;; Divide two integers, returning the quotient and remainder.
  ;; ! Is something like this built in? 
  ;; 
  (values (floor (/ n d)) (rem n d)))

(defun do-nothing ())

(defun thunkify (form)  ; Any -> () -> ()
  #'(lambda () (eval form)))            ; ! Can EVAL be avoided?

(defmacro make-thunk (form)
  `(function (lambda () ,form)))

(defun pathname-to-string (p)      ; pathname + string -> string   (idempotent)
  (format nil "~a" p))

;;; Convenience for inspecting an object
(defmacro i (id)                        ; symbol -> Id-Mixin
  `(inspect (find-instance ',id)))

(defmacro see (label var)
  `(format t "---- ~a ~a: ~a~%" ',label ',var ,var))

(defun inspect-values* (vars vals)      ; list(symbol) * list(any) -> ()
  (when vars
    (format t " ---- ~a: ~a~%" (car vars) (car vals))
    (inspect-values* (cdr vars) (cdr vals))))
