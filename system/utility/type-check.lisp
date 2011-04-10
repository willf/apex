;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/type-check.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: type-check.lisp,v 1.3 2006/01/15 03:43:03 dalal Exp $

;;; Type checking for functions and macros.  See type-check.text for
;;; documentation.


(in-package :user)

;;; --------------------------- API

(defparameter *error-on-type-mismatch*
    ;;
    ;; If true, a type mismatch will signal an error; otherwise only a
    ;; message is issued.
    ;;
    t)

(defmacro type-check (name &rest clauses) ; symbol, list(any) -> ()
  "Top level form for type checking"
  `(progn
     ,@(mapcar (expand-type-check `,name) `,clauses)))


;;; ---------------- Type Predicates

;;; Simple types

;;; Number
(defparameter tc-number                 ; any -> bool
    #'(lambda (x) (numberp x)))

;;; Symbol
(defparameter tc-symbol                 ; any -> bool
    #'(lambda (x) (symbolp x)))

;;; Integer
(defparameter tc-int                    ; any -> bool
    #'(lambda (x) (integerp x)))

;;; Natural number
(defparameter tc-nat                    ; any -> bool
    #'(lambda (x) (and (integerp x) (>= x 0))))

;;; Positive integer
(defparameter tc-posint                 ; any -> bool
    #'(lambda (x) (and (integerp x) (> x 0))))

;;; Function
(defparameter tc-function               ; any -> bool
    #'(lambda (x) (functionp x)))

;;; String
(defparameter tc-string                 ; any -> bool
    #'(lambda (x) (stringp x)))

;;; Boolean
(defparameter tc-bool                   ; any -> bool
    #'(lambda (x)
        (or (null x) (eq x t))))

;;; "Any Type"
(defparameter tc-any                    ; any -> bool
    #'(lambda (x)
        (declare (ignore x))
        t))

;;; Higher order types

;;; List (homogeneous)
(defun tc-list (type)                   ; (any -> bool) -> any -> bool
  #'(lambda (x)                         ; any -> bool
      (and (listp x) (every type x))))

;;; Optional
(defun tc-opt (type)                    ; (any -> bool) -> any -> bool
  #'(lambda (x)
      (or (null x) (funcall type x))))

;;; Sum
(defun tc-either (&rest types)          ; list(any -> bool) -> any -> bool
  #'(lambda (x)                         ; any -> bool
      (ormap
       #'(lambda (type) (funcall type x))
       types)))



;;; ----------- Implementation


(defun expand-type-check (name)         ; symbol -> function
  ;;
  ;; The top-level macro type-check and this function comprise a
  ;; mini-compiler that translates the user form into a sequence of
  ;; calls to the type checking function.
  ;;
  #'(lambda (clause) 
      ;; (symbol any) -> (symbol symbol symbol function list(any))
      `(type-check-parameter ',name
                             ',(car clause)
                             ,(car clause)
                             (tc-convert ',(cadr clause)))))

(defun type-check-parameter (name formal actual type)
  ;;
  ;; symbol, symbol, any, (any -> bool) -> ()
  ;;
  ;; The type checker itself.  Applies the type predicate to the actual
  ;; value and takes the appropriate action based on success.
  ;;
  (if (not (funcall (eval type) actual))
      (let* ((line1 (format nil "Type error in ~a:" name))
             (line2 (format nil "  Parameter ~a given bad value ~a"
                            formal actual))
             (line3 (format nil "  Expected type ~a" (tc-unconvert type)))
             (message (format nil "~a~%~a~%~a~%" line1 line2 line3)))
        (if *error-on-type-mismatch*
            (error message)
          ;; else
          (format t message))
        nil)
    t))

;;; Functions (a bit hacky but good enough) to handle the "tc-" prefix
;;; of type predicates.

(defun tc-convert (x)                 ; symbol + list(any) -> any
  (map-over-sexp x #'tcify #'symbolp))

(defun tc-unconvert (x)                 ; symbol + list(any) -> any
  (map-over-sexp x #'untcify #'symbolp))

(defun tcify (x) ; symbol -> symbol
  (intern (format nil "TC-~a" (symbol-name x))))

(defun untcify (x) ; symbol -> symbol
  (intern (subseq (symbol-name x) 3)))
