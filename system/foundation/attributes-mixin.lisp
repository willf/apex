;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/attributes-mixin.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: attributes-mixin.lisp,v 1.2 2006/01/15 03:43:01 dalal Exp $

;;; This file defines a mixin class that an interface for attributes,
;;; and related support.  Atrributes are name/value pairs where the name
;;; is a symbol and the value can be any type.

(in-package :user)

(defclass attributes-mixin ()
  ((attributes                          ; A-list of name-value pairs
    :type list
    :accessor attributes
    :initform nil
    :initarg :attributes)))

(defmethod attribute-names ((x attributes-mixin))
  (mapcar #'car (attributes x)))

(defmethod attribute-values ((x attributes-mixin))
  (mapcar #'cdr (attributes x)))

(defmethod set-attribute ((x attributes-mixin) (name symbol) val)
  (let ((binding (assoc name (attributes x))))
    (if binding
        (rplacd binding val)
      (push (cons name val) (attributes x)))
    (values)))

(defmethod get-attribute-value ((x attributes-mixin) (name symbol))
  (cdr (assoc name (attributes x))))

;;; Create a thunk from the given Lisp form and instance such that the
;;; form can access and update attributes as though they are locally
;;; bound variables.

(defmacro make-attribute-thunk (attr-mixin form)
  ;; Attribute-Mixin * LispExpression -> any
  `(function
    (lambda ()
      ;; Create a lexical scope from the attributes
      (progv
          (attribute-names ,attr-mixin)
          (attribute-values ,attr-mixin)
        ;; Evaluate the form and save result
        (let ((result ,form))
          ;; Update the attributes
          (mapc #'(lambda (s)
                    (set-attribute ,attr-mixin s (symbol-value s)))
                (attribute-names ,attr-mixin))
          result)))))

