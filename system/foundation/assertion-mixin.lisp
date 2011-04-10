;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/assertion-mixin.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: assertion-mixin.lisp,v 1.3 2006/01/15 03:43:01 dalal Exp $

;;; Interface to assertion-database.lisp

(in-package :user)

;;; This is a mixin class for providing assertion and query capability.
;;; It's a CLOS wrapper around the assertion database referenced above,
;;; which is based on Chapter 4 of SICP.  See that file for more info.

(defclass assertion-mixin ()
  ((suppressed-slots
    :allocation :class
    :reader suppressed-slots
    :initform '(database operation-table))
   (database
    :reader database
    :initform (make-memory-table))
   (operation-table
    :reader optable
    :initform (init-query-operators (make-memory-table))
    :allocation :class)))


;;; Function to assert a proposition.
;;; Example: (apex-assert x '(color car blue))

(defmethod apex-assert ((a assertion-mixin) assertion)
  (let ((qget (funcall (database a) 'lookup-proc))
        (qput (funcall (database a) 'insert-proc)))
    (assert* assertion)
    (log-event assertion)
    (values)))


;;; Function to query the database.
;;; Example:  (apex-query x '(color car ?color))

(defmethod apex-query ((a assertion-mixin) assertion)
  (let ((opget (funcall (optable a) 'lookup-proc))
        (qget (funcall (database a) 'lookup-proc))
        (qcontents (funcall (database a) 'contents-proc)))
    (query* assertion)))
