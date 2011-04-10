;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/son-of-pubsub.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: son-of-pubsub.lisp,v 1.4 2006/03/17 20:06:25 will Exp $


(in-package :cl-user)

(defclass type-router ()
  ((type->subscribers-table
    :initform (make-hash-table :test #'eql)
    :accessor type->subscribers-table)
   (subscribers->type-table
    :initform (make-hash-table :test #'eql)
    :accessor subscribers->type-table)
   (anykey 
    :initform (gensym)
    :accessor anykey
    :allocation :class
    :documentation "Internal 'keyword' for a subscriber to any item.")
  ))

(defun make-type-router ()
  (make-instance 'type-router))

(defmethod print-object ((r type-router) (stream stream))
  (print-unreadable-object (r stream :type t :identity t)
    (format stream "~a subscriber~:P to ~a type~:P"
	    (hash-table-count (subscribers->type-table r))
	    (hash-table-count (type->subscribers-table r)))))

(defmethod add-subscriber ((x t) (type symbol) (r type-router))
  "Add a subscriber to a type. Returns subscriber."
  (pushnew x (gethash type (type->subscribers-table r)))
  (pushnew type (gethash x  (subscribers->type-table r)))
  x)

(defmethod add-general-subscriber ((x t) (r type-router))
  "Add a subscriber that will get any incoming type. Returns subscriber."
  (add-subscriber x (anykey r) r))

(defmethod subscriptions-for ((x t) (r type-router))
  "Return all subscriptions for a subscriber."
  (values (gethash x  (subscribers->type-table r))))

(defmethod subscribers-of ((type symbol) (r type-router))
  "Return all subscribers to a type, including general subscribers."
  (union 
   (gethash type (type->subscribers-table r))
   (gethash (anykey r) (type->subscribers-table r))))

(defmethod specific-subscriber-to-p ((x t) (type symbol) (r type-router))
  (member type (subscriptions-for x r) :test #'eql))

(defmethod general-subscriber-to-p ((x t) (r type-router))
  (member (anykey r) (subscriptions-for x r) :test #'eql))

(defmethod subscribes-to-p ((x t) (type symbol) (r type-router))
  "Is X subscribed to type or to a general subscriber?"
  (and 
   (or (specific-subscriber-to-p x type r)
       (general-subscriber-to-p x r))
   t))
       

(defmethod specific-subscribers ((type symbol) (r type-router))
  "Return all subscribers to a type, not including general subscribers."
  (values (gethash type (type->subscribers-table r))))

(defmethod general-subscribers ((r type-router))
  "General subscribers to a router."
  (values (gethash (anykey r) (type->subscribers-table r))))

(defmethod remove-subscriber-from ((x t) (type symbol) (r type-router))
  "Remove a subscriber from a type. "
  (let ((subs
	 (setf (gethash type (type->subscribers-table r))
	   (remove x (gethash type (type->subscribers-table r))))))
  (when (null subs)
    (remhash type (type->subscribers-table r))))
  (let ((types (setf (gethash x  (subscribers->type-table r))
		 (remove type (gethash x  (subscribers->type-table r))))))
  (when (null types)
    (remhash x (subscribers->type-table r))))
  (values))

(defmethod remove-subscriber ((x t) (r type-router))
  "Remove a subscriber completely."
  (let ((types (gethash x (subscribers->type-table r))))
    (dolist (type types)
      (setf (gethash type (type->subscribers-table r))
	(remove x (gethash type (type->subscribers-table r)))))
    (remhash x (subscribers->type-table r)))
  (values))

  

