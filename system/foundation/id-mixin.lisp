;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/id-mixin.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: id-mixin.lisp,v 1.13 2006/01/15 03:43:01 dalal Exp $


(in-package :common-lisp-user)

;;; ----------------------------------------------------------------------------
;;; ---- Id-Mixin class
;;;
;;; This is a support class that provides a name and unique ID for each
;;; instance of each class type, and a print method.  It is a "mixin"
;;; class, an idiom in which users of this class (e.g. Appob)
;;; incorporate its features by inheriting from it.  I don't
;;; particularly like the semantics of this idiom because it confuses
;;; aggregation of properties with subtyping; however, the "-mixin"
;;; suffix indicates that aggregation is the intention.

(defconstant *id-mixin-first-number* 1)

(defclass id-mixin ()
  ((name :type string                 ; A descriptive name; NOT used as part of 
         :initform "unspecified"      ; object's unique ID.
         :initarg :name
         :accessor name)
   (num  :type integer                ; Number of this instance, which
				      ; is the total number of instances
				      ; of most specific class.
         :initform *id-mixin-first-number*
          :accessor num)
   (id :type string                   ; Unique id: concatenation of
				      ; class name with Number.
       :accessor id)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(name num numbers))
   (numbers  
    :type list     ; Class-scoped a-list that maps each class name to the
		   ; number for the next instance of that class.
    :allocation :class
    :initform '()
    :accessor numbers)))

(defmethod initialize-instance :after ((obj id-mixin) &rest initargs)
  ;; Id-Mixin -> ()
  (declare (ignore initargs))
  (let* ((type (type-of obj))
         (entry (assoc type (numbers obj) :test #'eq))
         (num (or (cdr entry) *id-mixin-first-number*)))
    (when (null entry)
      ;; If no instances exist, create table entry for class name.
      (setq entry (cons type num))
      (push entry (numbers obj)))
    (incf (cdr entry))                  ; increment number for next instance
    (setf (num obj) num)                ; assign number of this instance
    (setf (id obj) (intern (format nil "~a-~a" type num))) ; assign id
    (register-instance obj)))

;; If true, object numbers will be omitted from id-mixin's print form.
(defparameter *hide-object-numbers* nil)

(defmethod print-object ((ob id-mixin) s) ; Type = IdMixin * Stream -> ()
  (format s "#{~a~a}"
          (if *hide-object-numbers* (type-of ob) (id ob))
          (if (name ob) (format nil " ~a" (name ob)) ""))
  (values))


;;; ---- Instance Registry
;;;
;;; Provides a means to find Apex objects (Id-Mixin based) given their
;;; ID and/or class.

(defun make-table ()
  ;;
  ;; Defines a "headed" list that will represent a two-dimensional table.
  ;; ! Almost no abstraction here.  Make nicer later.
  ;;
  (list '*table))

(defvar *instance-registry*
    ;;
    ;; A-list defining partial function: Class -> ID -> Id-Mixin
    ;; where Class and ID are symbols.
    ;;
    (make-table))

(defun find-instance (id)
  ;;
  ;; symbol -> Id-Mixin
  ;; Look up an object in the registry.
  ;;
  (let* (
         ;; ! I hate to dissect a symbol, but that's the easiest way to get
         ;; the type and hence support a semi-efficient search.  Could
         ;; abstract "id" better to avoid this need
         (class-name (intern
                      (string-right-trim "1234567890-"
                                         (symbol-name id))))
         (subtable (assoc class-name (cdr *instance-registry*))))
    (if subtable (cdr (assoc (lowercase-intern-id id) (cdr subtable))))))

(defun lowercase-intern-id (id)
  ;;return the interned lowercase id
  (intern (format nil "~a" id)))

(defun collect-ids (type)
  ;;
  ;; () -> list(symbol)
  ;; Return names of all objects of given type (class).
  ;;
  (mapcar #'car (cdr (assoc type (cdr *instance-registry*)))))


(defun collect-classes ()
  ;;
  ;; () -> list(symbol)
  ;; Return names of all classes in registry.
  ;; ! There may be some "junk" names in this list that we can filter if
  ;; needed.
  ;;
  (mapcar #'car (cdr *instance-registry*)))

(defmethod register-instance ((obj id-mixin)) ; Id-Mixin -> ()
  ;;
  ;; Id-Mixin -> symbol
  ;; Add an object to the registry.
  ;;
  (let ((key1 (type-of obj))
        (key2 (id obj)))
    (let ((subtable (assoc key1 (cdr *instance-registry*))))
      (if subtable
          ;; ! would 'push' be better?
          (rplacd subtable
                  (cons (cons key2 obj)
                        (cdr subtable)))
        (rplacd *instance-registry*
                (cons (list key1 (cons key2 obj))
                      (cdr *instance-registry*))))
      key2)))


(defun reset-instance-registry ()
  ;; () -> ()
  (setq *instance-registry* (make-table))
  (reset-object-numbers))

(defun reset-object-numbers ()          ; -> ()
  ;; Reset the numbering of Id-Mixin instances.
  ;; Since we need an instance to invoke the method, just create a temporary one.
  (let ((obj (make-instance 'id-mixin)))
    (setf (numbers obj) '())))


;;; Support for save/restore

(defvar *saved-instance-registry* (make-table))

(defun save-instance-registry ()
  (setq *saved-instance-registry* (copy-list *instance-registry*)))

(defun restore-instance-registry ()
  (setq *instance-registry* (copy-list *saved-instance-registry*)))


(defun instance-named (name)
  "Find an instance with the name; symbol -> id-mixin"
  (or (find-agent name)
      (router-named name)
      (dolist (alist (cdr *instance-registry*))
	     (unless (or (eql (car alist) 'ps-router)
		             (eql (car alist) 'agent)
		    (dolist (entry (cdr alist))
		      (when (equal name (name (cdr entry)))
			(return-from instance-named (cdr entry)))))))))

