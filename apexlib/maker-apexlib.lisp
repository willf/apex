;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Maker (legacy hack -- please don't use anymore)
;;; apex/apexlib/maker-apexlib.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: maker-apexlib.lisp,v 1.3 2006/01/15 03:42:49 dalal Exp $


;;; Obsolete macros for defining Simworld classes and methods.  Please
;;; stop using this library!


(in-package :common-lisp-user)

(defmacro class-maker (name type arglis)
  (let ((fullargs nil))
    (dolist (i arglis)
      (push (list i :initarg (intern (format nil "~a" i) :keyword)
                  :accessor i :initform nil)
            fullargs))
  `(defclass ,name (,type) ,fullargs)))

(defmacro instance-maker (type pos dim locale)
  `(make-instance ,type :name ,type :shape (list ,type) :locale ,locale
                 :pos ,pos :dimensions ,dim))

(defmacro action-maker (name)
  (let ((a-name (intern (format nil "~a-ACT" (symbol-name name))))
        (h-name (intern (format nil "~a-HANDLER" (symbol-name name)))))
    `(progn
       (class-maker ,a-name resource-activity  (object))
       (defmethod ,a-name ((p physob))
	 (,h-name p)
	 )
       (defmethod complete-activity ((act  ,a-name) (h hand))
	 (signal-event (,a-name (object act)))))))             
