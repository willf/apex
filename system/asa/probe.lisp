;;-*- Mode: Lisp; Package: :cl-user  -*-
;;; ------------------------------------------------------------------------
;;; File:           $Id: probe.lisp,v 1.7 2006/01/15 03:43:00 dalal Exp $
;;; Created:        August. 2004
;;; Author:         Will Fitzgerald

;;
;; Code for probes ...

(in-package :cl-user)

(defclass probe (appob)
  ((count-limit :initarg :count-limit :initform NIL :accessor probe-count-limit)
   (time-limit  :initarg :time-limit :initform NIL :accessor probe-time-limit)
   (frequency-limit  :initarg :frequency-limit :initform NIL :accessor probe-frequency-limit)
   (user-function  :initarg :function :initform NIL :accessor probe-user-function)
   (function :initarg :probe-function :accessor probe-function)
   (state-variable :initarg :state-variable :initform NIL :accessor probe-sv)
   (task  :initarg :task :accessor probe-task)
   (active :initarg :active :initform NIL :accessor probe-active)
   (start :accessor start-of)
   (end   :accessor end-of)
   (call-count :initform 0 :accessor probe-call-count)))

(defmethod probe-inactive ((probe probe))
  (not (probe-active probe)))

(defun make-probe (state-variable task function frequency-limit &key count-limit time-limit)
  (assert (typep state-variable 'state-variable))
  (assert (typep task 'task))
  (assert (or (functionp function) 
	      (and (symbolp function) (fboundp function))
	      (and (variable-p function)
		   (eq :indexical-functional (variable-type function))
		   (fboundp (variable-name function))))
      nil
    "Invalid probe function: ~a" function)
  (assert (not (null frequency-limit)))
  (let ((probe 
	 (make-instance 'probe
	   :state-variable state-variable
	   :task task
	   :function 
	   (cond
	    ((functionp function) function)
	    ((and (symbolp function)
		  (fboundp function))
	     (symbol-function function))
	    ((variable-p function)
	     (lambda ()
	       (funcall (symbol-function (variable-name function))
			task))))
	      :count-limit count-limit
	   :time-limit (if time-limit (duration-read time-limit) NIL)
	   :frequency-limit frequency-limit)))
    (setf (probe-function probe) (make-probe-function probe function))
    probe))

(defmethod print-object ((probe probe) stream)
  (print-unreadable-object (probe stream :type t :identity t)
    (let ((sv (probe-sv probe)))
      (format stream "for (~a ~a) ~a"
	      (sv-attribute sv) (sv-object sv) (probe-function probe))
      (with-slots (count-limit time-limit frequency-limit) probe
	(when count-limit
	  (format stream " :count-limit ~A" count-limit))
	(when time-limit
	  (format stream " :time-limit ~A" time-limit))
	(when frequency-limit
	  (format stream " :frequency-limit ~A" frequency-limit))))))


(defmethod make-probe-function ((probe probe) function)
  (lambda ()
    (when (probe-active probe)
      (cogevent `(,(sv-attribute (probe-sv probe))
		  ,(sv-object (probe-sv probe))
		  =
		  ,(funcall function))
		(agent (probe-task probe))
		:trigger-asa t))))


(defmethod stop-probe ((probe probe))
  (setf (end-of probe) (current-time))
  (setf (probe-active probe) NIL)
  probe)

(defmethod start-probe ((probe probe))
  (setf (start-of probe) (current-time))
  (setf (probe-active probe) T)
  (schedule-next-probing probe)
  probe)

(defmethod probe-should-be-scheduled-p ((probe probe))
  (and 
   (probe-active probe)
   (or (null (probe-count-limit probe))
       (< (probe-count-limit probe)
	  (probe-call-count probe)))
   (or (null (probe-time-limit probe))
       (< (- (current-time probe)
	     (start-of probe))
	  (probe-time-limit probe)))
   ))
   
(defmethod schedule-next-probing ((probe probe))
  (if (not (probe-should-be-scheduled-p probe))
    (stop-probe probe)
    (schedule 
     (probe-frequency-limit probe)
     (incf (probe-call-count probe))
     (funcall (probe-function probe))
     (schedule-next-probing probe))))
    

	
	 
	 
	   

