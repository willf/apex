;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors-timepoint.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors-timepoint.lisp,v 1.4 2006/01/15 03:43:00 dalal Exp $


(in-package :cl-user)


(defclass timepoint-monitor (measurement-monitor)
  ((delay-form :initarg :delay-form :initform NIL :accessor delay-form)
   (timepoint :initarg :timepoint :initform nil :accessor timepoint)))


;;; (:delay [<tag>] <start> [<end>])
;;; (:timestamp  [<tag>] <start> [<end>])

(defmethod add-start-timepoint-constraint ((mon timepoint-monitor) const)
  (let ((tp const)) ;; (timepoint-read const)))
    (setf (timepoint mon) tp)
    (setf (timestamp-constraints mon)
      (append (timestamp-constraints mon)
	      `((>= ,tp))))))

(defmethod add-end-timepoint-constraint ((mon timepoint-monitor) const)
  (setf (timestamp-constraints mon)
    (append (timestamp-constraints mon)
     `((<=  ,const)))))

(defmethod add-start-duration-constraint ((mon timepoint-monitor) const)
  (let ((tp (+ (start-of mon) (duration-read const))))
    (setf (timepoint mon) tp)
    (setf (timestamp-constraints mon)
      (append (timestamp-constraints mon)
	      `((>= ,tp))))))

(defmethod add-end-duration-constraint ((mon timepoint-monitor) const)
  (setf (timestamp-constraints mon)
    (append (timestamp-constraints mon)
     `((<= (+ (start-of ,mon) ,const))))))


(defun duration-spec-for-delay-p (x)
  (if (symbolp x)
    (multiple-value-bind (v e)
	(ignore-errors (duration-read x))
      (declare (ignore v))
      (if e nil t))
    nil))

(defun timepoint-spec-for-time-p (x)
  (if (symbolp x)
    (multiple-value-bind (v e)
	(ignore-errors (timepoint-read x))
      (declare (ignore v))
      (if e nil t))
    (if (consp x) t nil)))

(defmethod special-monitor-pattern-p ((tag (eql :delay)))
  t)

;;; argh -- ambiguity of tag 
(defmethod compile-special-monitor-pattern ((tag (eql :delay)) pattern)
  (let ((monitor (make-instance 'timepoint-monitor :tag (gentemp "DELAY-")))) ;; tag may be overwritten -- tag
    (let* ((params (cdr pattern))
	   (len (length params)))
      (cond
       ((= len 0)
	(error "DELAY must have at least one delay specification: ~a"
	       pattern))
       ((= len 1)
	(unless (duration-spec-for-delay-p (car params))
	  (error "DELAY must have at least one delay specification: ~a in addition to tag ~a"
		 pattern (car params)))
	;; otherwise, ok
	(add-start-duration-constraint monitor (car params))
	(setf (delay-form monitor) (car params)))
       ((= len 2)
	(if (not (duration-spec-for-delay-p (car params)))
	  ;; tag + constraint
	  (progn
	    (setf (tag monitor) (car params))
	    (add-start-duration-constraint monitor (second params)))
	  ;;; two constraints
	  (progn
	    (setf (delay-form monitor) (car params))
	    (add-start-duration-constraint monitor (car params))
	    (add-end-duration-constraint monitor (second params)))))
       ((= len 3)
	(setf (tag monitor) (car params))
	(setf (delay-form monitor) (second params))
	(add-start-duration-constraint monitor (second params))
	(add-end-duration-constraint monitor (third params)))
       (t 
	(error "Too many parameter in delay: ~a" pattern))))
    
    monitor))

(defmethod compile-monitor-post-process ((monitor timepoint-monitor))
  ;; this is probably the *only* relevant type ...
  (setf (relevant-types monitor)
    (append (relevant-types monitor) (list 'tick)))
  ;; schedule a wakeup
  (let ((s-when (evaluate-constraint (delay-form monitor) nil)))
  (schedule s-when
	    ;; (format t ";;; TICKTOCK ~a~%" `(tick clock = ,(current-time)))
	    (cogevent `(tick clock = ,(current-time))
		      (agent (task monitor))
		      :trigger-asa t))))
;;;
;;; timepoint monitors ...
;;; 

(defmethod special-monitor-pattern-p ((tag (eql :timestamp)))
  t)

(defun teval (x)
  (eval (convert-duration-specs x)))

(defmethod compile-special-monitor-pattern ((tag (eql :timestamp)) pattern)
  (let ((monitor (make-instance 'timepoint-monitor :tag (gentemp "TIME-"))))
    (let* ((params (cdr pattern))
	   (len (length params)))
      (cond
       ((= len 0)
	(error "TIME must have at least one time specification: ~a"
	       pattern))
       ((= len 1)
	(unless (timepoint-spec-for-time-p (car params))
	  (error "TIME must have at least one time specification: ~a in addition to tag ~a"
		 pattern (car params)))
	;; otherwise, ok
	(setf (delay-form monitor) `(,(- (teval (car params)) (current-time))
				     ms))
	(add-start-timepoint-constraint monitor (car params)))
       ((= len 2)
	(if (not (timepoint-spec-for-time-p (car params)))
	  ;; tag + constraint
	  (progn
	    (setf (tag monitor) (car params))
	    (setf (delay-form monitor) `(,(- (teval (second params))
					     (current-time))
				     ms))
	    (add-start-timepoint-constraint monitor (second params)))
	  ;;; two constraints
	  (progn
	    (setf (delay-form monitor) `(,(- (teval (car params))
					 (current-time))
				     ms))
	    (add-start-timepoint-constraint monitor (car params))
	    (add-end-timepoint-constraint monitor (second params)))))
       ((= len 3)
	(setf (tag monitor) (car params))
	(setf (delay-form monitor) `(,(- (teval (car params))
					 (current-time))
				     ms))
	(add-start-timepoint-constraint monitor (second params))
	(add-end-timepoint-constraint monitor (third params)))
       (t 
	(error "Too many parameter in time: ~a" pattern))))
    
    monitor))


(defmethod signal-monitor/1 ((monitor timepoint-monitor) (task task) cogevent &optional (added-bindings no-bindings))
  (if (and (typep cogevent 'cogevent)
	   (not (relevant-event-type-p monitor (first (content cogevent)))))
    empty-pipe
    (let ((bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
	  (ts (if (typep cogevent 'cogevent)
		(timestamp cogevent)
		(current-time))))
      (results-from-query monitor task `(tick clock = ,(timepoint monitor)) ts bindings/s))))