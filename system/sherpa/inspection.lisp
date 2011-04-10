;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/sherpa/inspection.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: inspection.lisp,v 1.12 2006/01/15 03:43:02 dalal Exp $


(in-package :common-lisp-user)

(defvar $ nil)
(defvar $$ nil)
(defvar $$$ nil)

(defun update$ (obj)
  (unless (equal obj $)
    (setf $$$ $$)
    (setf $$ $)
    (setf $ obj)))

;;; ! let inspect take instance (not instance name) as arg

(defun inspect-internal (object-id terse hide-null stream)
  (let ((object (if (symbolp object-id) (find-instance object-id))))
    (cond 
     (object
      (inspect-apex-object object terse hide-null)
      (update$ object))  ;; allows "manual" inspect via Lisp listener
     ((and (consp object-id) (= 2 (length object-id)))
      (let ((object (find-instance (first object-id)))
	    (slot (second object-id)))
	(cond 
         ((and (equal (first object-id) '$slot) 
	       (slot-exists-p $ slot))
	  ;; if wanting to inspect slot of current inspect "focus"
	  (inspect-apex-slot $ slot stream))
	 ((and object (slot-exists-p object slot)
	       (slot-boundp object slot))
	  (inspect-apex-slot object slot stream)
	  (update$ object))
	 (t 
	  (format stream "[object not found]~%")))))))
    (values))

(defmethod inspect-apex-object ((obj id-mixin) terse hide-null-values &optional (stream t))
  (let* ((allslots (class-slot-names obj))
	 (slots
          (sort (or (and terse (accumulate-terse-slots obj)) allslots)
                (lambda (s1 s2) (string-lessp (symbol-name s1) (symbol-name s2)))))
	 (hidden nil)
	 (unbound nil))
    (dolist (slot slots)
      (let ((value (if (slot-boundp obj slot) (slot-value obj slot) 'unbound)))
	(cond ((and hide-null-values (null value))
	       (push slot hidden))
	      ((and hide-null-values (eql value 'unbound))
	       (push slot unbound))
	      (t
	       (show-inspected-slot obj slot value stream)))))
    (if hide-null-values (show-null-list hidden unbound stream))
    ;;(if terse (show-suppressed-list (set-difference allslots slots) stream))
    (values)))

  
  
;;; assumes slot exists and is bound for object
(defun inspect-apex-slot (obj slot &optional (stream t))
  (cond ((equal slot 'agenda)
	 (inspect-agenda obj))
	((equal slot 'monitor-array)
	 (inspect-monitors-list obj))
	((equal slot 'resource-allocation-table)
	 (inspect-resource-allocation-table obj))
	(t
	 (show-inspected-slot-unabridged obj slot (slot-value obj slot) 
					 stream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Different for MCL and ACL

#+CCL ;really MCL
(defmethod class-slot-names ((class-name symbol))
  (let ((ans)
	(slot-struct))
    (when (find-class class-name)
      (setf slot-struct (cdr (ccl::%class-slots (find-class class-name))))
      (dotimes (i (length slot-struct))
	(push (first (aref slot-struct i)) ans)))
    (reverse ans)))

#+CCL ;really MCL
(defmethod class-slot-names ((instance id-mixin))
  (class-slot-names (type-of instance)))

#+Allegro
(defmethod class-slot-names (class-name)  ;; of type class
  (and (find-class class-name nil) ;;checks if actually name of a class
       (mapcar #'clos:slot-definition-name
		   (clos:class-slots (find-class class-name)))))

#+Allegro
(defmethod class-slot-names ((instance standard-object))
  (mapcar #'clos:slot-definition-name
	  (clos:class-slots (class-of instance))))

#+Clisp
(defmethod class-slot-names ((instance standard-object))
  (clos::slot-names instance))

#+Clisp
(defmethod class-slot-names ((class-name symbol))
  ;; ! A bit of a hack; could 
  (class-slot-names (make-instance class-name)))


;;; max is the maximum list length of value to be shown
(defmethod show-inspected-slot (obj slot value &optional (stream t))
  (declare (ignore obj))
  (let ((temp1 *print-level*) (temp2 *print-length*))
    (setf *print-length* 10)
    (setf *print-level* 3)
    (format stream "~a: ~a~%" slot value)
    (setf *print-level* temp1)
    (setf *print-length* temp2))
  (values))

(defmethod show-inspected-slot-unabridged (obj slot value &optional (stream t))
  (declare (ignore obj))
  (format stream "~a: ~a~%" slot value)
  (values))

(defmethod show-null-list (nullslots unboundslots &optional (stream t))
  (if nullslots
      (format stream "~%The following slots had null values:~%~a~%" 
	      nullslots))
  (if unboundslots
      (format stream "~%The following slots were unbound:~%~a~%" 
	      unboundslots)))

(defmethod show-suppressed-list (verbose-slots &optional (stream t))
  (format stream "~%The following slots are not displayed in terse mode:~%~a~%"
	  verbose-slots))


;;; Terse display mode support

(defparameter *always-suppressed-slots*
    '(suppressed-slots parent-slot component-slots content-slots))

(defmethod accumulate-terse-slots ((obj id-mixin)) ; -> list(symbol)
  (accumulate-terse-slots (find-class (type-of obj))))

(defmethod accumulate-terse-slots ((class standard-object)) ; -> list(symbol)
  ;; Return list of all slots to *show* when in terse display mode.
  (set-difference
   (remove-duplicates ; covers "diamond" shaped class hierarchies
    (append
     (set-difference
      (mapcar #'mop:slot-definition-name
              (mop:class-direct-slots class))
      (find-suppressed-slots class))
     (remove nil ; covers case when ALL of a class's slots are suppressed
             (flatten
              (mapcar #'accumulate-terse-slots
                      (inspectable-direct-superclasses class))))))
   *always-suppressed-slots*))

(defmethod find-suppressed-slots ((class standard-object))
  ;; -> list(symbol)
  ;; Return list of suppressed slots in class definition.
  ;; Equivalent to (suppressed-slots (make-instance class-name))
  (flatten
   (loop for slot in (mop:class-direct-slots class)
       when (eq 'suppressed-slots (mop:slot-definition-name slot))
       collect (eval (mop:slot-definition-initform slot)))))

(defmethod inspectable-direct-superclasses ((class standard-object))
  ;; -> list(standard-object)
  ;; Return list of immediate superclasses subject to inspection.
  (remove-if
   ;; standard-object and higher not interesting
   (lambda (class) (eq 'standard-object (class-name class)))
   (mop:class-direct-superclasses class)))

;;; !! write routines to show selected attributes, not objects

(defun inspect-agenda (agent)
  (format t "Agenda for ~a  [~a tasks]~%" agent
          (length (tasks agent)))
  (dolist (a (tasks agent))
    (format t "~a  ~a~%" (state a) (specify-task-description a))))

;;; ! may want to show both specified and unspecified form
;;; ! need to show logical structure and connection to tasks

(defun inspect-monitors-list (agent)
  (format t "Monitors list for ~a [~a monitors]~%" 
	  agent (length (monitors agent)))
  (dolist (m (monitors agent))
    (format t "~a  ~a for ~a~%" (val m) m (id (task m)))))

(defun inspect-resource-allocation-table (agent)
  (format t "   Resource Allocation Table~%")
  (format t "-------------------------------~%")
  (format t "Current allocations:~%")
  (dolist (alloc (owners (resource-allocation-table agent)))
    (format t "~a allocated to ~a~%" (first alloc) (third alloc)))
  (format t "-------------------~%Awaiting allocation:~%")
  (maphash #'(lambda (key monitors)
               (declare (ignore key))
	       (dolist (mon monitors)
		 (format t "~a needed by ~a~%" (rest (expr mon)) (task mon))))
	   (monitors (resource-allocation-table agent)))
  (terpri))

;;; --- For debugging

(defun show-a ()
  (inspect-agenda *agent*))

(defun show-m ()
  (inspect-monitors-list *agent*))
