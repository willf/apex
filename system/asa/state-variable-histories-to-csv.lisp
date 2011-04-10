;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/state-variable-histories-to-csv.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: state-variable-histories-to-csv.lisp,v 1.8 2006/01/15 03:43:00 dalal Exp $


(in-package :cl-user)

;;; --- EXCEL (comma separated value) output
;;;
;;; The main function is:
;;;
;;; AGENT-SV-MEMORY-TO-CSV ((agent agent) filename &optional include-task-svs)
;;; 
;;; writes, to FILENAME, comma-separated values of state variables in AGENT's
;;; state variable memory. If INCLUDE-TASK-SVS is non-null, then task
;;; state variables are include, too. Otherwise, they're not.
;;;
;;; returns the pathname or NIL if there are no data rows.
;;;
;;;
;;; AGENT-SV-MEMORY-TO-CSV-STRING ((agent agent) &optional include-task-svs)
;;; 
;;; just like the above, only puts values to a string. 
;;; returns the string or NIL if there are no data rows.
;;;
;;; ----------------------------------------------------------------------------
;;; The basic idea is to allow outputting of state variable
;;; histories of an agent to something that Excel (and other programs)
;;; can read.
;;;
;;; The output will be something like this:
;;;
;;; TIME  sv-1   sv-2 ... sv-n
;;; t1    v11    v21  ... sv-n1
;;; t2    v12    v22  ... sv-n2
;;; ...
;;; tm    v1m    v2m  ... sv-nm
;;; 
;;; where v_{ij} is a value for sv_i at t_j, or 'blank'
;;; if there was no signal at that point. 
;;; 
;;; Fields are separated by commas. If a value contains a comma, newline
;;; or quotation mark ("), the the field is surrouned by quotation
;;; marks. Quotation marks within fields are surrounded by
;;; quotation marks.
;;;
;;; This, my friend, is a "test," so called ->
;;; "This, my friend, is a """test,""" so called."
;;;
;;; 23 -> 23
;;; enabled -> enabled
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Note that it's possible that two or more measurements for a 
;;; state variable might occur at the same timepoint (this frequently
;;; happens for task state variables, for instance). In this case, only
;;; the last value is used.
;;; 
;;; Processing:
;;;  - Run through all of the measurements of all sv histories and
;;;    store them by timestamp in a red-black tree.
;;;  - Pull them out by distinct timestamp; this is a 'row.'
;;;  - Go thru the rows and print them.
;;;  
;;;  It's likely that we could do this with lower memory requirements.
;;;
;;; 

(defvar *include-task-state-variables-in-output* T
  "Should we include task state variable histories in the output?")

(defun make-meas-tree ()
  "Create a red-black tree of stored measurements"
  (make-instance 'red-black-tree 
    :order-predicate #'<
    :key #'measurement-timestamp
    :equal-predicate #'=))

(defun task-state-variable-p (sv)
  (or
   (and (eql (sv-attribute sv) 'state)
	(typep (sv-object sv) 'task))
   (and (eql (sv-attribute sv) 'tick)
	(eql (sv-object sv) 'clock))))

(defun use-sv-history-for-csv-p (sv-history)
  (declare (special *include-task-state-variables-in-output*))
  (or  *include-task-state-variables-in-output*
       (not (task-state-variable-p (sv-history-sv sv-history)))))


(defmethod rb-meas-tree/svs ((agent agent))
  "Return a red-back tree of measurements and list of state variables of an agent"
  (let* ((mem (agent-sv-memory agent))
	(tr (make-meas-tree))
	(histories (filter 'use-sv-history-for-csv-p (sv-histories mem))))
    (dolist (sv-history histories)
      (timeseries-for-each 
       sv-history
       (lambda (measurement timestamp)
	 (tree-insert tr (make-measurement (sv-history-sv sv-history) measurement timestamp) ))))
    (values tr (mapcar #'sv-history-sv histories))))

(defun rb-meas-tree/svs->rows (tr svs)
  "Convert red-black tree + state variable list to a list of data rows"
  (let ((len (length svs)))
    (flet ((index (sv)
	     (1+ (position sv svs))))
      (let* ((row nil)
	     (rows (list)))
	(tree-for-each 
	 tr
	 (lambda (node)
	   (let ((measurement (node-data node)))
	     (when (or (null row) 
		       (/= (measurement-timestamp measurement)
			   (elt row 0)))
	       (setq row (make-array  (1+ len) :initial-element nil))
	       (setq rows (cons row rows))
	       (setf (elt row 0) (measurement-timestamp measurement)))
	     (setf (elt row (index (measurement-sv measurement)))
	       (measurement-value measurement)))))
	(values (reverse rows)
		tr)))))

(defun sv-header-row (svs)
  "Create a header row"
  (let ((row (make-array  (1+ (length svs)))))
    (setf (elt row 0) "Time")
    (loop for sv in svs
	for i from 1
	do
	  (setf (elt row i)
	    (format nil "(~A ~A)"
		    (sv-attribute sv)
		    (if (typep (sv-object sv) 'id-mixin)
		      (id (sv-object sv))
		      (sv-object sv)))))
    row))

(defmethod sv-data-rows ((agent agent))
  "return a list of rows of header + data rows of an agent for state variable histories,
   ordered by timestamp"
  (multiple-value-bind (tr svs)
      (rb-meas-tree/svs agent)
    (cons (sv-header-row svs)
	  (rb-meas-tree/svs->rows tr svs))))

      
(defmethod agent-sv-memory-to-csv ((agent agent) pathname &optional include-task-svs)
  "Send CSV values of state variable memory to a file, optionally including task state variables"
  (let ((*include-task-state-variables-in-output* include-task-svs))
    (declare (special  *include-task-state-variables-in-output*))
    (let ((rows (sv-data-rows agent)))
      (if (cdr rows) ;; i.e., at least one data row
	(progn 
	  (write-csv-file pathname rows)
	  pathname)
	nil))))


(defmethod agent-sv-memory-to-csv-string ((agent agent)  &optional include-task-svs)
  "Send CSV values of state variable memory to a string, optionally including task state variables"
  (let ((*include-task-state-variables-in-output* include-task-svs))
    (declare (special  *include-task-state-variables-in-output*))
    (let ((rows (sv-data-rows agent)))
      (if (cdr rows) ;; i.e., at least one data row
	(with-output-to-string (str)
	  (write-csv-records str #\, rows))
	nil))))


;;;
;;; This is some support for converting task state variables into dot graphics
;;;

<<<<<<< state-variable-histories-to-csv.lisp
(defun state-value->color (value)
 (case value
   (pending 'palegoldenrod)
   (engaged 'darkseagreen)
   (enabled 'darkseagreen)
   (ongoing 'darkseagreen)
   (terminated 'salmon)
   (suspended 'salmon)
   (resumed 'darkseagreen)
   (t 'grey)))

(defun state-value->label (value task)
 (case value
   (pending (symbol-name (id task)))
   (engaged "engaged")
   (enabled "enabled")
   (ongoing "ongoing")
   (terminated "terminated")
   (suspended "suspended")
   (resumed "resumed")
   (t "")))
	    
(defmethod task-state-to-dot ((agent agent) stream)
 (flet ((id-num (id)
	   (read-from-string (subseq (symbol-name id)  (position-if #'digit-char-p (symbol-name id))))))
   (let ((histories (sort (filter (lambda (history)
				  (typep (sv-object (sv-history-sv history)) 'task))
				(cdr (assoc 'state (sv-memory-table (agent-sv-memory agent)))))
			#'<
			:key (lambda (x)
			       (id-num (id (sv-object (sv-history-sv x)))))))
	  (always (make-interval 0 +end-of-time+)))
     (dolist (h histories)
	(sv-history-for-each 
	 h always 
	 (let ((last-state nil)
	       (states (list)))
	   (lambda (m)
	     (let* ((v (measurement-value m))
		   (task (sv-object (measurement-sv m)))
		    (idn (id-num (id task))))
	       (unless (member v states)
		 (format stream "task_~a_~a [color=~a label=~s];~%" 
			 idn v (state-value->color v)
			 (state-value->label v task))
		 (setq states (cons v states)))
	   (when last-state
	     (format stream "task_~a_~a -> task_~a_~a;~%" idn last-state idn v))
	   (setq last-state (measurement-value m))))))))))
	=======
;;;(defun state-value->color (value)
;;;  (case value
;;;    (pending 'palegoldenrod)
;;;    (engaged 'darkseagreen)
;;;    (enabled 'darkseagreen)
;;;    (ongoing 'darkseagreen)
;;;    (terminated 'salmon)
;;;    (suspended 'salmon)
;;;    (resumed 'darkseagreen)
;;;    (t 'grey)))
;;;
;;;(defun state-value->label (value task)
;;;  (case value
;;;    (pending (symbol-name (id task)))
;;;    (engaged "engaged")
;;;    (enabled "enabled")
;;;    (ongoing "ongoing")
;;;    (terminated "terminated")
;;;    (suspended "suspended")
;;;    (resumed "resumed")
;;;    (t "")))
;;;	    
;;;(defmethod task-state-to-dot ((agent agent) stream)
;;;  (flet ((id-num (id)
;;;	   (read-from-string (subseq (symbol-name id)  (position-if #'digit-char-p (symbol-name id))))))
;;;    (let ((histories (sort (filter (lambda (history)
;;;				  (typep (sv-object (sv-history-sv history)) 'task))
;;;				(cdr (assoc 'state (sv-memory-table (agent-sv-memory agent)))))
;;;			#'<
;;;			:key (lambda (x)
;;;			       (id-num (id (sv-object (sv-history-sv x)))))))
;;;	  (always (make-interval 0 +end-of-time+)))
;;;      (dolist (h histories)
;;;	(sv-history-for-each 
;;;	 h always 
;;;	 (let ((last-state nil)
;;;	       (states (list)))
;;;	   (lambda (m)
;;;	     (let* ((v (measurement-value m))
;;;		   (task (sv-object (measurement-sv m)))
;;;		    (idn (id-num (id task))))
;;;	       (unless (member v states)
;;;		 (format stream "task_~a_~a [color=~a label=~s];~%" 
;;;			 idn v (state-value->color v)
;;;			 (state-value->label v task))
;;;		 (setq states (cons v states)))
;;;	   (when last-state
;;;	     (format stream "task_~a_~a -> task_~a_~a;~%" idn last-state idn v))
;;;	   (setq last-state (measurement-value m))))))))))


>>>>>>> 1.4
