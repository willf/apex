;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/monitor-testing-framework.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitor-testing-framework.lisp,v 1.8 2006/01/15 03:43:03 dalal Exp $

;;;
;;; The idea here is to make it easy to write unit tests for monitors.
;;;

;; (defmacro def-monitor-test (&body args) (declare (ignore args)))

(defvar *tree* NIL)

(procedure (index (test-monitor-noop)))

(defmacro def-monitor-test (name  &body forms)
  (if (find 'ignore forms :key #'car)
    `(progn)
    `(apex.utility.unit-test:with-tests (:name ,name)
       (let* ((*tree* (make-instance 
			  'red-black-tree :order-predicate #'< :key #'car))
	      (*application* (make-instance 'mon-application))
	      (*agent* (make-instance 'agent :name "Monitor testor"  :initial-task '(test-monitor-noop)))
	      (*this-task* (make-init-task '(root) nil *agent* t))
	      
	      (ats ',(collect-ats forms) )
	      (results ',(collect-m-results forms))
	      (ignore-errors-p (ignore-mon-errors-p ',forms)))
	 (progn ,@(collect-log-forms forms))
	 (progn ,@(collect-todos forms))
	 (setf (parent *this-task*) *this-task*)
	 (setf (t-created *this-task*) 
	   (duration-read ',(task-start-spec forms)))
	 (setf (t-started *this-task*) 
	   (duration-read ',(task-start-spec forms)))
	 ;; we make this a headed list, so we can destructively
	 ;; modify it in process-mon-test-node
	 (let ((monitors (list NIL)))
	   (dolist (at ats)
	     (tree-insert *tree*
			  (cons (duration-read (cadr at))
				(cddr at))))
	   (tree-for-each 
	    *tree* (lambda (node) (process-mon-test-node node monitors ignore-errors-p)))
	   (test-m-results results (cdr monitors)))))))

(defclass mon-application (application) 
  ((ctime :initarg :ctime :initform 0 :accessor mon-application-ctime)))

(defmethod schedule-event ((app mon-application) (time integer) thunk)
  (tree-insert *tree* `(,(+ (get-current-time app) time) (funcall ,thunk))))

(defmethod get-current-time ((app mon-application))
  (mon-application-ctime app))
  
(defun collect-ats (forms)
  (loop for form in forms 
      when (eql (car form) 'at)
      collect form))

(defun collect-todos (forms)
  (loop for form in forms
      when (eql (car form) 'eval)
      collect (cadr form)))

(defun task-start-spec (forms)
  (or (cadr (assoc 'task-start forms)) 'P0S))

(defun collect-m-results (forms)
  (loop for form in forms 
      when (eql (car form) 'result)
      collect form
      when (and (eql (car form) 'at)
		(consp (fourth form))
		(eql (car (fourth form)) 'result))
      collect (fourth form)))

(defun collect-log-forms (forms)
  (loop for form in forms 
      when (eql (car form) 'log)
      collect `(apply #'start-sv-logging-policy (agent-sv-memory *agent*) 
		      (make-state-variable ',(car (second form))
					   ',(cadr (second form)))
		      ',(cddr form))))

(defun ignore-mon-errors-p (forms)
  (find 'ignore-errors forms :key #'car))

(defun test-m-results (forms monitors)
  (dolist (form forms)
    (test-m-result form monitors)))

(defun test-m-result (form monitors)
  (let ((name (cadr form))
	(good? (third form))
	(time (when (fourth form) (duration-read (fourth form)))))
    (let ((mon (find-if (lambda (m) (eql (tag m) name)) monitors)))
      (unless mon (warn "Monitor with name ~A not in list" name))
      (when mon
	(if (eq good? :succeed)
	  (progn 
	    (apex.utility.unit-test:test 
	     t (not (null (val mon))) 
	     :if-success (format nil "Successful ~A ~a" (tag mon) (val mon))
	     :if-failure (format nil "Non-successful ~A" (tag mon)))
	    (when (not (null (val mon)))
	      (apex.utility.unit-test:test 
	       time (monitor-time-recognized mon)
	       :if-success 
	       (format 
		nil 
		"Successful ~a  at the right time (~a)" (tag mon) time)
	       :if-failure  
	       (format 
		nil 
		"Monitor ~a succeeded, but not at the right time (~a instead of ~a)"
		(tag mon) (monitor-time-recognized mon) time))))
	  (progn
	    (apex.utility.unit-test:test 
	     t (null (val mon)) 
	     :if-success (format nil "Non-completed ~s" (tag mon))
	     :if-failure (format nil "Should not have completed ~s at ~s" (tag mon) (monitor-time-recognized mon)))))))))

(defun process-mon-test-node (node monitors ignore-errors)
  (let ((form (node-data node)))
    ;; (format t "Processing node: ~S~%" form)
    (setf (mon-application-ctime *application*) (car form))
    (let ((todo (second form)))
      (case (car todo)
	(funcall (funcall (cadr todo)))
	(signal-event
	 (let ((cogevent (make-instance 'cogevent 
			   :content (eval (second todo))
			   :agent *agent*
			   :timestamp (mon-application-ctime *application*)
			   :attributes '())))
	   ;; (format t "Signalling ~s at ~s~%" (content cogevent) (timestamp cogevent))
	   (record-in-memory cogevent *agent*)
	   (dolist (mon (cdr monitors))
	     (unless (val mon)
	       (signal-test-monitor mon cogevent *this-task* *application* ignore-errors)))))
	(waitfor 
	 (let ((mon (apex.utility.unit-test:test-no-error 
		     (compile-monitor (second todo))
		     :if-success (format nil "Compiled ~s" (second (second todo))))))
	   (when mon
	     (setf (cdr monitors) (nconc (cdr monitors) (list mon)))
	     (signal-test-monitor mon :creation *this-task* *application* ignore-errors)
	     )))
	(compile-error
	 (let* ((pattern (second (second todo)))
		(mon (apex.utility.unit-test:test-error 
		      (compile-monitor pattern)
		      :if-error (format nil "This form should not compile: ~s" pattern)
		      :if-success (format nil "Error, as predicted: ~s" pattern)))
		)
	   (declare (ignore mon))
	     ))
	(t (warn "Don't know how to ~s" form))))))

(defun signal-test-monitor (mon signal task application ignore-errors)
  (if ignore-errors
    (multiple-value-bind (aval err)
	(ignore-errors
	 (let ((bpipe (signal-monitor mon task signal)))
	   (setf (val mon) (not (eq bpipe empty-pipe)))))
      (declare (ignore aval))
      (when err
	;; force TEST to see the error
	(apex.utility.unit-test:test-no-error
	 (error err)
	 :if-error
	 (format nil 
		 "Error signalling ~a with ~a"
		 (tag mon) (if (typep signal 'cogevent) (content signal) signal))))
      (when (val mon) 
	(setf (monitor-time-recognized mon) 
	  (mon-application-ctime application))))
    (progn
      (let ((bpipe (signal-monitor mon task signal)))
	   (setf (val mon) (not (eq bpipe empty-pipe))))
      (when (val mon) 
	(setf (monitor-time-recognized mon) 
	  (mon-application-ctime application))))))
