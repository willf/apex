;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/agenda-support.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: agenda-support.lisp,v 1.15 2006/03/16 15:57:48 will Exp $


;;; Task agenda display support.

(in-package :common-lisp-user)

;;; Types:
;;;
;;; AgendaTree   :: (Time TaskDesc TaskDesc ...)
;;; TaskDesc     :: (Id Description State CreationTime EnabledTime
;;;                 Resources Monitors Subtasks)
;;;     NOTE: add pending, ongoing, terminated times
;;; Subtasks     :: AgendaTree
;;; Monitors     :: list(MonitorDesc)
;;; Resources    :: list (ResourceName)
;;; MonitorDesc  :: (Id Description)
;;; ResourceName :: symbol
;;; Resource     :: <instance of Resource class>
;;; Id           :: symbol
;;; State        :: symbol
;;; Description  :: list(<any type>)
;;; CreationTime :: Time
;;; EnabledTime  :: Time
;;; Time         :: int | NIL
;;;
;;;

(defun agenda-tree (agent)              ; agent -> AgendaTree
  (cons (time-format (current-time))
        (mapcar #'task-desc
                (remove-duplicates 
		 (append 
		  (top-level-tasks (tasks agent))
		  (completed-top-level-tasks agent))))))

(defun top-level-tasks (tasks)         ; list(task) -> list(task)
  (filter
   #'(lambda (task)
       (equal '(root) (description (parent task))))
   tasks))

(defun completed-top-level-tasks (agent)
  (let ((tasks ()))
    (timeseries-for-each 
     (agent-ae-memory agent)
     (lambda (event to)
       (declare (ignore to))
       (when (and (consp  event)
		  (eql (car event) 'terminated)
		  (typep (cadr event) 'task)
		  (equal '(root) (description (parent (cadr event))))
		  )
	 (push (cadr event) tasks))))
    (nreverse tasks)))
			     

(defun task-desc (task)			; Task -> TaskDesc
  (labels
      ((resource-name (x)		; Resource + ResourceName -> ResourceName
         (if (typep x 'id-mixin)
	   (id x)
           x)))
    (list (format nil "~a" (id task))
          (description task)
          (state task)
          (if (t-created task) (time-format (t-created task)))
          (if (t-started task) (time-format (t-started task)))
	  (time-in-state task)
	  ;; allocated resources
          (loop for i in (owners (resource-allocation-table (agent task)))
              when (eq (third i) task)
              collect (resource-name (car i)))
	  ;; monitor state
	  (monitor-state-desc task)
	  ;; subtasks
          (mapcar #'task-desc (order-by-task-id (children task)))
	  (last-timestamp-of-state task 'pending)
	  (last-timestamp-of-state task 'ongoing)
	  (if (t-terminated task) (time-format (t-terminated task))))))


(defun order-by-task-id (tasks)
  (sort  (copy-list tasks)
	 #'<
	 :key #'num))

(defun monitor-desc (mon)               ; Monitor -> MonitorDesc
  (list (id mon)
        (pattern mon)))

(defmethod time-in-state ((task task))  ; -> string
  (time-format
   (let ((t0 (t-started task)))
     (if t0
         (-  (current-time) t0)
       0))))

(defun monitor-state-desc (task)
  (cond ((monitors task)
	 (let ((ret-val
                (loop for monitor in (flatten (monitors task))
                    collect 
                      (list (string (id monitor))
                            (monitor-state monitor)
                            (monitor-summary monitor)))))
	   ret-val))
	(t
	 '())))

(defmethod task-history ((task task))
  (let ((mem (task-memory task)))
    (if (t-sv task)
      (sv-history mem (t-sv task))
      nil)))


(defmethod last-timestamp-of-state ((task task) (state symbol)) ; -> string
  (let ((h (task-history task)))
    (when h
      (sv-history-for-each-reverse 
       h +always-dur+ 
       (lambda (obs)
	 (when (eql (measurement-value obs) state)
	   (return-from last-timestamp-of-state
             (time-format (measurement-timestamp obs))))))))
  nil)
