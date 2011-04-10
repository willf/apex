;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/indexicals.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: indexical-variables.lisp,v 1.1 2006/03/13 19:22:37 will Exp $
;;; Created:        September 2004


;;; Adds the 'indexical-functional' variable type, and 
;;; defines standard indexical functions.


(in-package :cl-user)

;;; ------ Indexical functional variables support
;;; Functions attached to Indexical functional variables need to refer to 'this task' 
;;; and so we need to define a dynamic variable to allow this.
;;; Just five easy steps ...

(eval-when (:execute :load-toplevel :compile-toplevel)
  (add-variable-type :indexical-functional))


(defmethod convert-variable (from-var (to-type (eql :indexical-functional)))  
  (intern (concatenate 'string "+" (string-upcase (symbol-name (variable-name from-var)))
		       "+")))

(defmethod is-variable-p ((symbol-name string) (len integer) (type (eql :indexical-functional)))
  ;;; "+A+"
  (and 
   (>= len 3)
   (char= (char symbol-name  0) #\+)
   (char= (char symbol-name  (1- len)) #\+)))


(defmethod variable-type-of ((char (eql #\+)))
  :indexical-functional)

(defmethod get-binding-dispatch ((type (eql :indexical-functional)) var bindings)
  (declare (ignore bindings))
  (let ((n (variable-name var)))
    (assert (fboundp n) () "Indexical-functional variable ~S has no associated function." n)
    (let ((val (funcall (symbol-function n) *this-task*)))
      (if val
	(make-binding var val)
	NIL))))


(defmethod match-variable-dispatch ((type (eql :indexical-functional)) var input bindings)
  (let ((binding (get-binding var bindings))) ;; should always return a value
    (if (equal (binding-val binding) input)
	bindings fail)))

;;; -----------------------------------------------------------
;;;
;;; Standard indexical functions. Note that they require a TASK
;;; object (which will be the current task).
;;;
;;; -----------------------------------------------------------

(defun now (task) 
  (declare (ignore task))
  (current-time))

(defun this-task (task) task)

(defun self (task)
  (if task (agent task) 
      *agent*))

(defun tot (task)  ;; time on task
  (if task 
    (- (current-time) (t-started task))
    0))

;; we'll fix this eventually...
(defun met (&optional task) ;; mission elapsed time
  (declare (ignore task))
  (if *application* 
    (- (current-time) (start-of *application*))
    (current-met)))


;;; -- other functions needed -- for constraints, etc.

(defun type= (x type) (eql (type-of x) type))

(defun in (a &rest bs)
  (and (member a bs) t))


