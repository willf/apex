;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors-functional.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors-functional.lisp,v 1.2 2006/01/15 03:43:00 dalal Exp $
;;; Created:        March, 2005

(defclass functional-monitor (non-complex-monitor)
  ())

;; (and (= 3 ?x)

(defmethod create-monitor-from-pattern ((type (eql :function)) expr parameters pattern)
  (declare (ignore expr parameters pattern))
  (make-instance 'functional-monitor :relevant-types `(:any)))


(defmethod signal-monitor/1 ((monitor functional-monitor) (task task) (cogevent t) &optional (added-bindings no-bindings))
(let* ((bindings/s (make-bindings-stack added-bindings (get-local-context task) (task-globals task)))
       (expr (substitute-bindings-stack (monitor-expr monitor) bindings/s)))
  (if (contains-variable-p expr)
    empty-pipe
    (multiple-value-bind (val err)
	(eval expr)
      (if err 
	(progn
	  (warn "Function monitor ~s threw an error. Ignoring." expr)
	  no-bindings)
	(if (not val)
	  empty-pipe
	  (list
	   (make-monitor-result  
	    no-bindings
	    (make-interval (if (typep cogevent 'cogevent) 
			     (timestamp cogevent)
			     (current-time)))
	    monitor))))))))








    