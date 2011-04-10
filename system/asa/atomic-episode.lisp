;;;-*- Mode: Lisp; Package:  :apex.asa.ae -*-
;;;
;;; apex/system/asa/atomic-episode.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: atomic-episode.lisp,v 1.6 2006/01/15 03:43:00 dalal Exp $
;;; Created:        August. 2004

;; Code for state variables and measurements ...

(defpackage :apex.asa.ae
  (:use :common-lisp)
  (:use :apex.utility.datetime)
  (:use :apex.utility.timeseries)
  (:use :inet.util.process)
  (:use :apex.utility.patmatch)
  (:use :apex.utility.pipe)
  (:use :apex.utility.fp)
  (:export
   #:atomic-episode-memory
   #:atomic-episode-history
   #:make-ae-memory
   #:record-atomic-episode
   #:ae-memory-for-each
   #:ae-memory-map
   #:ae-agent
   #:query-atomic-episode-memory
   )
  )

(in-package :apex.asa.ae)

(defclass atomic-episode-memory (timeseries cl-user::appob)
  ((agent :initform NIL :initarg :agent :accessor ae-agent)))

(defmethod cl-user::appob-parent ((mem atomic-episode-memory))
  (ae-agent mem))

(defmethod make-ae-memory (&optional agent)
  (make-instance 'atomic-episode-memory :name "Atomic Episode Memory" :agent agent))

(defmethod record-atomic-episode ((mem atomic-episode-memory) form timestamp)
  (timeseries-insert mem
		     timestamp
		     form))

(defmethod ae-memory-for-each ((mem atomic-episode-memory) (interval interval) (function function))
  (multiple-value-bind (found index)
      (first-value-within mem interval)
    (let ((last-index (find-index-floor mem (interval-end interval))))
      (when (and index last-index)
	(funcall function found (timepoint-at mem index))
	(do ((index (1+ index) (1+ index)))
	    ((or (>= index (ts-item-count mem))
		 (>  index last-index)))
	  
	  (funcall function 
		   (value-entry-at mem index)
		   (timepoint-at mem index)))))))

(defmethod ae-memory-map ((mem atomic-episode-memory) (interval interval) (function function))
  (let ((results (list)))
    (ae-memory-for-each 
     mem 
     interval 
     (lambda (val timepoint)
       (push (funcall function val timepoint)
	     results)))
    (nreverse results)))

;; query -- form
;; bindings/s -- stack of binding sets
(defmethod query-atomic-episode-memory ((mem atomic-episode-memory) (interval interval) query &optional (bindings/s nil))
  (let ((query (substitute-bindings-stack query bindings/s)))
    (filter-pipe 
     #'identity
     (ae-memory-map mem 
		    interval
		    (lambda (atomic-episode timepoint)
		      ;; (cl-user::pvs! :newline ";;;" query :newline ";;;" atomic-episode)
		      (let ((newbinds  (pat-match query atomic-episode no-bindings)))
			(if (not newbinds)
			  nil
			  (list newbinds timepoint))))))))
			 
			 
   


;; end of file

