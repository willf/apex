;;;-*- Mode: Lisp; Package:  :apex.utility.era -*-
;;;
;;; apex/system/utility/era.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: era.lisp,v 1.6 2006/01/15 03:43:02 dalal Exp $
;;; Created:        June, 2004


(defpackage :apex.utility.era
  (:use :common-lisp)
  (:use :apex.utility.datetime)
  (:use :apex.utility.patmatch)
  (:use :inet.util.process)
  (:export #:contravene-p
	   #:*ignore-contravention*
	   #:recognizer-tell
	   #:event-pattern-recognizer
	   #:sequential-event-pattern-recognizer
	   #:all-event-pattern-recognizer	   
	   #:evnt
	   #:make-event
	   #:event-interval
	   #:event-form
	   #:recognizer-pattern
	   #:recognizer-started
	   #:recognizer-ended
	   #:recognizer-bindings
	   #:recognizer-status
	   #:recognizer-reason
	   #:compile-event-pattern
	   #:*recognizer-queue*
	   #:recognizer-queue
	   #:recognizer-queue-recognizers
	   #:recognizer-queue-lock
	   #:recognizer-queue-entry
	   #:recognizer-queue-entry-recognizer
	   #:recognizer-queue-entry-pattern
	   #:recognizer-queue-entry-restart-on-active
	   #:recognizer-queue-entry-restart-on-complete
	   #:recognizer-queue-entry-restart-on-futile
	   #:recognizer-queue-entry-restart-on-rebinding
	   #:recognizer-queue-entry-callback
	   #:recognizer-queue-entry-lease
	   #:add-recognizer
	   #:process-recognizer-queue
	   #:*event-queue*
	   #:event-queue
	   #:event-queue-events
	   #:event-queue-lock
	   #:event-queue-process
	   #:event-queue-count
	   #:event-queue-max-count
	   #:enqueue-event
	   #:start-event-server
	   #:stop-event-server
	   #:wait-for-event-pattern
	   #:era-syntax-word-p
   )
  )

(in-package :apex.utility.era)

(defclass evnt ()
  ((form :accessor event-form :initarg :form :initform NIL)
   (interval :accessor event-interval :initarg :interval :initform NIL)))

(defmethod print-object ((evnt evnt) (stream stream))
  (print-unreadable-object  (evnt stream :type t :identity t)
    (with-slots (form interval) evnt
      (princ form stream)
      (if interval
	(format stream " ~A" interval)
	(princ ": <interval-undefined>" stream)))))

(defun make-event (form start &optional end)
  (make-instance 'evnt :form form :interval (make-interval start end)))

(defvar *recognizer-range* '(:ignore :active :futile :complete)
  "Possible return state values of recognizers.")

;;;---
;;; Utility functions.
;;;---

;; 2 event forms contravene iff:
;; they are lists,
;; they are tree-equal *except* in the 'rightmost' 'last' position
;; 

(defvar *ignore-contravention* NIL
  "Set to T to ignore contravention checks.")

(defun contravene-p (event-form-1 event-form-2)
  (if *ignore-contravention* NIL
    (if (not (and (consp event-form-1)
		  (consp event-form-2)))
	nil
      (let ((l1 (length event-form-1))
	    (l2 (length event-form-2)))
	(if (/= l1 l2)
	    nil
	  (let ((e1 (elt event-form-1 (1- l1)))
		(e2 (elt event-form-2 (1- l2))))
	    (if (and (atom e1) (atom e2) (not (eql e1 e2)))
		(if (tree-equal (butlast event-form-1)
				(butlast event-form-2))
		    T
		  NIl)
	      (contravene-p e1 e2))))))))

	   
(defclass event-pattern-recognizer ()
  ((pattern  :initarg :pattern :initform '() :accessor recognizer-pattern)
   (seen :initarg :seen :initform (list) :accessor seen-set)
   (remainder :initarg :remainder :initform NIL :accessor remainder-list)
   (seen-in-order :initarg :seen-in-order :initform (list) :accessor seen-in-order)   
   (started  :initarg :started :initform NIL :accessor recognizer-started)
   (ended    :initarg :ended :initform NIL :accessor recognizer-ended)
   (bindings :initarg :binding-set :initform no-bindings :accessor recognizer-bindings)
   (status   :initarg :status      :initform :runnable   :accessor recognizer-status)
   (reason   :initarg :reason      :initform :ok         :accessor recognizer-reason)
   (interval :initarg :interval    :initform NIL         :accessor recognizer-interval)))

(defmethod event-pattern-recognizer-p ((epr T))
  nil)

(defmethod event-pattern-recognizer-p ((epr event-pattern-recognizer))
  T)

;;; This simple looking method is very important -- it allows the methods below
;;; to update a lot of state at once, AND return the status of the recognizer.
;;; it's helpful in minimizing the coding of what are already methods that are
;;; too long. 

(defmethod update-status ((epr event-pattern-recognizer)
			  &key (seen nil sp)
			       (seen-in-order nil sipp)
			       (remainder nil rp)
			       (started nil sap)
			       (ended nil ep)
			       (bindings nil bp)
			       (status nil stp)
			       (reason nil rep)
			       (interval nil igp))
  (when sp (setf (seen-set epr) seen))
  (when sipp (setf (seen-in-order epr) seen-in-order))
  (when rp (setf (remainder-list epr) remainder))
  (when sap (setf (recognizer-started epr) started))
  (when ep (setf (recognizer-ended epr) ended))
  (when bp (setf (recognizer-bindings epr) bindings))
  (when stp (setf (recognizer-status epr) status))
  (when rep (setf (recognizer-reason epr) reason))
  (when igp (setf (recognizer-interval epr) interval))
  (recognizer-status epr))


;;; this is the main method. Note that we check to see if the recognizer is 
;;; still 'active.' before we move on. 
(defmethod recognizer-tell ((epr event-pattern-recognizer) (event evnt))
  (case (recognizer-status epr)
    (:futile (update-status epr :status :futile :reason :futile-recognizer-resignaled))
    (:complete (update-status epr :status :futile :reason :complete-recognizer-resignaled))
    (t (rec.tell epr event))))


;;;(defmethod rec.tell-1 ((epr event-pattern-recognizer) (event evnt) (epr2 event-pattern-recognizer))
;;;  (rec.tell epr2 event))


			      

;;; -------------------------------------------------------------------
;;; SEQUENTIAL
;;; -------------------------------------------------------------------

(defclass sequential-event-pattern-recognizer (event-pattern-recognizer) ())

(defmethod rec.tell ((epr sequential-event-pattern-recognizer) (event evnt))
  (assert (not (null (remainder-list epr))))
  (rec.tell-1 epr event (car (remainder-list epr))))

(defmethod rec.tell-1 ((epr sequential-event-pattern-recognizer) (event evnt) (epr2 event-pattern-recognizer))
  (let ((result (recognizer-tell epr2 event)))
    (ecase result
      ((:complete)
       (let ((bs (binding-set-union (recognizer-bindings epr) (recognizer-bindings epr2))))
       (update-status epr 
		     :remainder (substitute-bindings (cdr (remainder-list epr)) bs)
		     :seen-in-order (append (seen-in-order epr) (seen-in-order epr2))
		     :bindings bs
		     :ended (interval-end (event-interval event)))
       (if (null (remainder-list epr))
	   (update-status epr 
			  :status :complete 
			  :interval (make-interval (recognizer-started epr) (recognizer-ended epr)))
	 (update-status epr :status :active))))
      ((:active)
       (update-status epr
		      :bindings (binding-set-union (recognizer-bindings epr) (recognizer-bindings epr2))
		      :status :active))
      ((:ignore)
       (update-status epr :status :ignore))
      ((:futile)
       (update-status epr :status :futile :reason (recognizer-reason epr2))))))

(defmethod rec.tell-1 ((epr sequential-event-pattern-recognizer) (event evnt) (event-form T))
  (let ((newbindings (pat-match event-form (event-form event))))
    (if (not (eq newbindings fail))
	;; we have a match.
	(if (and (not (null (recognizer-ended epr)))
		 (> (recognizer-ended epr) (interval-start (event-interval event))))
	    (update-status epr :status :futile :reason :out-of-order)
	  (let* ((bs (binding-set-union (recognizer-bindings epr) newbindings))
		 (st (if (null (recognizer-started epr))
			 (interval-start (event-interval event))
		       (recognizer-started epr)))
		 (end (interval-end (event-interval event)))
		 (rem (substitute-bindings (cdr (remainder-list epr)) bs)))
	    (update-status epr 
			   :bindings bs
			   :remainder rem
			   :started st
			   :ended end 
			   :seen   (cons (event-form event)
					 (remove-if #'(lambda (e)
							(contravene-p (event-form event) e))
						    (seen-set epr)))
			   :seen-in-order (nconc (seen-in-order epr) (list event))
			   :interval (if (null rem) ;; i.e., complete
					 (make-interval st end))
			   :status (if (null rem) ;; i.e., complete
				       :complete
				     :active))))
      ;; didn't match
      (if (some #'(lambda (e)
		    (contravene-p (event-form event) e))
		(seen-set epr))
	  (update-status epr :status :futile :reason :contravention)
	(update-status epr :status :ignore)))))

		     

;;; -------------------------------------------------------------------
;;; ALL
;;; -------------------------------------------------------------------

(defclass all-event-pattern-recognizer (event-pattern-recognizer) ())

(defmethod rec.tell ((epr all-event-pattern-recognizer) (event evnt))
  (assert (not (null (remainder-list epr))))
  (rec.tell-r epr event (remainder-list epr)))

(defmethod rec.tell-r ((epr all-event-pattern-recognizer) (event evnt) rlist)
  (cond
   ((null rlist)
    (update-status epr :status :ignore))
   (t
    (let ((eform (car rlist)))
      (cond
       ((event-pattern-recognizer-p eform)
	(let ((result (recognizer-tell eform event)))
	  (ecase result
	    ((:complete)
	     (let ((bs (binding-set-union (recognizer-bindings epr) (recognizer-bindings eform))))
	       (update-status epr 
			      :remainder (substitute-bindings (remove eform (remainder-list epr)) bs)
			      :seen-in-order (append (seen-in-order epr) (seen-in-order eform))
			      :bindings bs
			      :ended (interval-end (event-interval event)))
	       (if (null (remainder-list epr))
		   (update-status epr 
				  :status :complete 
				  :interval (make-interval (recognizer-started epr) (recognizer-ended epr)))
		   (update-status epr :status :active))))
	    ((:active)
	     (update-status epr
			    :bindings (binding-set-union (recognizer-bindings epr) (recognizer-bindings eform))
			    :status :active))
	    ((:ignore)
	     (update-status epr :status :ignore)
	     (rec.tell-r epr event (cdr rlist)))
	    ((:futile)
	     (update-status epr :status :futile :reason (recognizer-reason eform))))))
       (t ;; a regular event form...
	(let ((newbindings (pat-match eform (event-form event))))
	  ;; (format t "Sanity check: we have a non-recognizer here: eform/rlist: ~S/~S~%" eform rlist)
	  (if (not (eq newbindings fail))
	      ;; we have a match.
	      (let* ((bs (binding-set-union (recognizer-bindings epr) newbindings))
		     (st (if (null (recognizer-started epr))
			     (interval-start (event-interval event))
			   (recognizer-started epr)))
		     (end (interval-end (event-interval event)))
		     (rem (remove (event-form event) (substitute-bindings (remainder-list epr) bs)
				  :test #'tree-equal)))
		;; (format t "Sanity check: we have a match.~%")
		(update-status epr 
			       :bindings bs
			       :remainder rem
			       :started st
			       :ended end 
			       :seen   (cons (event-form event)
					     (remove-if #'(lambda (e)
							    (contravene-p (event-form event) e))
							(seen-set epr)))
			       :seen-in-order (nconc (seen-in-order epr) (list event))
			       :interval (if (null rem) ;; i.e., complete
					     (make-interval st end))
			       :status (if (null rem) ;; i.e., complete
					   :complete
					 :active)))
	    ;; didn't match
	    (if (some #'(lambda (e)
			  (contravene-p (event-form event) e))
		      (seen-set epr))
		(update-status epr :status :futile :reason :contravention)
	      (progn
		;; (format t "Sanity check: We do *not* have a match. ~%")
		(update-status epr :status :ignore)
		(rec.tell-r epr event (cdr rlist))))))))))))

;;; -------------------------------------------------------------------
;;; ONE-OF
;;; -------------------------------------------------------------------

(defclass one-of-event-pattern-recognizer (event-pattern-recognizer) ())

(defmethod rec.tell ((epr one-of-event-pattern-recognizer) (event evnt))
  (assert (not (null (remainder-list epr))))
  (rec.tell-r epr event (remainder-list epr)))

(defmethod rec.tell-r ((epr one-of-event-pattern-recognizer) (event evnt) rlist)
  (cond
   ((null rlist)
    (update-status epr :status :ignore))
   (t
    (let ((eform (car rlist)))
      (cond
       ((event-pattern-recognizer-p eform)
	(let ((result (recognizer-tell eform event)))
	  (ecase result
	    ((:complete)
	     (let ((bs (binding-set-union (recognizer-bindings epr) (recognizer-bindings eform)))
		   (st (or (recognizer-started epr) (interval-start (event-interval event))))
		   (end (interval-end (event-interval event))))
	       (update-status epr 
			      :remainder (remove eform (remainder-list epr))
			      :seen-in-order (append (seen-in-order epr) (seen-in-order eform))
			      :bindings bs
			      :started st
			      :ended end
			      :interval (make-interval st end)
			      :status :complete)))
	    ((:active)
	     (update-status epr
			    :bindings (binding-set-union (recognizer-bindings epr) (recognizer-bindings eform))
			    :status :active))
	    ((:ignore)
	     (update-status epr :status :ignore)
	     (rec.tell-r epr event (cdr rlist)))
	    ((:futile)
	     (update-status epr :status :futile :reason (recognizer-reason eform))))))
       (t ;; a regular event form...
	(let ((newbindings (pat-match eform (event-form event))))
	  ;; (format t "Sanity check: we have a non-recognizer here: eform/rlist: ~S/~S~%" eform rlist)
	  (if (not (eq newbindings fail))
	      ;; we have a match.
	      (let* ((bs (binding-set-union (recognizer-bindings epr) newbindings))
		     (st (if (null (recognizer-started epr))
			     (interval-start (event-interval event))
			   (recognizer-started epr)))
		     (end (interval-end (event-interval event)))
		     (rem (remove eform (remainder-list epr)))
			  
		     )
		;; (format t "Sanity check: we have a match.~%")
		(update-status epr 
			       :bindings bs
			       :remainder rem ;; not really necessary, but kind of nice
			       :started st
			       :ended end 
			       :seen  (list (event-form event)) 
			       :seen-in-order (nconc (seen-in-order epr) (list event))
			       :interval (make-interval st end)
			       :status :complete))
	    ;; didn't match
	    (rec.tell-r epr event (cdr rlist))))))))))

;;; -------------------------------------------------------------------
;;; WITHIN
;;; -------------------------------------------------------------------

(defclass within-event-pattern-recognizer (event-pattern-recognizer) 
  ((duration :initarg :duration :initform NIL :accessor within-duration)
   ))

(defmethod rec.tell ((epr within-event-pattern-recognizer) (event evnt))
  (let ((eform (car (remainder-list epr))))
    (if (event-pattern-recognizer-p eform)
	(ecase (rec.tell eform event)
	  ((:complete)
	   (let ((intr (recognizer-interval eform)))
	     (unless (interval-p intr)
	       (error "Interval not returned from ~A in ~A" (second (recognizer-pattern epr))
		       (recognizer-pattern epr)))
	     (if (<= (- (interval-end intr) (interval-start intr))
		     (within-duration epr))
		 (update-status epr
		  :seen-in-order (seen-in-order eform)
		  :bindings (recognizer-bindings eform)
		  :interval intr
		  :started (interval-start intr)
		  :ended (interval-end intr)
		  :status :complete)
	       (update-status epr
		:status :futile
		:reason :duration-exceeded))))
	  ((:active)
	   (update-status epr :status :active)) ;; should we check interval??
	  ((:ignore)
	   (update-status epr :status :ignore))
	  ((:futile
	    (update-status epr :status :futile :reason (recognizer-reason eform)))))
      ;; a regular form 
      (let ((newbindings (pat-match eform (event-form event))))
	(if (not (eq newbindings fail))
	    ;; we have a match. check duration
	    (let ((intr (event-interval event)))
	      (if (<= (- (interval-end intr) (interval-start intr))
		      (within-duration epr))
		  (update-status epr
				 :seen-in-order (list event)
				 :seen (list (substitute-bindings eform newbindings))
				 :bindings newbindings
				 :interval intr
				 :started (interval-start intr)
				 :ended (interval-end intr)
				 :status :complete)
		(update-status epr ;; most of this for debugging 
				 :seen-in-order (list event)
				 :seen (list (substitute-bindings eform newbindings))
				 :bindings newbindings
				 :interval intr
				 :started (interval-start intr)
				 :ended (interval-end intr)
				 :status :futile
				 :reason :duration-exceeded)))
	  ;; we don't have a match.
	  (update-status epr :status :ignore))))))

;;; -------------------------------------------------------------------
;;; WITHOUT
;;; -------------------------------------------------------------------

(defclass without-event-pattern-recognizer (event-pattern-recognizer) 
  ((interval :initarg :interval :initform NIL :accessor without-interval)
   ))

(defmethod rec.tell ((epr without-event-pattern-recognizer) (event evnt))
  (let ((eform (car (remainder-list epr))))
    (if (event-pattern-recognizer-p eform)
	(ecase (rec.tell eform event)
	  ((:complete) ;; we saw one; did it happen within interval???
	   (let ((intr (recognizer-interval eform)))
	     (unless (interval-p intr)
	       (error "Interval not returned from ~A in ~A" (second (recognizer-pattern epr))
		      (recognizer-pattern epr)))
	     (if (> (recognizer-started eform)
		    (interval-end (without-interval eform)))
		 (update-status epr :status :complete)
	       (update-status epr
			      :status :futile
			      :reason  :without-interval-violated))))
	  ((:active)
	   (if (> (interval-start (event-interval event)))
	       (update-status epr :status :complete)
	     (update-status epr :status :active)))
	  ((:ignore)
	   (if (> (interval-start (event-interval event)))
	       (update-status epr :status :complete)
	     (update-status epr :status :ignore)))
	  ((:futile)
	   (update-status :status :complete))) ;; we can never see it, so we will succeed!
      ;; a regular form 
      (let ((newbindings (pat-match eform (event-form event))))
	(if (not (eq newbindings fail))
	    ;; we have a match. check duration
	    (if (> (interval-start (event-interval event))
		   (interval-end (without-interval epr)))
		(update-status epr :status :complete)
	      (update-status epr :status :futile
			     :reason :without-interval-violated))
	  ;; no match ... might succeed, still
	  (if (> (interval-start (event-interval event))
		   (interval-end (without-interval epr)))
		(update-status epr :status :complete)
	      (update-status epr :status :ignore)))))))


;;; -------------------------------------------------------------------
;;; ALLEN
;;; -------------------------------------------------------------------

(defclass allen-event-pattern-recognizer (event-pattern-recognizer) 
  ((relationship :initarg :relationship :initform NIL :accessor allen-relationship)
   (first-interval :initarg :first-interval :initform NIL :accessor first-interval)
   (second-interval :initarg :second-interval :initform NIL :accessor second-interval)   
   ))

(defmethod rec.tell ((epr allen-event-pattern-recognizer) (event evnt))
  (let ((first (first (remainder-list epr)))
	(second (second (remainder-list epr)))
	(allen-relationship (allen-relationship epr)))
      (cond
       ;; neither one seen yet ...
       ((and (null (first-interval epr))
	     (null (second-interval epr)))
	(multiple-value-bind (result interval seen-in-order)
	    (rec.tell-1 epr event first)
	  (ecase result
	    ((:complete)
	     (setf (first-interval epr) interval)
	     (update-status 
	      epr
	      :started (interval-start interval)
	      :seen-in-order (append (seen-in-order epr) seen-in-order)
	      :status :active))
	    ((:futile) ;; *must* come from a recgnizer
	     (if (event-pattern-recognizer-p first)
		 (update-status epr
				:status :futile
				:reason (recognizer-reason first))
	       (error "FUTILE returned from non-recognizer: ~A." first)))
	    ((:active :ignore)
	     (multiple-value-bind (result2 interval2 seen-in-order2)
		 (rec.tell-1 epr event second)
	       (ecase result2
		 ((:complete)
		  (setf (second-interval epr) interval2)
		  (update-status 
		   epr
		   :started (interval-start interval2)
		   :seen-in-order (append (seen-in-order epr) seen-in-order2)
		   :status :active))
		 ((:futile) ;; *must* come from a recgnizer
		  (if (event-pattern-recognizer-p second)
		      (update-status epr
				     :status :futile
				     :reason (recognizer-reason second))
		    (error "FUTILE returned from non-recognizer: ~A." second)))
		 ((:active :ignore)
		  (update-status epr :status :active))))))))
	;; first one has been seen
       ((not (null (first-interval epr)))
	(multiple-value-bind (result2 interval2 seen-in-order2)
	    (rec.tell-1 epr event second)
	  (ecase result2
	    ((:complete)
	     (setf (second-interval epr) interval2)
	     (if (funcall allen-relationship (first-interval epr) interval2)
		 (let ((start (min (interval-start (first-interval epr))
				   (interval-start (second-interval epr))))
		       (end (max (interval-end (first-interval epr))
				 (interval-end (second-interval epr)))))
		       
		 (update-status epr 
				:seen-in-order (append (seen-in-order epr) seen-in-order2)
				:started start
				:ended end
				:interval (make-interval start end)
				:status :complete))
	       (update-status epr :status :futile 
			      :reason :failed-allen-relationship)))
	    ((:futile) ;; *must* come from a recgnizer
	     (if (event-pattern-recognizer-p second)
		 (update-status epr
				:status :futile
				:reason (recognizer-reason second))
	       (error "FUTILE returned from non-recognizer: ~A." second)))
	    ((:active :ignore)
	     (update-status epr :status  :active)))))
	;;; 2nd one has been seen
	(T (multiple-value-bind (result interval seen-in-order)
	    (rec.tell-1 epr event first)
	  (ecase result
	    ((:complete)
	     (setf (first-interval epr) interval)
	     (if (funcall allen-relationship interval (second-interval epr))
		 (update-status epr 
				:seen-in-order (append (seen-in-order epr) seen-in-order)
				:status :complete)
	       (update-status epr :status :futile 
			      :reason :failed-allen-relationship)))	     
	    ((:futile) ;; *must* come from a recgnizer
	     (if (event-pattern-recognizer-p first)
		 (update-status epr
				:status :futile
				:reason (recognizer-reason first))
	       (error "FUTILE returned from non-recognizer: ~A." first)))
	    ((:active :ignore)
	     (update-status epr :status :active))))))))


		      

(defmethod rec.tell-1 ((epr allen-event-pattern-recognizer) (event evnt) (epr2 event-pattern-recognizer))
  (let ((result (rec.tell epr2 event)))
    (ecase result
      ((:complete)
       (let ((bs (binding-set-union (recognizer-bindings epr)
				    (recognizer-bindings epr2))))
	 (update-status 
	  epr 
	  :bindings bs
	  :remainder (substitute-bindings (remainder-list epr) bs))
	 (values :complete (recognizer-interval epr2) (seen-in-order epr2))))
      ((:active)
       (let ((bs (binding-set-union (recognizer-bindings epr)
				    (recognizer-bindings epr2))))
	 (update-status 
	  epr 
	  :bindings bs
	  :remainder (substitute-bindings (remainder-list epr) bs))
	 (values :active NIL '())))
      ((:ignore)
       (values :ignore NIL '()))
      ((:futile
	(values :futile NIL '()))))))
    
(defmethod rec.tell-1 ((epr allen-event-pattern-recognizer) (event evnt) (event-form T))
  (let ((newbindings (pat-match event-form (event-form event))))
    (if (not (eq newbindings fail))
	;; we have a match
	(let ((bs (binding-set-union newbindings (recognizer-bindings epr))))
	  (update-status 
	   epr
	   :bindings bs
	   :remainder (substitute-bindings (remainder-list epr) bs))
	  (values :complete (event-interval event) (list event)))
      (values :ignore NIL '()))))




;;; ----------------------------------------------------------------------------
;;; Pattern compilation
;;; ----------------------------------------------------------------------------

;; '(:in-order a b d (:any a b c) f) :in-order

(defparameter  *era-syntax-words* 
    '(:in-order :one-of :all :within :without 
      :contains :finishes :starts :before :meets :overlaps
      :cotemporal :during :finished-by :started-by :after :met-by
      :overlapped-by
      :precedes :follows :intersects))

(defun era-syntax-word-p (x)
  (and (member x *era-syntax-words* :test #'eq) t))

(defun compile-event-pattern (pattern)
  (if (and (consp pattern)
	   (era-syntax-word-p (car pattern)))
      (compile-pattern-form pattern (car pattern))
    (compile-pattern-form pattern :raw)))

(defmethod compile-pattern-form (form (type (eql :raw)))
  (if (consp form)
      (compile-pattern-form `(:in-order ,@form) :in-order)
    (make-instance 'sequential-event-pattern-recognizer 
      :pattern form
      :remainder (list form))))


(defmethod compile-pattern-form (form (type (eql :in-order)))
  (when (null (cdr form))
    (error "At least one element is necessary after :IN-ORDER"))
  (make-instance 'sequential-event-pattern-recognizer 
    :pattern form
    :remainder 
    (mapcar #'(lambda (pitem)
		(if (and (consp pitem)
			 (era-syntax-word-p (car pitem)))
		    (compile-pattern-form pitem (car pitem))
		  pitem))
	    (cdr form))))

(defmethod compile-pattern-form (form (type (eql :all)))
  (when (null (cdr form))
    (error "At least one element is necessary after :ALL"))  
  (make-instance 'all-event-pattern-recognizer 
    :pattern form
    :remainder 
    (mapcar #'(lambda (pitem)
		(if (and (consp pitem)
			 (era-syntax-word-p (car pitem)))
		    (compile-pattern-form pitem (car pitem))
		  pitem))
	    (cdr form))))

(defmethod compile-pattern-form (form (type (eql :one-of)))
  (when (null (cdr form))
    (error "At least one element is necessary after :ONE-OF"))  
  (make-instance 'one-of-event-pattern-recognizer 
    :pattern form
    :remainder 
    (mapcar #'(lambda (pitem)
		(if (and (consp pitem)
			 (era-syntax-word-p (car pitem)))
		    (compile-pattern-form pitem (car pitem))
		  pitem))
	    (cdr form))))

(defmethod compile-pattern-form (form (type (eql :within)))
  (unless (= (length form) 3)
    (error "Bad :WITHIN form. Syntax should be (:WITHIN event duration), was ~A." form))
  (unless (realp (third form))
    (error "Third value in :WITHIN form must be a number, was ~A" (third form)))
  (make-instance 'within-event-pattern-recognizer 
    :pattern form
    :duration (third form)
    :remainder 
    (if (consp (second form))
	(list (compile-pattern-form (second form) (first (second form))))
      (list (second form)))))


(defmethod compile-pattern-form (form (type (eql :without)))
  (unless (= (length form) 4)
    (error "Bad :WITHOUT form. Syntax should be (:WITHOUT event start finish), was ~A." form))
  (unless (and (realp (third form)) (realp (fourth form)))
    (error "Third and/or fourth value in :WITHOUT form must be a number, were ~A, ~A" (third form) (fourth form)))
  (make-instance 'without-event-pattern-recognizer 
    :pattern form
    :interval (make-interval (third form) (fourth form))
    :remainder 
    (if (consp (second form))
	(list (compile-pattern-form (second form) (first (second form))))
      (list (second form)))))

(defmacro define-allen-compiler (name relationship)
  `(defmethod compile-pattern-form (form (type (eql ,name)))
     (unless (= (length form) 3)
       (error "Bad :~a form. Syntax should be (:~a event event), was ~S." ,(string-upcase (symbol-name name)) ,(string-upcase (symbol-name name)) form))
     (make-instance 'allen-event-pattern-recognizer 
       :pattern form
       :relationship #',relationship
       :remainder 
       (mapcar #'(lambda (pitem)
		   (if (and (consp pitem)
			    (era-syntax-word-p (car pitem)))
		       (compile-pattern-form pitem (car pitem))
		     pitem))
	       (cdr form)))))

(define-allen-compiler :contains contains-p)
(define-allen-compiler :finishes finishes-p)
(define-allen-compiler :starts starts-p)
(define-allen-compiler :before before-p)
(define-allen-compiler :meets meets-p)
(define-allen-compiler :overlaps overlaps-p)
(define-allen-compiler :cotemporal cotemporal-p)
(define-allen-compiler :during during-p)
(define-allen-compiler :finished-by finished-by-p)
(define-allen-compiler :started-by started-by-p)
(define-allen-compiler :after after-p)
(define-allen-compiler :met-by met-by-p)
(define-allen-compiler :overlapped-by overlapped-by-p)

(defun precedes-p (i1 i2)
  (if (= (interval-start i1) (interval-end i1))
      (< (interval-end i1) (interval-start i2))
    (or (before-p i1 i2)
	(meets-p i1 i2))))

(defun follows-p (i1 i2)
  (if (= (interval-start i1) (interval-end i1))
      (> (interval-start i1) (interval-end  i2))
    (or (after-p i1 i2)
	(met-by-p i1 i2))))

(defun intersects-p (i1 i2)
  (and (not (before-p i1 i2))
       (not (after-p i1 i2))))

(define-allen-compiler :precedes precedes-p)
(define-allen-compiler :follows follows-p)
(define-allen-compiler :intersects intersects-p)

;;; ----------------------------------------------------------------------------
;;; Event recognizer queue
;;; ----------------------------------------------------------------------------

(defstruct recognizer-queue-entry
  recognizer
  pattern
  restart-on-active
  restart-on-complete
  restart-on-futile
  restart-on-rebinding
  callback
  lease)

(defun duplicate-recognizer-queue-entry (recognizer-queue-entry)
  (let ((new (copy-recognizer-queue-entry recognizer-queue-entry)))
    (setf (recognizer-queue-entry-recognizer recognizer-queue-entry)
      (compile-event-pattern (recognizer-queue-entry-pattern recognizer-queue-entry)))
    new))

(defstruct recognizer-queue recognizers (lock (proc.make-lock)))
										  
(defparameter *recognizer-queue* (make-recognizer-queue) "Queue of event recognizers")

(defun add-recognizer (pattern &key restart-on-active
				    restart-on-complete
				    restart-on-futile
				    restart-on-rebinding
				    callback
				    lease
				    (queue *recognizer-queue*))
  (assert (or (functionp callback) (null callback)))
  (assert (or (and (realp lease) (>= lease 0)) (null lease)))
  (let ((er (make-recognizer-queue-entry
	     :recognizer (if (event-pattern-recognizer-p pattern)
			     pattern
			   (compile-event-pattern pattern))
	     :pattern (if (event-pattern-recognizer-p pattern)
			  (recognizer-pattern pattern)
			pattern)
	     :restart-on-active restart-on-active
	     :restart-on-complete restart-on-complete
	     :restart-on-futile restart-on-futile
	     :restart-on-rebinding restart-on-rebinding
	     :callback callback
	     :lease lease)))
    (add-recognizer-entry er queue)))

(defun add-recognizer-entry (recognizer-queue-entry queue)
  (proc.with-lock
	(recognizer-queue-lock queue)
      (setf (recognizer-queue-recognizers queue)
	(nconc (recognizer-queue-recognizers queue)
	       (list recognizer-queue-entry )))))

(defun clear-recognizer-queue (&optional (queue *recognizer-queue*))
  (proc.with-lock
	(recognizer-queue-lock queue)
	(setf (recognizer-queue-recognizers queue)
	  (list))))
       

(defun process-recognizer-queue (event &optional (queue *recognizer-queue*))
  (let ((completed ())
	(futiled ())
	(rebound ())
	(activated ())
	(restarted ())
	(overleased ()))
    ;; send the event to each of the recognizers, checking lease first
    (dolist (recognizer (recognizer-queue-recognizers queue))
      (if (and (recognizer-queue-entry-lease recognizer)
	       (>= (current-milliseconds) (recognizer-queue-entry-lease recognizer)))
	  (push recognizer overleased)
	(let* ((current-bindings (recognizer-bindings (recognizer-queue-entry-recognizer recognizer)))
	       (val (multiple-value-list (rec.tell (recognizer-queue-entry-recognizer recognizer) event))))
	  (ecase (car val)
	    ((:futile)
	     (push recognizer futiled))
	    ((:complete)
	     (push recognizer completed))
	    ((:active)
	     (push recognizer activated)
	     (when (not (eq (recognizer-bindings (recognizer-queue-entry-recognizer recognizer)) current-bindings))
	       (push recognizer rebound)))
	    ((:ignore)
	     (when (not (eq (recognizer-bindings (recognizer-queue-entry-recognizer recognizer)) current-bindings))
	       (push recognizer rebound)))))))
    ;; remove futile, complete, and overleased recognizers
    ;; maybe add to restart list
    (dolist (futile futiled)
      (proc.with-lock
	  (recognizer-queue-lock queue)
	(setf (recognizer-queue-recognizers queue)
	  (delete futile (recognizer-queue-recognizers queue))))
      (when (recognizer-queue-entry-restart-on-futile futile)
	(push futile restarted)))
    (dolist (complete completed)
      (proc.with-lock
	  (recognizer-queue-lock queue)
	(setf (recognizer-queue-recognizers queue)
	  (delete complete (recognizer-queue-recognizers queue))))
      (when (recognizer-queue-entry-restart-on-complete complete)
	(push complete restarted)))
    (dolist (ov overleased)
      (proc.with-lock
	  (recognizer-queue-lock queue)
	(setf (recognizer-queue-recognizers queue)
	  (delete ov (recognizer-queue-recognizers queue)))))
    ;; maybe add rebound and active restarters to restart list
    (dolist (reb rebound)
      (when (recognizer-queue-entry-restart-on-rebinding reb)
	(push reb restarted)))
    (dolist (active activated)
      (when (recognizer-queue-entry-restart-on-active active)
	(push active restarted)))
    ;; call any callback functions
    (dolist (complete completed)
      (when (recognizer-queue-entry-callback complete)
	(funcall (recognizer-queue-entry-callback complete)
		 (substitute-bindings (recognizer-pattern (recognizer-queue-entry-recognizer complete))
				      (recognizer-bindings (recognizer-queue-entry-recognizer complete)))
		 (recognizer-interval (recognizer-queue-entry-recognizer complete)))))
    ;; add restarts ...
    (dolist (restart (remove-duplicates restarted))
      (add-recognizer-entry (duplicate-recognizer-queue-entry restart)
			    queue))
    ;; return number of completed patterns
    (length completed)))
    

;;; ----------------------------------------------------------------------
;;; Event queue. Essentially, this is ACL's 'xqueue' example.
;;; 
;;; note: the event queue can get arbitrary large unless MAX-COUNT
;;; is set to a positive number.
;;; -----------------------------------------------------------------------

(defstruct event-queue 
  events 
  (count 0)
  (max-count 1000)
  (lock (proc.make-lock))
  (gate (proc.make-gate NIL))
  process)

(defparameter *event-queue* (make-event-queue))

(defun enqueue-event (event &optional (queue *event-queue*))
  (proc.with-lock (event-queue-lock queue)
    (when (>= (event-queue-count queue)
	      (event-queue-max-count queue))
      ;; probably only 1, but just to be sure.
      (dotimes (i (1+ (- (event-queue-max-count queue)
			 (event-queue-count queue))))
	(pop (event-queue-events queue))
	(decf (event-queue-count queue))))
    (setf (event-queue-events queue)
      (nconc (event-queue-events queue) (list event)))
    (incf (event-queue-count queue))
    (proc.open-gate (event-queue-gate queue)))
  event)
					    
(defun start-event-server (&optional (queue *event-queue*)
				     (process 'process-recognizer-queue))
  (setf (event-queue-process queue)
    (proc.run-function
	"Event Server"
      #'(lambda (lqueue server-function)
	  (loop
	    (proc.wait "Waiting for events"
			     #'proc.gate-open-p
			     (event-queue-gate lqueue))
	    (let (event run)
	      (proc.with-lock (event-queue-lock lqueue)
		(if (null (event-queue-events lqueue))
		    (proc.close-gate (event-queue-gate lqueue))
		  (progn
		    (setq event (pop (event-queue-events lqueue)))
		    (decf (event-queue-count lqueue)) ;; assumes non-null!
		    (setq run t)))
		(when (and run (eq :exit (funcall server-function event)))
		  (return))))))
      queue process))
  queue)

(defun stop-event-server (&optional (queue *event-queue*))
  (when (event-queue-process queue)
    (proc.stop (event-queue-process queue)))
  queue)

(defun wait-for-event-pattern (pattern &optional timeout (recognizer-queue *recognizer-queue*))
  "Process will run until attempt to recognize pattern is complete or futile, or until timeout seconds have passed.
   Pattern is compiled and placed on recognizer-queue, which defaults to *recognizer-queue*. Returns two values,
   the status, and the recognizer."
  (let* ((epr (compile-event-pattern pattern))
	 (fn #'(lambda ()
		 (let ((status (recognizer-status epr)))
		   (or (eq status :complete)
		       (eq status :futile))))))
    (add-recognizer epr :queue recognizer-queue)
    (if timeout
	(proc.wait-with-timeout 
	 (format nil "Waiting for event pattern to complete (timeout in ~f second~p)." timeout timeout)
	 timeout fn)
	 (proc.wait 
	  "Waiting for event pattern to complete (no timeout)." 
	  fn))
    (values (recognizer-status epr) epr)))

;;; --
;;; just for testing ...
;;; --

(defun rtest (pattern eforms)
  (let ((epr (compile-event-pattern pattern)))
    (let ((result NIL))
      (dolist (seq eforms (values epr result))
	(cond
	 ((and (consp seq)
	       (numberp (first (last seq))))
	  (setq result (recognizer-tell epr (make-event (car seq) (second seq) (third seq))))
	  )
	 (T
	  (setq result (recognizer-tell epr (make-event seq (current-milliseconds))))))))))



(defun mr (pattern)
  (add-recognizer pattern
		  :restart-on-rebinding t
		  :callback (lambda (event interval)
			      (format t "Saw ~S over ~A%" event interval))))