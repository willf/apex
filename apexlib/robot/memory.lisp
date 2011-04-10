;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Robot memory
;;; apex/apexlib/robot/memory.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: memory.lisp,v 1.5 2006/01/15 03:42:51 dalal Exp $

(in-package :common-lisp-user)

;;; --------------------------------------------------------------------
;;;   ----- Memory -----

;;; The memory resource represents mechanisms for encoding and storing
;;; transient information.  Two types of signals to the resource are
;;; allowed.  1) ENCODE signals cause the resource to add an item to
;;; the memory store after one cycle.  A newly encoded items
;;; supercedes an older items if the two can be matched on the basis
;;; of a FLUENT declaration.  2) RETRIEVE signals produce different
;;; effects depending on whether a match to the signalled retrieval
;;; cue exists in memory.  If not, a "feeling of knowing" ends the
;;; retrieval attempt in a time less than would be requred to retrieve
;;; the item if present.

(defclass memory (resource)
  ((last-retrieved :accessor last-retrieved))
  ) ;; last item retrieved from memory-store (if any)

(defun get-beliefs (mem)  ;; belief set stored with agent
  (beliefs (agent-of mem)))

(defun add-belief (mem prop)
  (push prop (beliefs (agent-of mem))))

;;; ----- Memory activities
;;;
;;; Memory activities include retrieving and encoding.

;;; -- Retrieving and retrieved
;;;
;;; Futility-interval is useful for modeling feeling-of-knowing effects
;;; that might cause someone to stop a retrieval attempt when there is
;;; no expectation of a successful retrieval, rather than persist as long
;;; as a successful retrieval would require.

(defclass retrieving (resource-activity)
  ((cue :accessor cue :initarg :cue :initform nil)
   ;; pattern for retrieval process to match against memory store
   (match :accessor match :initarg :match :initform nil)
   ;; memory item that matches cue (may be nil)
   (futility-interval :accessor futility-interval :initarg :futility-interval
		      :initform nil))) ;; how long to wait if cue has no match

(defmethod initialize-activity ((act retrieving) (mem-1 memory))
  (setf (match act) (find-in-memstore (cue act) (get-beliefs mem-1)))
  (if (and (not (match act)) (futility-interval act))
      (schedule-completion act (futility-interval act))))

(defun find-in-memstore (cue store)
  (let ((prop (find-if #'(lambda (item) 
			   (pat-match cue (proposition-prop item)))
		      (contents store))))
    (or prop 'not-in-mem)))

(defmethod complete-activity ((act retrieving) (mem-1 memory))
  (if (match act)
      (signal-event (retrieved mem-1 (match act)))
    (signal-event (not-retrieved mem-1 (cue act)))))

(defun retrieved (mem-1 match &key cause)
  (let ((agent (agent-of mem-1)))
    (setx (retrieved mem-1) match :cause cause)
    (cogevent match agent :cause cause)
    (cogevent `(retrieved ,match) agent :cause cause :trigger-asa t)))

(defun not-retrieved (mem-1 cue &key cause)
  (cogevent `(not retrieved ,cue) (agent-of mem-1) :cause cause :trigger-asa t))


;;; --- Encoding and encoded
;;;
;;; The encoding process uses knowledge of fluent forms to determine if
;;; any existing items in memory are to be overridden by new entry.  
;;; Memory is considered to be a simple assertional database.

(defclass encoding (resource-activity)
  ((assertion :accessor assertion :initarg :assertion :initform nil))
  );; new proposition to be stored

(defmethod complete-activity ((act encoding) (mem-1 memory))
  (signal-event (encoded mem-1 (assertion act))))

(defun encoded (mem-1 p &key cause)
  (multiple-value-bind (fluent-form binds)
      (find-fluent-form p)
    (if (null fluent-form)
	(encode-nonfluent p mem-1 :cause cause)
      (encode-fluent p binds fluent-form mem-1 :cause cause))))

(defun encode-nonfluent (p mem-1 &key cause)
  (encode-memitem p 'new mem-1 :cause cause))

(defun encode-fluent (p binds fluent-form mem-1 &key cause)
  (loop for item in (beliefs mem-1) do
	(let ((rel (prop-relation p (proposition-prop item) 
				  binds fluent-form)))
	  (when (member rel '(revalued refreshed))
	    (encode-memitem p rel mem-1 item :cause cause)
	    (return))))
  (encode-memitem p 'new mem-1 :cause cause))


;;; --- Encode-memitem

;;; Handles encoding of a proposition into a memory store.  If the item
;;; is new to the memory store, a NEW cogevent is generated.  If it 
;;; modifies or confirms an existing item, a REVALUED or REFRESHED
;;; cogevent is generated instead.  Note that only in the case of a new
;;; item is there a need to add a new proposition to the store.

;;;   modal: one of {refreshed,revalued,new}
;;;   p: "new" proposition 
;;;   pstruc: nil or equals old (potentially obsolete) proposition
;;;   mem-1: a memory resource

(defun encode-memitem (p modal mem-1 &optional pstruc &key cause) 
  (let ((agent (component-of mem-1)))
  (case modal
    (new
     (add-belief mem-1 (make-proposition :prop p :status 'new))
     (cogevent `(new ,p) agent :cause cause :trigger-asa t))
    (revalued
     (setx (proposition-superceded pstruc) 
	   (cons (proposition-prop pstruc) (proposition-superceded pstruc))
	   :cause cause)
     (setx (proposition-prop pstruc) p :cause cause)
     (setx (proposition-status pstruc) 'revalued :cause cause)
     (setx (proposition-timestamp pstruc) (current-time) :cause cause)
     (cogevent `(revalued ,p) agent :cause cause :trigger-asa t))
    (refreshed
     (setx (proposition-timestamp pstruc) (current-time) :cause cause)
     (setx (proposition-status pstruc) 'refreshed :cause cause)
     (cogevent `(refreshed ,p) agent :cause cause :trigger-asa t)))))


;;; MEMORY

;;; --- Assumption failure

;;; note: steps get added to this proc at proc load time as a result
;;; of assume clauses
;;;(procedure :special
;;; (index (handle-assume-failure ?assumevar +self+))
;;; (handle-assume-failure ?assumevar +self+))  ;; +self+ is a global var

(primitive 
 (index (handle-assume-failure ?assumevar +self+))
 (return (handle-assume-failure ?assumevar +self+)))  ;; +self+ is a global var

;;; --- Encode and retrieve



(procedure
 (index (retrieve ?cue))
 (profile (memory 10 10))
 (step s1 (start-activity memory retrieving :cue ?cue :duration 1000 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?hack1 (retrieved ?cue)))
 (step s4 (terminate failure) (waitfor ?hack1 (not retrieved ?cue))))

(procedure
 (index (encode ?cue))
 (profile (memory 10 10))
 (step s1 (start-activity memory encoding :assertion ?cue 
                          :duration 1000 => ?act))
 (step hack1 (waitfor-completion ?act)
       (waitfor ?s1))
 (step s2 (reset +this-task+) (waitfor (resumed +this-task+)))
 (step s3 (terminate) (waitfor ?hack1)))
