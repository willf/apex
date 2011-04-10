;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/cogevents.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: cogevents.lisp,v 1.1 2006/03/13 19:22:37 will Exp $

(in-package :common-lisp-user)


;;; ------ Cogevents

;;; Cognitive events are buffered before being processed.  The function
;;; PROCESS-ALL-COGEVENTS, part of the main action-selection cycle, matches
;;; events in the list to items on the agent's monitor-array slot, enabling
;;; associated tasks if appropriate.

;;; Note: cogevents are all information events.  They may refer to stable
;;; aspects of worldstate or agentstate.  E.g. the event (on block table) means
;;; that some perceptual resource recently observed that the block was on the
;;; table, NOT (necessarily) that this is a new situation or even that it is new
;;; to the agent.  

;;; Calls to the cogevent function cause a cogevent to be put on the agent's
;;; cogevent list (buffer) for processing.

;;; ! add support for multivalued propositions (generated one for each value
;;; but only log the comprehensive version.  Alternately, support matches to
;;; single values in monitor matching rather than generate them individually.

(defmethod cogevent (eventform (agent-name symbol)
                     &key match-rationale match-required suppress-log
                          trigger-asa attributes (timestamp (current-time)))
  (cogevent eventform (find-the-agent agent-name)
            :timestamp timestamp
            :suppress-log suppress-log
            :trigger-asa trigger-asa
            :match-required match-required
            :match-rationale match-rationale
            :attributes attributes))

(defmethod cogevent (eventform (agent-name string)
                     &key match-required match-rationale suppress-log
                          trigger-asa attributes (timestamp (current-time)))
  (cogevent eventform (find-the-agent agent-name)
            :timestamp timestamp
            :suppress-log suppress-log
            :trigger-asa trigger-asa
            :match-required match-required
            :match-rationale match-rationale
            :attributes attributes))

(defun find-the-agent (name)  ; symbol + string -> Agent
  (or (find-agent name)
      (error "No agent named ~a" name)))

;;; sigh. no more telling the agent what it knows...
;;;(defmethod cogevent :before (eventform (agent agent)
;;;			     &key suppress-log trigger-asa attributes) ; -> ()
;;;  (declare (ignore suppress-log trigger-asa attributes))
;;;  (tell (agent-kb agent) eventform))



(defmethod record-in-memory ((cogevent cogevent) 
			     (agent agent))
  (with-slots (content timestamp) cogevent
    (if (valid-measurement-form-p content)
      (insert-measurement  
       (agent-sv-memory  agent)
       (make-measurement 
	(measurement-form-sv content)
	(measurement-form-value content)
        timestamp))
      ;; it's a 'simple episode -- put in SE memory
      
      (record-atomic-episode 
       (agent-ae-memory agent)
       content
       (timestamp cogevent)))))

(defmethod print-current-milliseconds ((app application) timestamp stream)
  (princ timestamp stream))

(defmethod print-current-milliseconds ((app non-native-application) timestamp stream)
  (princ timestamp stream))

(defmethod print-current-milliseconds ((app realtime-application) timestamp stream)
  (datetime-princ (milliseconds->datetime 
		   (+ (start-of app)
		      timestamp)) 
		  stream)
  (multiple-value-bind (hrs mins secs frac)
      (apex.utility.datetime:ms2hms timestamp)
    (format stream " [~2,'0,d:~2,'0,d:~2,'0,d.~3,'0,d] " hrs mins secs frac)
  ))

(defmethod log-cogevent ((cogevent cogevent))
  (when (log-stream *application*)
    (with-slots (log-stream) *application*
      (with-slots (content timestamp) cogevent
	(print-current-milliseconds *application* timestamp log-stream)
	(format log-stream " ~a~%" content)
	(force-output log-stream)))))
      
  
     
(defmethod cogevent (eventform (agent agent)
                     &key match-rationale match-required
                          suppress-log source cause trigger
                          trigger-asa attributes
                          (timestamp (current-time))) ; -> event
  ;; type checks here
  (let ((event (make-event eventform source cause
                           trigger agent attributes timestamp
                           match-required
                           match-rationale )))
    (enqueuef (cogevents agent) event)
    (record-in-memory event agent)
    (log-cogevent event)
    (unless suppress-log (record-event event))
    (if trigger-asa (asamain agent))
    ;; (when trigger-asa (format t ";;; -- ~a ~a~%" (current-time) (rhume t)))
    event))


;;; Called after all cogevents have been processed

(defun reset-cogevents (agent)
  (setf (cogevents agent) nil))
