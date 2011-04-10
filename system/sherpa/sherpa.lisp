;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/sherpa/sherpa.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: sherpa.lisp,v 1.34 2006/03/17 23:21:43 rharris Exp $

;;; Platform-independant portion of interface to Sherpa client.

(in-package :common-lisp-user)


;;;----------------------------------------------------------------------------
;;; global variables and constants
;;;----------------------------------------------------------------------------

;; socket port
;; ! arbitrary choice, perhaps not ideal

(defparameter *sherpa-port* 5555) 

;; version tag of apex

(defvar *apex-version* (nth 1 (delimited-string-to-list "$Name: v3-0-11-1 $" #\space)))


;; number of trace lines after which to send a heartbeat to sherpa, so
;; that it does not time out

(defparameter *sherpa-trace-count* 300)

;;maximum number of trace events to send back to sherpa
(defvar *max-trace-events* 1000)

;;;----------------------------------------------------------------------------
;;; sherpa support functions
;;;----------------------------------------------------------------------------

;; read command from sherpa socket stream and dispatch them
(defun dispatch-sherpa-command ()
  ;; read in the command string
  (let ((command-string (sherpa-read-line *socket-stream*))
        (result nil))
    
    ;; if we have a good command, dispatch that command
    (when (and (not (equal command-string ""))
               (equal (subseq command-string 0 1) "("))
      (setf result (eval-sherpa-command (read-from-string command-string))))
    
    ;; flush out any buffered stream bytes
    (finish-output *socket-stream*)

    ;; pass result of the eval back to caller
    result))

(defmacro without-errors (&rest forms)
  (let ((tag (gensym)))
    `(block ,tag
       (handler-bind ((error
                       #'(lambda (c)
                           (when *socket-stream*  ; ! necessary?
                             (sherpa-meta-error-start id)
                             (write-line-sherpa 
                              "In process: ~a~%"
                              (mp:process-name sys:*current-process*))
                             (write-line-sherpa "~a" c)
                             (sherpa-meta-error-end id)
                             (sherpa-stop-simtime-reporter)
                             (multiple-value-bind (debug?)
                                 (sherpa-get-error-info)
                               
                               ;; if asked to not debug the message,
                               ;; return values, to bypass the debugger
                               ;; otherwise we will drop into the debugger
                               
                               (if  (not debug?)
                                   ;; ! Should we return a value, like c?
                                   (return-from ,tag (values))))))))
       ,@forms))))


;; this is where global info about an error which came from sherpa
;; is stored so any old process can pick it up and use it, it should
;; be nil at all times when an error is not actively being handled

(defvar *sherpa-error-info* nil)

;; this is a special function called by sherpa to signal the
;; correct error behavior back to apex from sherpa
;; it does not conform to the standard command protocal in that
;; no meta information  (acknowledgement) is sent back to sherpa

(defun sherpa-set-error-info (id debug?)
  (setf *sherpa-error-info* (list id debug?)))

;; This function should return the last error handling information
;; provided by Sherpa: whether to drop into the debugger, and a string
;; containing the error message plus information needed to reproduce
;; the error.  This function must be "one-shot" -- a repeated call
;; should return nil values.

(defun sherpa-get-error-info ()  ; -> bool, string
  
  ;; wait for the error info the come in
  
  (while (not *sherpa-error-info*)
         
         ;; if we are in the command handler process then we need to
         ;; check for data comming in from the socket
         
         (let ((proc-name (mp:process-name sys:*current-process*))
               (hdlr-name *sherpa-command-handler-name*))
           (when (string= hdlr-name proc-name
                          :end1 (length hdlr-name)
                          :end2 (min (length hdlr-name) (length proc-name)))
             (dispatch-sherpa-command)))
         
         ;; don't hog ALL the cycles
         
         (sleep 0.1))
  
  ;; store the value of the global varabile, and reset it
  ;; to nill for some for the next error that might transpire
  
  (let ((info *sherpa-error-info*))
    (setf *sherpa-error-info* nil)
    
    ;; return the info
    
    (values (cadr info))))

;; evaluate sherpa command which SHOULD call an existing
;; function who names starts with "sherpa-"
(defun eval-sherpa-command (exp)

  ;; evaluate a reggiered version of the expresion from this:
  ;;    (<id> (sherpa-func) [params])
  ;; into this:
  ;;    (sherpa-func <id> [params])
  (eval exp))

;; execute forms with standard output teporaraly
;; directed to the sherpa socket stream
(defmacro with-output-to-sherpa (&body forms)
  `(let ((*standard-output* *socket-stream*)
         (*print-pretty* nil))   ; ! shadows sherpa-command-handler; okay?
     ,@forms
     (force-output *standard-output*)))

;;; write a single formatted line of text to sherpa
(defun write-line-sherpa (string &rest args) ; string, *(Any) -> ()
  (let ((stream *socket-stream*))
    (line-break stream) ; ! Could make more efficient by inlining
    (let ((*print-pretty* nil))  ; ! shadows sherpa-command-handler; okay?
      (apply #'format stream string args)
      (line-break stream)
      (force-output stream))))

(defun write-object-sherpa (obj) ; Any -> ()
  (write-line-sherpa "~s" obj))


;;;----------------------------------------------------------------------------
;;; sherpa meta commands used by verious sherpa functions to 
;;; signal message structure to sherpa
;;;----------------------------------------------------------------------------

(defun sherpa-meta-ack (id)
  (write-line-sherpa "sherpa~aack" id))

(defun sherpa-meta-nack (id)
  (write-line-sherpa "sherpa~anack" id))

(defun sherpa-meta-more (id)
  (write-line-sherpa "sherpa~amore" id))

(defun sherpa-meta-start (id)
  (write-line-sherpa "sherpa~astart" id))

(defun sherpa-meta-more-start (id)
  (sherpa-meta-more id)
  (sherpa-meta-start id))

(defun sherpa-meta-heartbeat (id)
  (write-line-sherpa "sherpa~aheartbeat" id))

(defun sherpa-meta-end (id)
  (write-line-sherpa "sherpa~aend" id))

(defun sherpa-meta-error-start (id)
  (write-line-sherpa "sherpa~aerror-start" id))

(defun sherpa-meta-error-end (id)
  (write-line-sherpa "sherpa~aerror-end" id))

;;;----------------------------------------------------------------------------
;;; functions called by sherpa, these should begin with "sherpa-"
;;;----------------------------------------------------------------------------

;; eval an expression from sherpa
(defun sherpa-eval (id exp)
  (sherpa-meta-more-start id)
  (with-output-to-sherpa
   (format t "~a" (ignore-errors (eval exp))))
  (sherpa-meta-end id))


(defun two-column-sort (items predicate)
  ;;sorts list of items such that filling 2 columns row-wise will be sorted via newspaper sort
  ;; sorting (a b c d e f) alphabetically will yield (a d b e c f) to display as
  ;; a d
  ;; b e
  ;; c f
  (let ((num-items (length items))
	(half-items (ceiling (length items) 2))
	(sorted-items (sort (copy-list items) predicate))
	(two-col-items '()))
    (loop for i from 0 to (- half-items 1)
	  do
	  (let ((index1 i)
		(index2 (+ i half-items)))
	    (cond ((< index1 num-items)
		   (setq two-col-items (nconc two-col-items (list (nth index1 sorted-items))))
		   (if (< index2 num-items)
		       (setq two-col-items (nconc two-col-items (list (nth index2 sorted-items)))))))))
    two-col-items))

;; get all event types
(defun sherpa-events (id)
  (sherpa-meta-more-start id)
  (with-output-to-sherpa
    (mapcar #'(lambda (x) (format t "~a ASA~%" x)) (two-column-sort asa-eventtypes-default #'string-lessp))) ;;*asa-event-types*
  (sherpa-meta-end id))


;; get all application specifice event types
(defun sherpa-app-events (id)
  (sherpa-meta-more-start id)
  (let ((app-event-types (two-column-sort (application-specific-event-types) #'string-lessp)))
    (if app-event-types
	(with-output-to-sherpa
	 (mapcar #'(lambda (x) (format t "~a APP~%" x)) app-event-types))))
  (sherpa-meta-end id))

;;  (loop while (not (eq (state *application*) 'initialized)) do
;;        (sleep 500))


;; inspect object
(defun sherpa-inspect (id object-id terse hide-null)
  (sherpa-meta-more-start id)
  (with-output-to-sherpa
      (inspect-internal object-id terse hide-null t))
  (sherpa-meta-end id))

(defun sherpa-multiple-inspect (id object-id-list terse hide-null)
  (sherpa-meta-more-start id)
  (with-output-to-sherpa
      (mapcar
       #'(lambda (object-id)
           (inspect-internal object-id terse hide-null t)
           (write-line-sherpa
            "---------------------------------------------------"))
       object-id-list))
  (sherpa-meta-end id))

;; is the given object an agent
(defun sherpa-is-agent (id object-id)
  (sherpa-meta-more-start id)
  (write-line-sherpa 
   "~a" (if (member object-id (mapcar #'id *all-agents*)) t nil))
  (sherpa-meta-end id))

;; provide a list of all agents in app
(defun sherpa-agent-list (id)
  (sherpa-meta-more-start id)
  (mapcar (lambda (agent)
            (write-line-sherpa "~a ~a" (id agent) (type-of-agent agent))) *all-agents*)
  (sherpa-meta-end id))

(defun type-of-agent (agent)
  (let ((agent-class (find-class 'agent nil))
	(human-class (find-class 'human nil)))
    (cond ((and human-class (typep agent human-class))
	   'human)
	  ((and agent-class (typep agent agent-class))
	   'agent))))

;; output a list of recent applications and paths
(defun sherpa-recent-apps (id)
  (sherpa-meta-more-start id)
  (with-output-to-sherpa
   (mapcar #'(lambda (name-path)
               (format t "~a~%" (car name-path))
               (format t "~a~%" (substitute #\/ #\\ (cadr name-path))))
           (get-recent-application-names-and-pathnames)))
  (sherpa-meta-end id))

;;respond to a heartbeat
(defun sherpa-heartbeat (id)
  (sherpa-meta-ack id))

;; load an application
(defun sherpa-load-app (id file)
  (without-errors
   (progn
     (sherpa-meta-more-start id)
     (if *application*
         (reset *application*))
     (load-application-file file)
     (write-line-sherpa "app-name ~a" (app-name *application*))
     (write-line-sherpa "app-native ~a"
                        (typep *application* 'native-sim-application))
     (sherpa-meta-end id))))

;; get apex server info
(defun sherpa-server-info (id)
  (sherpa-meta-more-start id)
    (with-output-to-sherpa
     (format t          "version ~a~%" *apex-version*)
     (format t    "multithreaded ~a~%" *multithreaded*)
     (format t   "implementation ~a~%" (lisp-implementation-type))
     (format t        "apex-home ~a~%" (pathname-to-string 
                                        (translate-logical-pathname "apex:")))
     (format t "agreement-signed ~a~%" (agreement-signed?)))
    (sherpa-meta-end id))

;; return the license agreement to sherpa
(defun sherpa-print-agreement (id)
  (sherpa-meta-more-start id)
  (with-output-to-sherpa
   (print-agreement))
  (sherpa-meta-end id))

(defun sherpa-sign-agreement (id)
  (sign-agreement)
  (sherpa-meta-ack id))

;; sets the show level
;; and prints a list of event types associated with the level
;; (! not really valid!)
(defun sherpa-set-show-level (id level-str)
  (sherpa-meta-more-start id)
  (let ((level (intern (string-upcase level-str))))
    (with-output-to-sherpa
     (cond ((lookup-show-level level)
          (set-trace-level level)
          (unless (eq level 'all)
            (dolist (item (extract-level-events level))
              (format t "~a~%" item))))
           
           ;; ! this should never happen, since show levels are
           ;; menu-selected, but not sure if this is the best error
           ;; action
           
           (t (user-error 'show (format nil "No show level named ~a" level)))))
    (sherpa-meta-end id)))

;; computes the event type list for a given show level.
;; ! this function does not have correct semantics, because a show
;; level is not strictly associated with a list of event types, but
;; rather a boolean composition of trace constraints (see
;; simulation/event.lisp for info.
(defun extract-level-events (level)
  (let ((leveldef (cdr (assoc level *show-levels*))))
    (when (and leveldef (eq 'or (first leveldef)))
      (loop for i in (rest leveldef)
	  if (and (consp i) (eq 'event-type (first i)))
	  collect (second i)))))

(defun pert-chart-available-p (object)
  (> (pert-size object) 0))


;; gen pert chart
(defun sherpa-pert-chart (id object-id)
  ;; if there is  pert chart, send it to sherpa
  (let ((object (find-instance object-id)))
    (if (and object
	     (pert-chart-available-p object))
      (progn
	(sherpa-meta-more-start id)
	(sherpa-pert object)
	(sherpa-meta-end id))))
    
    ;; otherwise send a nack
    (sherpa-meta-nack id))

(defun sherpa-get-pert-size (id agent-id)
  (sherpa-meta-more-start id)
  (let ((agent (find-agent-ancestor agent-id))
	(size 0))
    (if agent
	(setq size (pert-size agent)))
    (if (null size)
	(setq size 0))
    (with-output-to-sherpa
       (write-line-sherpa "~s" size)))
  (sherpa-meta-end id))

;;; get task agenda
(defun sherpa-get-agenda (id agent-id)
  (sherpa-meta-more-start id)
  (let ((agent (find-agent-ancestor agent-id)))
    (if agent
	(let ((atree (agenda-tree agent)))
	  (with-output-to-sherpa
	   (write-line-sherpa "~s" atree)))))
  (sherpa-meta-end id))

;;; get pdl
(defun sherpa-get-pdl (id agent-id)
  (sherpa-meta-more-start id)
  (let ((agent (find-agent-ancestor agent-id)))
    (if agent
	(let ((pdl-list (pdl-list agent))
	      (pdl-call-tree (get-pdl-call-tree agent)))
	  (with-output-to-sherpa
	   (write-line-sherpa "~s" pdl-list)
	   (write-line-sherpa "~s" pdl-call-tree)
	   ))))
  (sherpa-meta-end id))

(defun pdl-list (agent)
  (let ((procs (copy-list (procedures agent))))
    ;;just return sorted list of all procedures
    (pdl-list-small procs)))

(defun pdl-list-small (procs)
  ;;sort alphabetically
  (setq procs (sort procs #'proc-name-predicate))
  (list (mapcar #'proc-info procs)))

(defun proc-name-predicate (proc1 proc2)
   (string-lessp (format nil "~s" (index proc1))
		 (format nil "~s" (index proc2))))


(defun pdl-list-big (procs)
  ;;sort by bundle
  (setq procs (sort procs #' bundle-name-predicate))
  (let ((all-blists nil)
 (sub-blist nil))
    (loop for proc in procs
   for last-bname = (proc-bundle (first procs)) then bname
   for bname = (proc-bundle proc)
   do
   (cond ((eq bname last-bname)
   (push proc sub-blist))
  (t
   (push sub-blist all-blists)
   (setq sub-blist (list proc))
   )))
    (if sub-blist (push sub-blist all-blists))
    (setq all-blists
   (loop for l in all-blists
  collect (sort l #'proc-name-predicate)))
    (loop for l in all-blists
   collect (mapcar #'proc-info l))
    ))


(defun bundle-name-predicate (proc1 proc2)
   (string-lessp (format nil "~s" (proc-bundle proc1))
		 (format nil "~s" (proc-bundle proc2))))

(defun proc-bundle (proc)
  (let ((bname (bundle proc)))
    (if bname
	bname
      :global-bundle)))

;;returns list of (index-clause bundle pdl)
(defun proc-info (proc)
  (list (string (id proc)) (index proc) (proc-bundle proc) (pdl proc)))

;;;----------------------------------------------------------------------------
;;; support for call tree
;;;----------------------------------------------------------------------------

(defun get-pdl-call-tree (agent)
  (get-full-proc-tree (find-start-proc agent) (procedures agent)))

(defun find-start-proc (agent)
  (let ((procs (procedures agent))
	(start-task (initial-task agent)))
    (loop for proc in procs
	  when (pat-match (index proc) start-task)
	  return proc)))

(defun get-full-proc-tree (start-proc all-procs)
  (get-proc-tree start-proc (index start-proc) all-procs '() 0))


(defun create-proc-tree-list (index-clause proc-id disjunctNum alreadyVisited kid-list)
   (list index-clause (format nil "~a" proc-id) disjunctNum alreadyVisited kid-list))

(defun get-proc-tree (proc index-or-match-clause all-procs seen-procs disjunctNum)
  ;;(format t "get-proc-tree ~a~%" (index proc))
  (let ((index-clause (index proc))
	(id (id proc))
	psteps)
    (cond ((member proc seen-procs)
	   (create-proc-tree-list index-or-match-clause id disjunctNum t nil))
	  (t
	   (push proc seen-procs)
	   (cond ((or (typep proc 'primitive-procedure) (eq (proctype proc) 'special))
		  (create-proc-tree-list index-or-match-clause  id disjunctNum nil nil))
		 (t
		  ;;non-primitive, non special procedure
		  (setq psteps (steps proc))
		  ;;(format t "PPPROC=~a~%" proc)
		  (cond (psteps
			 (create-proc-tree-list index-or-match-clause id disjunctNum nil
						(get-kid-list-for-psteps psteps all-procs seen-procs 0))
			 )
			(t
			 (create-proc-tree-list index-or-match-clause id disjunctNum nil nil)))))))))


(defun get-kid-list-for-psteps (psteps all-procs seen-procs disjunctNum)
  (let ((kid-list '()))
    (loop for pstep in psteps
	  with proc-trees
	  do
	  (let ((matching-procs
		 (loop for proc in all-procs
		       for match = (pat-match (index proc) (activity pstep))
		       when (eq match no-bindings)  ;;exact match, no variables
		       return (list proc)
		       else when match
		       collect proc
		       )))
	    (if (and matching-procs (> (length matching-procs) 1))
		(setq disjunctNum (+ 1 disjunctNum)))
	  
	    (setq proc-trees 
		  (mapcar #'(lambda (proc) (get-proc-tree proc (activity pstep) all-procs seen-procs disjunctNum)) matching-procs))
	    (loop for proc-tree in proc-trees
		  do
		  (setq kid-list (append kid-list (list proc-tree)))
		  )
	    )
	  )

    kid-list
    ))

;;;----------------------------------------------------------------------------

;; signal the sherpa cammand handler to commit sepuku
(defun sherpa-disconnect (id)
  (sherpa-meta-ack id)
  :stop-command-handler)

;;;----------------------------------------------------------------------------
;;; application runtime control
;;;----------------------------------------------------------------------------

;; create the thread which starts the application running
(defun sherpa-app-play (id)
  (sherpa-meta-more id)
  (mp:process-run-function
      (list :name (app-name *application*)
            :initial-bindings (list (cons '*standard-output* *standard-output*)))
    #'sherpa-app-play-helper
    id))

;; run the application
(defun sherpa-app-play-helper (id)
  (without-errors
   (progn 
     (sherpa-meta-start id)
     (sherpa-start-simtime-reporter)
     (with-output-directed
         (startapp nil))
     (end-when-done id)))
  (sherpa-stop-simtime-reporter))

(defun end-when-done (id)
  (loop while *threads* do (sleep 0.1))
  (sherpa-meta-end id))

;; create the thread which starts the application running
(defun sherpa-app-step (id step-size)
  (sherpa-meta-more id)
  (mp:process-run-function
      (list :name (app-name *application*)
            :initial-bindings (list (cons '*standard-output* *standard-output*)))
    #'sherpa-app-step-helper id step-size))

;; step applicatin
(defun sherpa-app-step-helper (id step-size)
  (without-errors
   (progn
     ;;(setq step-size step-size)
     (sherpa-meta-start id)
     (sherpa-start-simtime-reporter)
     (with-output-directed
         (stepapp/pause nil))
     (sherpa-meta-end id)))
  (sherpa-stop-simtime-reporter))


;; pause applicatin
(defun sherpa-app-pause (id)
  (stopapp nil)
  (sherpa-meta-ack id))

;; reset applicatin
(defun sherpa-app-reset (id)
  (stopapp nil)
  (resetapp nil)
;;  (initapp)
  (sherpa-meta-ack id))

;; get application state
(defun sherpa-app-state (id)
  (sherpa-meta-more-start id)
  (let ((current-time (current-time)))
    (write-line-sherpa "~a ~a ~a" (state *application*) current-time (time-format current-time)))
  (sherpa-meta-end id))

;; create a thread which periodically reports the simulation time
;; back to sherpa

(defvar *simtime-reporter-proc* nil)
(defun sherpa-start-simtime-reporter ()
  ;;  (setq *simtime-reporter-alive* t)
  (setq *simtime-reporter-proc*
        (mp:process-run-function
         (list :name "Simulation Time Reporter"
               :initial-bindings (list 
                                  (cons '*standard-output* *standard-output*)))
         (lambda ()
           (loop while t do
           (sleep 0.5)
           (write-line-sherpa "[~a]" (current-time)))))))

;; stop thread which periodically reports the simulation time back to sherpa
(defun sherpa-stop-simtime-reporter ()
  (if *simtime-reporter-proc*
      (progn
        (mp:process-kill *simtime-reporter-proc*)
        (loop while (mp:process-active-p *simtime-reporter-proc*) do
              (sleep 0.5)))))

;;;----------------------------------------------------------------------------
;;; trace related functions
;;;----------------------------------------------------------------------------

;; set trace filter
;;OBSOLETE.... sherpa should call sherpa-set-trace-filter
;;leave for backward compatibility...
(defun sherpa-trace-filter (id filter)
  (set-trace-constraint filter)
  (sherpa-meta-ack id))

;; set trace filter
(defun sherpa-set-trace-filter (id event-types object-ids inc-desc-object-ids proc-list time-range)
  ;;event-types is either 'all or a list of event types
  ;;object-ids is a list of ids for which we don't include descendants
  ;;inc-desc-object-ids is a list of ids for which we include descendants
  ;; time-range is either nil or a time-range (eg. (time-range (0 50)))

  ;;empty constaints
  (show :level none)
  (let ((full-constraint (get-full-trace-filter event-types object-ids inc-desc-object-ids proc-list time-range)))
    (set-trace-constraint full-constraint)
    (sherpa-meta-ack id)
    ))

(defun get-full-trace-filter (event-types object-ids inc-desc-object-ids proc-list time-range)
  (let ((full-constraint '(and))
	ec oc odc)

    ;;add event type constaints
    (setq ec (cond ((eq event-types 'all)
		    nil)
		   (t
		    (loop for event-type in  event-types
				    collect  `(event-type ,event-type)))))
    (if ec
	(setq full-constraint (append full-constraint (list (cons 'or ec)))))


    ;; constraint for objects without descendants
    (setq oc (loop for object-id in object-ids 
		   collect (get-constraint-for-object object-id)))
   
	  
    ;; constraint for objects with descendants
    (setq odc (loop for object-id in inc-desc-object-ids 
		    for object-constaint = (get-constraint-for-object object-id)
		    for desc-constraint =  (compute-trace-constraint-for-descendants (find-instance object-id))
		    collect `(or ,object-constaint ,desc-constraint)))
  
    ;;constraint for procedures
    (setq pc (loop for proc in proc-list
		   collect `(task-type ,proc)))
    
    (if pc
	(setq full-constraint (append full-constraint (list (cons 'or pc)))))

    (cond ((and oc odc)
	   (setq full-constraint (append full-constraint (list (append (cons 'or oc) odc)))))
	  (oc
	    (setq full-constraint (append full-constraint (list (cons 'or oc)))))
	  (odc
	    (setq full-constraint (append full-constraint (list (cons 'or odc))))))
    
    (if time-range
	 (setq full-constraint (append full-constraint (list time-range))))
     
    full-constraint
    ))

(defun get-constraint-for-object (object-id)
  (let ((object (find-instance object-id)))
    (if object
	(cond ((typep object 'agent)
	       `(agent-id ,object-id))
	      (t
	       `(object-id ,object-id))))))

;; set trace destination
(defun sherpa-trace-destination (id dest)
  (setq *trace-destination* dest)
  (sherpa-meta-ack id))

;; output trace to the current trace destination
(defun sherpa-output-trace (id)
  (sherpa-meta-more-start id)
  (case *trace-destination*
    (sherpa (with-output-to-sherpa (sherpa-print-trace id)))
    (listener (sherpa-print-trace id))
    (otherwise (error "sherpa-trace: bad value of *trace-destination*")))
;;  (write-line-sherpa "[~a] (TRACE-END)" (current-time))
  (sherpa-meta-end id))

;; save trace to file
(defun sherpa-save-trace (id filename)
  (save-trace filename)
  (sherpa-meta-ack id))

;; print trace, send heartbeat every once in a while 
;; to keep sherpa from fussing
;;;(defun sherpa-print-trace (id)
;;;  (let ((trace-events (limit-events))
;;;        (count 0))
;;;    (dolist (trace-event trace-events)
;;;      (when (> count *sherpa-trace-count*)
;;;        (sherpa-meta-heartbeat id)
;;;        (setq count 0))
;;;      (incf count)
;;;      (format t "~a~%" trace-event))))
;;;
;;;(defun limit-events ()
;;;  (let ((the-events (events (generate-trace))))
;;;  (if (eq *trace-destination* 'sherpa)
;;;      (subseq the-events 0 (min (length the-events) *max-trace-events*)))
;;;  the-events))

(defun sherpa-print-trace (id)
  (let ((filter (trace-filter (get-current-trace-spec)))
	(count 0))
    (timeseries-for-each/filtered 
     (get-event-history)
     #'(lambda (value timepoint)
	 (declare (ignore timepoint))
	 (if (> count *sherpa-trace-count*)
	   (progn
	     (sherpa-meta-heartbeat id)
	     (setq count 0))
	   (incf count))
       (format t "~a~%" value))
     #'(lambda (value timepoint)
	 (declare (ignore timepoint))
	 (funcall filter value)))))
   

;;set time format to :hms or :runtime
(defun sherpa-set-time-format (id time-format)
  (cond ((eq time-format :hms) 
	 ;;(unshow :runtime)
	 (show :hms))
	(t
	 (unshow :hms)
	 ;;(show :runtime)
	 ))
  (sherpa-meta-ack id))

;;get counts for trace (filtered event count & total event count)
(defun sherpa-get-event-filter-count (id)
  (sherpa-meta-more-start id)
  (write-line-sherpa "~a" (get-trace-counts))
  (sherpa-meta-end id))


(defun get-trace-counts ()
  ;;filtered event count & total event count
;;  (format nil "~a ~a ~a"  (length (events (generate-trace))) (length (get-event-history)) *max-trace-events*))
  (format nil "~a ~a ~a"  (filter-events/count)  (ts-item-count (get-event-history)) *max-trace-events*))

;;return true iif object-id is a valid object id
(defun sherpa-is-valid-id (id object-id)
  (sherpa-meta-more-start id)
  (cond ((eq (find-instance object-id) nil)
	 (write-line-sherpa "~a" "false"))
	(t
	 (write-line-sherpa "~a" "true")))
  (sherpa-meta-end id))

;;get full path for object-id
(defun sherpa-get-object-path (id object-id)
  (sherpa-meta-more-start id)
  (let ((agent (find-agent-ancestor object-id)))
    (write-line-sherpa "~a ~a" (get-object-path object-id) (not (null agent))))
  (sherpa-meta-end id))

(defun get-object-path (object-id)
  (let ((object (find-instance object-id))
	ids)
    (cond (object
	   (setq ids (cons object-id (ancestor-ids object)))
	   (join-string-list (reverse ids)))
	  (t
	   object-id))))


(defun join-string-list (string-list)
    "Concatenates a list of strings and puts / between the elements."
    (format nil "~{~A~^/~}" string-list))


(defun sherpa-is-taskp (id obj-id)
  ;;returns t iff object with obj-id is of type 'task
  (sherpa-meta-more-start id)
  (let ((obj (find-instance obj-id)))
    (cond ((and obj (typep obj 'task))
	   (write-line-sherpa "~a" (format nil "~a" obj-id)))
	  (t
	   (write-line-sherpa ""))))
  (sherpa-meta-end id))

(defun sherpa-get-agent-for-object (id obj-id)
  (sherpa-meta-more-start id)
  (let ((agent (find-agent-ancestor obj-id)))
    (cond (agent
	   (write-line-sherpa "~a" (format nil "~a" (id agent))))
	  (t
	   (write-line-sherpa ""))))
  (sherpa-meta-end id))


(defun find-agent-ancestor (obj-id)
  (let ((obj (find-instance obj-id)))
    (if obj
	(cond ((typep obj 'agent)
	       obj)
	      ((typep obj 'task)
	       (agent obj))
	      (t
	       (find-agent-ancestor1 obj)))
      nil)))


(defun find-agent-ancestor1 (obj)
  (let ((parent-obj (appob-parent obj)))
    (cond ((typep obj 'agent)
	   obj)
	  (parent-obj
	   (find-agent-ancestor1 parent-obj))
	  (t 
	   nil))))

;;;**********************************************************************
;;; support for dot
;;;**********************************************************************
(defun sherpa-is-dot-available(id)
  (sherpa-meta-more-start id)
    (write-line-sherpa "~a" (if (dot-application-available-p) "true" "false"))
    (sherpa-meta-end id))

(defun sherpa-set-dot-location (id filename)
  (apex-set-dot-filename filename)
  (sherpa-meta-ack id))

;;;**********************************************************************
;;; support for diagram view
;;;**********************************************************************
(defun get-diagram-for-object (obj &optional view-type)
  (cond ((eq view-type 'tree)
	 (get-default-diagram obj))
	(t
	 (cond ((has-file-diagram-p obj)
		(get-svg-file-diagram obj))
	       ((has-diagram-p obj)
		(get-svg-diagram obj view-type))
	       (t
		(get-default-diagram obj))))))

(defun has-file-diagram-p (obj)
  (let ((gob (get-graphical-object obj)))
    (and gob (typep gob 'file-svg-object))))

(defun has-diagram-p (obj)
  (get-graphical-object obj))

(defun get-graphical-object (obj)
  (and (slot-exists-p obj 'graphical-object) (graphical-object obj)))

(defun get-svg-file-diagram (obj)
  ;;file based diagram
  (format nil "file~%~a~%" (filename (graphical-object obj))))  ;;indicate that diagram type is file, and output filename

(defun get-svg-diagram (obj view-type)
  ;;hierarchical diagram
  (let ((tree (get-descendants-breadth-first obj))
	(outstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
	(diagram-type "object-2d"))
    (cond ((and (find-class 'interface-object nil) (typep obj 'interface-object))
	   ;;interface-objects are 2d
	   (setq diagram-type "object-2d"))
	  ((and (find-class 'visob nil) (typep obj 'visob))
	   (if (null view-type)
	       (setq view-type 'xy))
	   (setq diagram-type (format nil "object-3d-~a" view-type)))
	  (t
	   (setq diagram-type "object-2d")))
    (with-output-to-string 
      (s outstr)
      (format s "~a~%" diagram-type) ;;indicate that diagram type is object-2d, object-3d-xy, object-3d-xz or object-3d-yz
      (loop for appob in tree
	    for gob = (and (slot-exists-p appob 'graphical-object) (graphical-object appob))
	    do
	    (cond (gob
		   ;;update coords of graphical object if necessary before converting to svg
		   (if (auto-update gob)
		       (setq gob (update-appob-graphical-object appob :view-type view-type)))
		   (format s "~s ~s ~a~%" (string (name appob)) (string (id appob)) (to-svg gob))))))
    outstr))

(defun get-default-diagram (obj)
  ;;default boxes & arrows diagram
  (let ((diagram (diagram-tree obj)))
    (format nil "default~%~s" diagram)))   ;;indicate that diagram type is default 


(defun diagram-tree (obj)
  (diagram-tree-to-depth obj 0 1)
  )

(defun diagram-tree-to-depth (obj my-depth max-depth) 
  (let ((kids (get-sorted-children obj)))
    (if (< my-depth max-depth) 
	(list (string (name obj)) (string (id obj)) 
	      (mapcar #'(lambda (kid) (diagram-tree-to-depth kid (+ 1 my-depth) max-depth))
		      kids))
      (list (string (name obj)) (string (id obj)) nil))))

;;command called from Sherpa to generate diagram
(defun sherpa-diagram (id obj-id &optional view-type)
  (let ((obj (find-instance obj-id)))
    (if obj
	(progn
	  (sherpa-meta-more-start id)
	  (with-output-to-sherpa 
	   (format t "~a~%" (get-diagram-for-object obj view-type)))
	  (sherpa-meta-end id))
      (sherpa-meta-nack id))))


;;;**********************************************************************
;;; support for object tree
;;;**********************************************************************

;; get all objects from the application
(defun sherpa-objects (id)
  (sherpa-meta-more-start id)

  ;; get the object tree
  (let ((object-tree (get-full-object-tree *application* )))
    ;; send either the object tree or "[none]" if no object tree found
    (write-line-sherpa "~s" (or object-tree '[none])))
  (sherpa-meta-end id))

(defun get-terse-object-tree (x)
  (get-object-tree x t #'otree-recurse-predicate))

(defun get-full-object-tree (x)
  (get-object-tree x))

(defun get-object-tree (x &optional (filter-empty-component-sets-p nil) (recurse-predicate nil))
    (let ((kids (if filter-empty-component-sets-p
		    (filter-empty-component-sets (appob-children x))
		  (appob-children x)))
	  (name (string (name x)))
	  (id (string (id x)))
	  (objType (string (get-apex-object-type x)))
	  (containsDiagram (and (has-diagram-p x) t))
	  )
      (cond ((and kids (or (null recurse-predicate) (funcall recurse-predicate x)))
	     (list id name objType containsDiagram 
		   (mapcar #'(lambda (kid) (get-object-tree kid filter-empty-component-sets-p recurse-predicate)) kids)))
	    (t
	     (list id name objType containsDiagram nil)))))

(defun otree-recurse-predicate (obj)
  ;;returns t iff obj is of NOT of type (resource-set task-agenda procedure-set monitor-array)
  ;;(we don't recurse down these objects when showing object tree in Sherpa)
  (not (or (typep obj 'procedure-set) (typep obj 'task-agenda) (typep obj 'resource-set) (typep obj 'monitor-array))))

(defun filter-empty-component-sets (x)
  (loop for item in x
	unless (and (typep item 'component-set) (null (appob-children item)))
	collect item))
 

(defun sherpa-get-object-type (id obj-id)
  (sherpa-meta-more-start id)
  (let ((obj (find-instance obj-id)))
    (cond (obj
	   (write-line-sherpa "~a" (format nil "~a" (get-apex-object-type obj))))
	  (t
	   (write-line-sherpa ""))))
  (sherpa-meta-end id))

(defun get-apex-object-type (obj)
  ;;Note that for object where we care about type in Sherpa we return a known name instead of just type-of 
  ;;this keeps sherpa from having to know about class name changes... assuming we modify typep correctly
  (cond ((typep obj 'application) 
	 'application)
	((and (find-class 'human nil) (typep obj 'human))
	 'human)
	((typep obj 'agent) 
	 'agent)
	((typep obj 'task-agenda)
	 'task-agenda)
	((typep obj 'procedure-set)
	 'procedure-set)
	((typep obj 'procedure)
	 'procedure)
	((typep obj 'monitor-array)
	 'monitor-array)
	((typep obj 'monitor)
	 'monitor)
	((typep obj 'native-sim-application)
	 'native-sim-application)
	(t
	 (type-of obj))))


(defun find-object-by-name (name)
    (loop for class-items in (cdr *instance-registry*)
	  for class-name = (car class-items)
	  for items = (cdr class-items)
	  with found-obj = nil
	  unless (eq class-name 'task)
	  do 
	  (setq found-obj (loop for item in items 
				for obj = (cdr item)
				for objName = (name obj)
				when (if (symbolp objName)
					 (string-equal (string objName) name)
				       (string= objName name))
 				do  (return (cdr item))
				))
	  (if found-obj
	      (return found-obj))))

;;searched in instance-registry for object with name=objName, if object is found, return the objectId 
(defun sherpa-get-object-by-name (id objName) 
   (sherpa-meta-more-start id)
   (let ((obj (find-object-by-name objName)))
    (cond (obj
	   (write-line-sherpa "~a" (format nil "~a" (id obj))))
	  (t
	   (write-line-sherpa ""))))
   (sherpa-meta-end id))



;;;**********************************************************************
;;; support for monitor diagram
;;;**********************************************************************

(defun sherpa-get-monitor-diagram (id monitor-id)
  (sherpa-meta-more-start id)
  (let ((dot-available (dot-application-available-p))
	filename)
    (if dot-available
	(setq filename  (monitor-to-svg-file (find-instance monitor-id))))
    (cond ((and dot-available filename)
	   (with-output-to-sherpa
	    (write-line-sherpa "~a" filename)))
	  (t
	   (write-line-sherpa ""))))
  (sherpa-meta-end id))

;;;**********************************************************************
;;; support for state variable view
;;;**********************************************************************

(defun sherpa-get-state-variable-info (id obj-id include-task-svs)
  (sherpa-meta-more-start id)
  (let ((agent (find-agent-ancestor obj-id))
	(pathname nil))
    (if agent
	(setq pathname (agent-sv-memory-to-csv agent (system:make-temp-file-name) include-task-svs)))
    (with-output-to-sherpa
     (cond (pathname
	    (write-line-sherpa "~a" pathname))
	   (t
	    (write-line-sherpa "")))))
  (sherpa-meta-end id))


(defun sherpa-export-state-variable-info (id pathname obj-id include-task-svs)
  (sherpa-meta-more-start id)
  (let ((agent (find-agent-ancestor obj-id))
	(ret-pathname nil))
    (if agent
	(setq ret-pathname (agent-sv-memory-to-csv agent pathname include-task-svs)))
    (with-output-to-sherpa
     (cond (ret-pathname
	    (write-line-sherpa "~a" ret-pathname))
	   (t
	    (write-line-sherpa "")))))
  (sherpa-meta-end id))

;;**********************************************************************
;;  move to hierarchy.lisp
;;**********************************************************************

(defun get-sorted-children (x)
   (sort (appob-children x) #'name-or-id-predicate))

(defun name-or-id-predicate (obj1 obj2)
  (let ((n1 (or (name obj1) (id obj1)))
	(n2 (or (name obj2) (id obj2))))
   (string-lessp n1 n2)))

(defun get-descendants-breadth-first (x)
  (get-descendants-breadth-first1 (appob-children x) (list x)))

(defun get-descendants-breadth-first1 (queue result-list)
  (cond (queue
	 (let* ((parent (pop queue))
		(kids (appob-children parent)))
	   (nconc result-list (list parent))
	   (setq queue (nconc queue kids))
	   (get-descendants-breadth-first1 queue result-list)))
	(t
	 result-list)))

;;**********************************************************************
;; function for setting step pause options
(defun sherpa-set-pause-num-steps (id numSteps)
  ;;clear other settings
  (set-pause-time nil)
  (set-pause-interval nil)
  ;;set
  (set-pause-cycle numSteps)
  (sherpa-meta-ack id))

(defun sherpa-set-pause-time-point (id dur-exp)
  ;;clear other settings
  (set-pause-cycle nil)
  (set-pause-interval nil)
  ;;set
  (set-pause-time dur-exp)
  (sherpa-meta-ack id))
  
(defun sherpa-set-pause-time-interval (id dur-exp)
  ;;clear other settings
  (set-pause-cycle nil)
  (set-pause-time nil)
  ;;set
  (set-pause-interval dur-exp)
  (sherpa-meta-ack id))

(defun sherpa-is-valid-duration-expression (id dur-exp)
  (sherpa-meta-more-start id)
  (write-line-sherpa "~a" 
		     (if (duration-expression? dur-exp) "true" "false"))
  (sherpa-meta-end id))

(defun sherpa-is-past-pause-timepoint (id)
  (let ((pt (pause-time *application*)))
    (sherpa-meta-more-start id)
    (write-line-sherpa "~a" (if (and pt (>= pt (current-time))) "true" "false"))
    (sherpa-meta-end id)))

(defun sherpa-get-app-info (id)
  (sherpa-meta-more-start id)
  (write-line-sherpa "~a ~a ~a ~a ~a ~a" 
		     (boolean-val (typep *application* 'realtime-application))
		     (boolean-val (pausable *application*))
		     (boolean-val (stepable *application*))
		     (boolean-val (pause-time-setter *application*))
		     (boolean-val (pause-interval-setter *application*))
		     (boolean-val (pause-cycle-setter *application*))
		     )
  (sherpa-meta-end id))

(defun boolean-val(x)
  (if (null x)
      "false"
    "true"))

;;;**********************************************************************
;;; support for displaying view after running/stepping app
;;;**********************************************************************
(defmacro with-step (id &body forms)
 `(let (heart-throb-proc)
     (unwind-protect 
       (progn
	 (setq heart-throb-proc (sherpa-start-heart-throbbing ,id))
	 (let ((*standard-output* (make-string-output-stream)))
	   ;;send standard-out for stepapp to neverneverland, not to listener
	   (stepapp/pause nil)))
       (if heart-throb-proc
	   (sherpa-stop-heart-throbbing heart-throb-proc)))
     ,@forms))

(defmacro with-run (id &body forms)
  `(let (heart-throb-proc)
     (unwind-protect
       (progn
	 (setq heart-throb-proc (sherpa-start-heart-throbbing ,id))
	 ;;send standard-out for startapp to neverneverland, not to listener
	 (let((*standard-output* (make-string-output-stream)))
	   (startapp nil))
	 )
       (if heart-throb-proc
	   (sherpa-stop-heart-throbbing heart-throb-proc)))
     ,@forms
     ))

;; create a thread which periodically sends a heartbeat to sherpa
(defun sherpa-start-heart-throbbing (id)
  (let ((heart-throb-proc
	 (mp:process-run-function
	  (list :name "Heart Throbber"
		:initial-bindings (list 
				   (cons '*standard-output* *standard-output*)))
	  (lambda ()
	    (loop while t do
		  (sleep 10)
		  (sherpa-meta-heartbeat id))))))
    heart-throb-proc))

;; stop thread which periodically reports the simulation time back to sherpa
(defun sherpa-stop-heart-throbbing (heart-throb-proc)
  (if heart-throb-proc
      (progn
        (mp:process-kill heart-throb-proc)
        (loop while (mp:process-active-p heart-throb-proc) do
              (sleep 0.5)))))

(defun sherpa-silent-play (id)
  (sherpa-meta-more id)
  (mp:process-run-function
   (list :name (app-name *application*)
	 :initial-bindings (list (cons '*standard-output* *standard-output*)))
    #'sherpa-silent-play-helper id))

;; run the application
(defun sherpa-silent-play-helper (id)
  (let (heart-throb-proc
	;;send standard-out for startapp to neverneverland, not to listener
	(*standard-output* (make-string-output-stream)))
    (unwind-protect 
      (progn 
	(setq heart-throb-proc (sherpa-start-heart-throbbing id))
	(sherpa-meta-start id)
	(startapp nil)
	(sherpa-stop-heart-throbbing heart-throb-proc)
	(setq heart-throb-proc nil)
	(sherpa-meta-end id)
	)
      (if heart-throb-proc
	  (sherpa-stop-heart-throbbing heart-throb-proc)))))


(defun sherpa-silent-step (id)
  (sherpa-meta-more id)
  (mp:process-run-function
      (list :name (app-name *application*)
            :initial-bindings (list (cons '*standard-output* *standard-output*)))
    #'sherpa-silent-step-helper id))

;; step applicatin
(defun sherpa-silent-step-helper (id)
  (let (heart-throb-proc
	;;send standard-out for startapp to neverneverland, not to listener
	(*standard-output* (make-string-output-stream)))
    (unwind-protect 
      (progn 
	(setq heart-throb-proc (sherpa-start-heart-throbbing id))
	(sherpa-meta-start id)
	(stepapp/pause nil)
	(sherpa-stop-heart-throbbing heart-throb-proc)
	(setq heart-throb-proc nil)
	(sherpa-meta-end id)
	)
      (if heart-throb-proc
	  (sherpa-stop-heart-throbbing heart-throb-proc)))))

;;;**********************************************************************
