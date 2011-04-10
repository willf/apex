;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/sherpa/pert/pertier.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: pertier.lisp,v 1.5 2006/01/15 03:43:02 dalal Exp $
;;; Created:        June, 2005

;;;
;;; PERT Charts.
;;;
;;; This file replaces the previous two files pert.lisp and cp.lisp
;;; It produces Pert charts for Sherpa and for DOT output.
;;; 
;;; 
;;; --------------------------------------------------------------------

(in-package :common-lisp-user)


;;; helper functions


(defun ht-keys (ht) ;; hashtable -> list
  "Returns all keys of a hash table"
  (loop for key being the hash-key in ht collecting key))

(defun ht-values (ht) ;; hashtable -> list
  "Returns all values of a hash table"
  (loop for value being the hash-value in ht collecting value))

(defmethod task-isa ((child task) (parent task)) ;; task x task -> bool
  "Is child a subtask of parent?"
  (or (eq child parent)
      (member parent (task-ancestors child))))

(defmethod task-ancestors ((task task)) ;; task -> list(task)
  "Returns all task ancestors of task (not including self or root)"
  (let ((parent (parent task)))
    (if (or (eql parent task)
	    (null parent)
	    (equal (description task) '(root))
	    (equal (description parent) '(root)))
      nil
      (cons parent (task-ancestors parent)))))

(defmethod primitive-task-p ((task task)) ;; task -> bool
  "Is this object a primitive task?"
  (eql (tasktype task) 'primitive))

(defmethod primitive-task-p ((object t)) ;; object -> bool
  "Is this object a primitive task?"
  nil)

(defun all-top-level-tasks (agent) ;; agent -> list(task)
  (remove-duplicates 
   (append 
    (top-level-tasks (tasks agent))
    (completed-top-level-tasks agent))))

;;;
;;; -- A *pert* chart:
;;;     -- a set of resources, 
;;;     -- a set of fragments,
;;;     -- a set of resource dependency lists, 
;;;     -- a set of logical dependency edges. 
;;;   Additional information collected is:
;;;     -- the top level task being examined,
;;;     -- its requested interval,
;;;     -- a list of all edges,
;;;     -- partition of edges by after 
;;;     -- a table of color numbers (for Sherpa)
;;;    list of all edges.

(defclass pert ()
  ((resources :initarg :resources :initform nil :accessor resources
	      :documentation "Set of resources")
   (fragments :initarg :fragments :initform nil :accessor fragments
	      :documentation "Set of fragments")
   (resource-dependency-lists
    :initarg :resource-dependency-lists
    :initform nil
    :accessor resource-dependency-lists
    :documentation "Set of set of reesource dependencies")
   (logical-dependencies
    :initarg :logical-dependencies
    :initform nil
    :accessor logical-dependencies
    :documentation "Set of logical dependencies")
   (task :initarg :task :initform nil :accessor task
	 :documentation "task under examniation")
   (interval :initarg :interval :initform nil :accessor interval
	     :documentation "Interval under examination")
   (edges :initarg :edges :initform nil :accessor edges
	  :documentation "All edges")
   (dependency-table :initarg :dependency-table :initform (make-hash-table :test 'eq)
		     :accessor dependency-table
		     :documentation "table of of dependencies")
   (color-table :initarg :color-table :initform (make-hash-table :test 'eq)
		:accessor color-table 
		:documentation "Colors of 'template' task->color integer")
   ))

(defmethod print-object ((chart pert) (stream stream))
  (print-unreadable-object (chart stream :identity t :type t)
    (format stream 
	    "~a fragments on ~a"
	    (length (fragments chart))
	    (task chart))))

(defmethod  create-pert ((task task) &optional (interval (task-interval task))) 
  "Create an pert object from task + inteval"
  ;; task X interval -> pert
  ;; Note: the order of these steps need to be done pretty much in
  ;; this order.
  (let ((chart (make-instance 'pert
		 :fragments (all-task-fragments task interval)
		 :task task
		 :interval interval)))
    
    ;; Resource dependencies
    (setf (resource-dependency-lists chart)
      (calculate-resource-dependency-lists (fragments chart)))
    
    ;; from that list, it's easy to calculate resources...
    (setf (resources chart)
      (resource-list-from-resource-dependency-lists
       (resource-dependency-lists chart)))

    ;; logical dependencies... depends on calculation of resource
    ;; dependencies. 
    (setf (logical-dependencies chart)
      (calculate-logical-dependencies (fragments chart)))
    
    ;; record all edges ... need both resource & logical dependencies
    ;; for this. 
    
    (setf (edges chart)
      (calculate-all-edges chart))
    
    ;; now calculate critical path nodes. This depends on edges.
    
    (calculate-critical-path chart)
    
    ;; sherpa wants dependency partition, create it
    
    (setf (dependency-table chart)
      (calculate-dependency-table chart))
    
    ;; define colors of 'template' tasks
    
    (setf (color-table chart)
      (calculate-color-table chart))
    
    chart))


(defun top-level-pert (agent) ;; agent -> pert (based on first top
  ;; level task)
  (create-pert (car (all-top-level-tasks agent))))


;;; almost a helper function ... from a list of lists of resource dependencies
;;; pull out the resources -- which are the source (before) 
;;; dependency of each edge.
(defun resource-list-from-resource-dependency-lists (lists)
  (mapcar (lambda (edges)
		(edge-before (car edges)))
	  lists))

;;; -- a *fragment* is a particular interval of non-interrupted task
;;; execution on a resource. a *task* is broken up into multiple
;;; fragments -- at least one fragment, of course! Fragments are
;;; numbered sequentially.  Note that a fragment uses exactly one
;;; resource.
;;;  

(defclass fragment (id-mixin)
  ((task :initarg :task :initform nil :accessor task
	 :documentation "Pointer to primitive task of which this is a fragment.")
   (start :initarg :start :initform nil :accessor start-of
	  :documentation "Starting time of fragment")
   (end :initarg :end :initform nil :accessor end-of
	:documentation "End time of fragment")
   (number :initarg :number :initform nil :accessor fragment-number
	   :documentation "Nth fragment of task")
   (ls :initarg :ls :initform nil :accessor ls 
       :documentation  "Latest start")
   (lf :initarg :lf :initform nil :accessor lf
        :documentation  "Latest finish")
   (es :initarg :es :initform nil :accessor es
        :documentation  "Earliest start")
   (ef :initarg :ef :initform nil :accessor ef
        :documentation  "Earliest finish")
   (resource :initarg :resource :initform nil :accessor resource
	     :documentation "Associated resource")
   ))

(defun make-fragment (task start end frag-no resource)
  (make-instance 'fragment
    :task task
    :start start
    :end end
    :number frag-no
    :resource resource))

(defun fragment-p (x) ; object -> bool
  (typep x 'fragment))

(defmethod slack ((fragment fragment)) ;; fragment -> real or NIL
  (if (and (ls fragment)
	   (es fragment))
    (- (ls fragment) (es fragment))
    nil))

(defmethod fragment-duration ((fragment fragment)) ;; fragment -> real
  (- (or (end-of fragment) 
	 (current-time))
     (or (start-of fragment) 0)))

;; eql test, because slack might be NIL if ES LS not defined.
(defmethod on-critical-path-p ((fragment fragment)) ;; fragment -> bool
  (eql (slack fragment) 0))

(defmethod print-object ((fragment fragment) (stream stream))
  (print-unreadable-object (fragment stream :identity t :type t)
    (format stream 
	    "id: ~a ~a ~a ~s/~s"
	    (num fragment)
	    (fragment-number fragment)
	   (if (task fragment)
	     (task fragment)
	     "None")
	   (start-of fragment)
	   (end-of fragment)
	   )))

;;;
;;; -- Code for finding all fragments of a top level task (within
;;;    some inteval).
;;;


(defmethod task-interval ((task task)) ;; task -> interval.
  (make-interval (or 
		  (start-of task)
		  0)
		 (or (end-of task)
		     +end-of-time+)))

(defmethod all-task-fragments ((task task) &optional (interval (task-interval task)))
  ;; task x interval -> list(fragments)
  (all-task-fragments-in-interval task interval))

;;; we only collect fragments that use resources, and which
;;; inherit from the parent task.
(defmethod all-task-fragments-in-interval ((task task) (interval interval))
  (mappend 
   (lambda (h)
     (let ((primitive-task (sv-object (sv-history-sv h))))
       (if (and (resources primitive-task)
		(task-isa primitive-task task))
	 (fragments-from-history h interval))))
   (primitive-task-histories task)))

;;; collect state variable histories in the member of the task's agent
(defmethod primitive-task-histories ((task task))
  (filter (lambda (history)
	    (primitive-task-p (sv-object (sv-history-sv history))))
	  (gethash 'state (sv-memory-table (agent-sv-memory (agent task))))))

;;;
;;; heart of the calculation of fragments. we look for transitions
;;; from ongoing -> suspended terminated enabled pending
;;; to create a fragment.
(defmethod fragments-from-history ((sv-history sv-history) (interval interval)) 
  (let ((primitive-task (sv-object (sv-history-sv sv-history))))
    (assert (primitive-task-p primitive-task))
    (let ((fragments (list))
	  (fragment-count 0)
	  (current-fragment nil))
      (sv-history-for-each 
       sv-history interval
       (lambda (measurement) 
	 ;; (pvs! "Checking" (measurement-sv measurement) (measurement-value measurement))
	 (case (measurement-value measurement) 
	   ((ongoing)
	    (when current-fragment
	      (warn "Creating new fragment on ~a although one is current."
		    primitive-task))
	    (setq current-fragment 
	      ;; this is a kind of 'super fragment' because we
	      ;; are ignoring resources for now.
	      (make-fragment primitive-task
			     (measurement-timestamp measurement)
			     nil ;; end time
			     (incf fragment-count)
			     nil ;; for now -- set fragment to nil
			     )))
	   ((suspended terminated enabled pending) ;; enabled?? pending??
	    (when current-fragment ;; might get terminated before it starts..
	      (setf (end-of current-fragment)
		(measurement-timestamp measurement))
	      (push current-fragment fragments)
	      (setq current-fragment nil))))))
      ;; might reach end without finishing fragment ... that's ok
      ;;--
      ;; now create 'real' fragments -- one for each resource+super-fragment
      (unresourced-fragments-to-resourced-fragments (nreverse fragments)))))

;;; given a list of fragments w/o resources attached, generate a list of 
;;; fragments w/ each resource attached...
(defmethod unresourced-fragments-to-resourced-fragments (fragments)
  (mappend 
   (lambda (f)
     (mapcar
      (lambda (resource)
	(make-fragment 
	 (task f)
	 (start-of f)
	 (end-of f)
	 (fragment-number f)
	 resource))
      (resources (task f))))
   fragments))
     

;;; defining pert edges between a resource or fragment and fragment.
;;; if a,b are fragments, a -> b means b can't start until a
;;; completes.  if a is a resource, a -> b means a is 'source'
;;; resource for b.  a -> b means b can not complete until after a
;;; ends, if both are.
;;;

(defclass pert-edge ()
  ((before :initarg :before :initform nil :accessor edge-before 
	   :documentation "enabler")
   (after  :initarg :after  :initform nil :accessor edge-after
	   :documentation "dependant")
   (type   :initarg :type   :initform :resource :accessor edge-type
	   :documentation ":resource or :logical (or NIL)")))


(defun make-edge (before after &optional type)
  (make-instance 'pert-edge
    :before before
    :after after
    :type type))

(defmethod on-critical-path-p ((edge pert-edge))
  (and (on-critical-path-p (edge-before edge))
       (on-critical-path-p (edge-after edge))))

(defmethod print-object ((edge pert-edge) (stream stream))
  (print-unreadable-object (edge stream :identity t :type t)
    (format stream "~a -> ~a" (edge-before edge) (edge-after edge))))


;;; ---
;;; ---
;;; defining *resource* dependencies
;;; ---
;;; ---


(defmethod resources ((fragment fragment))
  (resources (task fragment)))

(defmethod resources ((task task))
  (mapcar #'car (profile task)))

(defun sorted-fragments (fragments)
  (sort (copy-list fragments) #'< :key 'start-of))

;;; returns a hash table whose keys are resources,
;;; and whose values are fragments that use the resource.
;;; the values are in sorted order.
;;; these must be fragments w/a resource attached.
(defun partition-fragments-by-resource (resourced-fragments)
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (fragment resourced-fragments)
      (push fragment (gethash (resource fragment) ht)))
    (dolist (key (ht-keys ht))
      (setf (gethash key ht)
	(sorted-fragments (gethash key ht))))
    ht))

;;;
;;; returns LIST of LISTS of dependency edges
;;;  the car of each list is the first resource->fragment edge
;;;  the cdr of each list are the subsequent fragment->fragment edges
(defun calculate-resource-dependency-lists (fragments)
  (let ((table (partition-fragments-by-resource fragments)))
    (mapcar 
     (lambda (resource)
       (let ((rfragments (gethash resource table)))
	 (cons (make-edge resource (car rfragments) :resource)
	       (loop for before in rfragments
		   for after in (cdr rfragments)
		   collect
		     (make-edge before after :resource)))))
     (ht-keys table))))


;;; ---
;;; ---
;;; defining *logical* dependencies
;;; ---
;;; ---

;;; todo: add ssupend, etc.
(defmethod all-monitors ((task task))
  (car (monitors task)))

(defmethod appears-in? ((task task) (monitor monitor))
  nil)

(defmethod appears-in? ((task task) (monitor measurement-monitor))
  (and (pat-match `(state ,task = terminated)
		  (expr monitor))
       t))

(defmethod appears-in? ((task task) (monitor atomic-episode-monitor))
  (and (pat-match `(terminated ,task)
		  (expr monitor))
       t))

(defmethod appears-in? ((task task) (monitor allen/and-monitor))
  (some (lambda (subm)
	  (appears-in? task subm))
	(submonitors monitor)))

(defmethod immediate-dependency? ((before task) (after task))
  (some (lambda (monitor)
	  (appears-in? before monitor))
	(all-monitors after)))

(defmethod logical-edge? ((before task) (after task))
  (if (immediate-dependency? before after)
    t
    (progn
      (dolist (a1 (cons after (task-ancestors after)))
	(dolist (a2 (cons before (task-ancestors before)))
	  ;; eq check is just a shortcut.
	  (when (and (not (eq a1 a2)) (immediate-dependency? a2 a1))
	    (return-from logical-edge? t))))
      nil)))

;;; the last fragment of a test to the first fragment of another task.
;;; todo: check if this works for repititions!
(defmethod logical-edge? ((before fragment) (after fragment))
  (and (= (fragment-number after) 1)
       (= (fragment-number before)
	  (fragment-number (task before)))
       (logical-edge? (task before) (task after))))



;;;
;;; first, we figure out all possible logical edges. then, we
;;; filter out ones we don't need.
;;; Note: filter 1 -- logical dependencies on the same resource --
;;; we remove here in place.
;;;

(defun all-logical-dependencies (fragments)
  (let ((edges))
    (dolist (before fragments)
      (dolist (after fragments)
	(when (and 
	       (not (eq (resource before)
			(resource after)))
	       (logical-edge? before after))
	  (push (make-edge before after :logical) edges))))
    edges))


;;;
;;; filters.
;;;
;;; nomenclature: f_i -> f_j: there exists a logical dependency between fragments f_i
;;; and f_j; f_j depends on f_i (couldn't start until f_i finishes).
;;;
;;; r(f) : resource of fragment. 


;;; Filter 1.
;;;
;;; if there are any logical dependencies which are covered by resource
;;; dependencies, remove them.
;;;
;;; f_i -> f_j; r(f_i)=r(f_j)
;;;
;;;
;;; Note: it is more efficient to just calculate this when creating all
;;; logical dependencies, so we do this above.
;;;
;;;
;;;(defun filter-same-resource-edges (edges)
;;;  (filter (lambda (edge)
;;;	    (not 
;;;	     (eq (resource (edge-before edge))
;;;		 (resource (edge-after edge)))))
;;;	  edges))

;;;
;;; filter 2.
;;;
;;; a fragment points to two fragments on the same resource
;;; remove the second edge. 
;;; 
;;; f_i -> f_j; f_i -> f_k; f_k after f_j; 
;;; r(f_j)==r(f_k); r(f_i) <> r(f_j); r(f_i) <> r(f_k);
;;; remove f_i -> f_k
;;;
;;; r -> ...  f_j ... f_k
;;;            ^      ^
;;;            |     /
;;;            |    /
;;;            |   / -- filter out this link.
;;;            |  /
;;;            | /    
;;;            |/
;;; r'-> ...  f_i
;;
;;; sort by start time of 'before' edge. put resource edge
;;; before anything else -- which (by definition of 'time') is
;;; zero. 
;;; 
;;; to do this, we partition all the edges by the before edge,
;;; sort each partiion, and remove duplicates 

(defun remove-extraneous-afters (edges)
  (remove-duplicates edges
		     :from-end t
		     :key 
		     (lambda (edge)
		       (resource (edge-after edge)))))

(defun sorted-edges/before (edges)
  (sort (copy-list edges) #'< :key (lambda (edge) 
				     (if (fragment-p (edge-before edge))
				       (start-of (edge-before edge))
				       -1))))

(defun partition-logical-edges-by-before (edges)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (edge edges)
      (push edge (gethash (edge-before edge) ht)))
    (dolist (key (ht-keys ht))
      (setf (gethash key ht)
	(sorted-edges/before (gethash key ht))))
    ht))

(defun filter-before-resource-edges (edges)
  (let ((ht (partition-logical-edges-by-before edges)))
    (mappend 
     (lambda (before)
       (remove-extraneous-afters (gethash before ht)))
     (ht-keys ht))))

;;; filter 3.
;;;
;;; two fragments on the same resource point to the same
;;; fragment on a different fragment.
;;; remove the second edge. 
;;; 
;;; f_j -> f_i; f_k -> f_i; f_k after f_j; 
;;; r(f_j)==r(f_k); r(f_i) <> r(f_j); r(f_i) <> r(f_k);
;;; remove f_i -> f_k
;;;
;;; r -> ...  f_j ... f_k
;;;             \      |
;;;              \     |
;;;               \    | -- filter out this link.
;;;                \   |
;;;                 \  |
;;;                  \ |
;;;                   VV
;;; r'-> ...          f_i
;;
;;; sort by start time of 'before' edge. put resource edge
;;; before anything else -- which (by definition of 'time') is
;;; zero. 
;;; 
;;; to do this, we partition all the edges by the after edge,
;;; sort each partiion, and just keep the first edge in each partition.
;;;
;;; fit

(defun remove-extraneous-befores (edges)
  (remove-duplicates edges
		     :from-end t
		     :key 
		     (lambda (edge)
		       (resource (edge-before edge)))))

(defun partition-logical-edges-by-after (edges)
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (edge edges)
      (push edge (gethash (edge-after edge) ht)))
    (dolist (key (ht-keys ht))
      (setf (gethash key ht)
	(sorted-edges/before (gethash key ht))))
    ht))

(defun filter-after-resource-edges (edges)
  (let ((ht (partition-logical-edges-by-after edges)))
    (mappend
     (lambda (before)
       (remove-extraneous-befores (gethash before ht)))
     (ht-keys ht))))

;;;
;;; -- ok, apply filters.
;;;
(defun calculate-logical-dependencies (fragments)
  (filter-after-resource-edges
   (filter-before-resource-edges 
    (all-logical-dependencies fragments))))


;;; just collects all edges in chart.
(defmethod calculate-all-edges ((chart pert))
  (append
   (apply 'append (resource-dependency-lists chart))
   (logical-dependencies chart)))


;;; creates partition table of edges by dependent.
(defmethod calculate-dependency-table ((chart pert))
  (with-slots (edges) chart
    (let ((aht (make-hash-table :test 'eq)))
      (dolist (edge edges)
	(when (and (typep (edge-before edge) 'fragment)
		   (typep (edge-after edge) 'fragment))
	  (push edge
		(gethash (edge-after edge) aht))))
      (dolist (key (ht-keys aht))
	(setf (gethash key aht)
	  (sorted-edges/before (gethash key aht))))
      aht)))

(defmethod pert-dependent-edges ((chart pert) (fragment fragment))
  (gethash fragment (dependency-table chart)))


;;; might override this...
(defmethod template-task ((app application) (task task))
  (parent (parent task)))

;;; don't override this.
(defmethod template-task ((app application) (fragment fragment))
  (template-task app (task fragment)))

(defmethod calculate-color-table ((chart pert))
  (let ((counter -1)
	(ht (make-hash-table :test 'eq)))
    (dolist (fragment (fragments chart))
      (let ((template (template-task *application* fragment)))
	(unless (gethash template ht)
	  (setf (gethash template ht)
	    (incf counter)))))
    
    ht))

;;; Colors of tasks ...
(defmethod pert-color ((chart pert) (task task))
  (or (gethash (template-task *application* task)
	       (color-table chart))
      1))

(defmethod pert-color ((chart pert) (fragment fragment))
  (pert-color chart (task fragment)))




;;; critical path calculation
;;; Base on code in old CP.lisp, and 
;;; www.cob.sjsu.edu/facstaff/davis_r/courses/qbareader/cpmalgorithm.html
;;;
;;; because we use 'dummy nodes' to calculate critical path info,
;;; we'll be a bit redundant
;;; Critical path is pretty easy to calculate, and this code should
;;; probably be less opaque.

;;; this is a critical path structure. 
;;; bht -> before hash table (partition of edges by before)
;;; aht -> after hash table (partition of edges by after)
;;; est/lst -> earliest/latest start table (fragment -> earliest/latest start)
;;; eft/lft -> earliest/latest finish table (fragment -> earliest/latest finish)
;;; pert -> link back to pert chart.


(defstruct cp bht aht est eft lst lft pert)

(defun create-cp-struct (pert)
  (let ((cp (make-cp :pert pert)))
    (define-cp-adj-tables cp)
    (define-cp-start-end-tables cp)
    cp))

(defun define-cp-adj-tables (cp)
  (with-slots (fragments edges) (cp-pert cp)
    (let ((bht (make-hash-table :test 'eq))
	  (aht (make-hash-table :test 'eq)))
      (dolist (edge edges)
	(push (if (typep (edge-before edge) 'fragment)
		(edge-before edge)
		'beginning-of-process)
	      (gethash (edge-after edge) bht))
	(push (edge-after edge)
	      (gethash (edge-before edge) aht)))
    ;; mark beginning and end .
      (dolist (fragment fragments)
	(unless (gethash fragment bht)
	  (setf (gethash fragment bht)
	    (list 'beginning-of-process)))
	(unless (gethash fragment aht)
	  (setf (gethash fragment aht)
	    (list 'end-of-process))))
      
      (setf (cp-bht cp) bht)
      (setf (cp-aht cp) aht)
      cp)))

(defun define-cp-start-end-tables (cp)
  (setf (cp-est cp) (make-hash-table :test 'eql))
  (setf (cp-eft cp) (make-hash-table :test 'eql))
  (setf (cp-lst cp) (make-hash-table :test 'eql))
  (setf (cp-lft cp) (make-hash-table :test 'eql))
  cp)

(defun cp-priors (node cp)
  (gethash  node (cp-bht cp)))

(defun cp-afters (node cp)
  (gethash node (cp-aht cp)))

(defun cp-es (node cp)
  (if (eq node 'beginning-of-process) 0
      (gethash node (cp-est cp))))

(defun (setf cp-es) (value node cp)
  (setf (gethash node (cp-est cp)) value))

(defun cp-ls (node cp)
  (if (eq node 'beginning-of-process) 0
      (gethash node (cp-lst cp))))

(defun (setf cp-ls) (value node cp)
  (setf (gethash node (cp-lst cp)) value))

(defun cp-lf (node cp)
  (if (eq node 'beginning-of-process) 0
      (gethash node (cp-lft cp))))

(defun (setf cp-lf) (value node cp)
  (setf (gethash node (cp-lft cp)) value))

(defun cp-ef (node cp)
  (if (eq node 'beginning-of-process) 0
      (gethash node (cp-eft cp))))

(defun (setf cp-ef) (value node cp)
  (setf (gethash node (cp-eft cp)) value))

(defun cp-dur (node)
  (if (or (eq node 'beginning-of-process)
	  (eq node 'end-of-process))
    0
    (- (end-of node)
       (start-of node))))

(defun cp-nodes (cp)
  (fragments (cp-pert cp)))

(defun every-ef-known? (node cp)
  (every (lambda (pred)
	   (cp-ef pred cp))
	 (cp-priors node cp)))

(defun next-forward-node (cp)
  (find-if (lambda (node)
	     (and 
	      (every-ef-known? node cp)
	      (not (cp-es node cp))))
	   (cp-nodes cp)))

(defun do-forward-pass (cp)
  (let ((next (next-forward-node cp)))
    (if (not next)
      (values)
      (progn
	(process-forward-node next cp)
	;; (pvs! next)
	(do-forward-pass cp)))))

(defun process-forward-node (node cp)
  (if (eq node 'beginning-of-process)
    (values)
    (let* ((earliest-start
	    (apply 'max
		   (mapcar 
		    (lambda (prev)
		      (cp-ef prev cp))
		    (cp-priors node cp))))
	   (earliest-end
	    (+ earliest-start
	       (cp-dur node))))
      (setf (cp-es node cp) earliest-start)
      (setf (cp-ef node cp) earliest-end))))

(defun every-ls-known? (node cp)
  (every (lambda (pred)
	   (or (eq pred 'end-of-process)
	       (cp-ls pred cp)))
	 (cp-afters node cp)))

(defun next-backward-node (cp)
  (find-if (lambda (node)
	     (and (every-ls-known? node cp)
		  (not (cp-lf node cp))))
	   (cp-nodes cp)))

(defun process-backward-node (node cp)
  (if (eq node 'end-of-process)
    (values)
    (let* ((latest-finish 
	    (apply 'min
		   (mapcar 
		    (lambda (next)
		      (if (eq next 'end-of-process)
			(cp-ef node cp)
			(cp-ls next cp)))
		    (cp-afters node cp))))
	  (latest-start
	   (- latest-finish
	      (cp-dur node))))
      (setf (cp-lf node cp) latest-finish)
      (setf (cp-ls node cp) latest-start))))
		    

(defun do-backward-pass (cp)
  (let ((next (next-backward-node cp)))
    (if (not next)
      (values)
      (progn
	(process-backward-node next cp)
	(do-backward-pass cp)))))
	    
(defun transfer-info-to-fragments (cp)
  (dolist (fragment (cp-nodes cp))
    (setf (ls fragment) (cp-ls fragment cp))
    (setf (es fragment) (cp-es fragment cp))
    (setf (lf fragment) (cp-lf fragment cp))
    (setf (ef fragment) (cp-ef fragment cp))))

(defmethod calculate-critical-path ((chart pert))
  (let ((cp (create-cp-struct chart)))
    (do-forward-pass cp)
    (do-backward-pass cp)
    (transfer-info-to-fragments cp)))


;;;
;;; SHERPA INTERFACE. 
;;;

(defmethod pert-size ((thing t))
  0)

(defmethod pert-size ((task task))
  (length (all-task-fragments task)))

(defmethod pert-size ((agent agent))
  (let ((tptask (car (all-top-level-tasks agent))))
      (if (null tptask) 0
	  (pert-size tptask))))

(defmethod sherpa-pert ((thing t))
  (values))

(defmethod sherpa-pert ((task task))
  (let ((p (create-pert task)))
    (dolist (f  (sort-fragments-by-dependencies p))
      (write-line-sherpa (sherpa-pert-fragment-string p f)))))

(defmethod sherpa-pert ((agent agent))
  (let ((tptask (car (all-top-level-tasks agent))))
    (if (null tptask) (values)
	(sherpa-pert tptask))))

;;; Example output:
;;; 165 2 false false gaze 1460 0 (world new-cursor-location 4-key) (fast-move-click 4-key) (168 1 nil 171 0 nil )
;;; 
;;; a line has
;;; 1. unique id (num fragment)
;;; 2. color 
;;; 3. 'true' or 'false' : on critical path
;;; 4. 'true' or 'false' : clobbered? always set to false
;;; 5. resource name
;;; 6. start of fragment (real number)
;;; 7. duration of fragment (real number)
;;; 8. task description
;;; 9. template description (see TEMPLATE-TASK)
;;; 11. a list of triples describing dependencies:
;;;     - a dependant
;;;     - '0' or '1' if resource or logical
;;;     - 't' or 'nil' if edge is on critical path.

(defun sherpa-pert-rname (x)
  (if (typep x 'resource)
    (if (or (null (name x)) (string= (name x) "unspecified"))
      (id x)
      (name x))
    x))

(defmethod sherpa-pert-fragment-string ((chart pert) (fragment fragment))
  (with-output-to-string (str)
    (format str "~a " (num fragment))                                    ;; unique fragment id
    (format str "~a " (pert-color chart fragment))                       ;; color of fragment
    (format str "~a " (if (on-critical-path-p fragment) "true" "false")) ;; on critical path?
    (format str "false ")                                                ;; clobbered? -- set to false always for now
    (format str "~a " (sherpa-pert-rname (resource fragment)))           ;; 'resource row'
    (format str "~a " (start-of fragment)) ;; start of fragment
    (format str "~a " (fragment-duration fragment))   ;; duration of fragment
    (format str "~a " (resolved-description-string (task fragment)))
    (format str "~a " (resolved-description-string (template-task *application* fragment)))
    (format str "(" )
    (dolist (edge (pert-dependent-edges chart fragment))
      (format str "~a " (num (edge-before edge)))
      (ecase (edge-type edge)
	((:resource) (format str "1 "))
	((:logical ) (format str "0 ")))
      (format str "~a " (on-critical-path-p edge)))
    (format str ")" )
    ))

(defmethod depends-on-p ((chart pert) (f1 fragment) (f2 fragment))
  (and (member-if (lambda (edge)
	       (eq (edge-before edge) f2))
		  (pert-dependent-edges chart f1))
       t))

;;; fragments contain fragments that have been processed already. May be null
(defmethod already-saw-dependencies-p ((chart pert) (f fragment) fragments)
  (every (lambda (edge)
	   (member (edge-before edge) fragments))
	 (pert-dependent-edges chart f)))

;;; I think it's the case that Sherpa needs to get lines such that no
;;; dependency is referenced before it is created. This code sorts the pert
;;; chart's fragments in just that order. 
(defmethod sort-fragments-by-dependencies ((chart pert))
  (let ((seen (list))
	(unseen (copy-list (fragments chart))))
    (labels ((process ()
	       (when (null unseen)
		 (return-from sort-fragments-by-dependencies (nreverse seen)))
	       (let ((next (find-if 
			    (lambda (f)
			      (already-saw-dependencies-p 
			       chart
			       f
			       seen))
			    unseen)))
		 (unless next (error "unable to sort by dependencies"))
		 (setq seen (cons next seen))
		 (setq unseen (delete next unseen))
		 (process))))
      (process))))


;;; -----------------------------------------------
;;; Output to DOT format : for (will's) debugging :)
;;; ------------------------------------------------

(defmethod dot-id ((fragment fragment))
  (format nil "~a_~a_~a"
	  (dot-id (resource fragment))
	  (dot-id (task fragment))
	  (fragment-number fragment)))

(defun string-replace-all (old new big)
  "Replace all occurences of OLD string with NEW
string in BIG string."
  (do ((newlen (length new))
       (oldlen (length old))
       (i (search old big)
          (search old big :start2 (+ i newlen))))
      ((null i) big)
      (setq big
            (concatenate 'string
              (subseq big 0 i)
              new
              (subseq big (+ i oldlen))))))


;;;  task1 [shape=Mrecord,label="Reach for news  | { {Task 1 | 00:011} | {20 ms | 00:031}}"];

(defun dot-safe-description-string (task)
   (string-replace-all
   "{" "\\{"
   (string-replace-all 
    "}" "\\}" 
    (resolved-description-string task))))

(defun resolved-description-string (task)
  (princ-to-string (replace-vars (description task)
				 (get-local-context task)
				 (task-globals task))))

(defun princ-mrecord (fragment stream)
  (let ((end (or (end-of fragment) 
		 (current-time))))
    (format stream "  ~a [shape=~a,label=\"~a  | { { ~a/~a | ~a} | {~a | ~a}}\"];~%"
	    ;;(dot-id resource)
	    (dot-id fragment)
	    (if (on-critical-path-p fragment) "Mrecord" "record")
	    (dot-safe-description-string   (task fragment))
	    (dot-id (resource fragment))
	    (id (task fragment))
	    (with-output-to-string (str) (duration-princ (- end (start-of fragment)) str))
	    (start-of fragment)
	    end)))

;;;(defun princ-mrecord (fragment stream)
;;;
;;;    (format stream "  ~a "
;;;	    (dot-id fragment)))



(defmethod pert-to-dot-file ((chart pert) &optional (filename (system:make-temp-file-name)))
    (with-open-file (str filename
		     :direction :output 
		     :if-exists :supersede)
      (pert-to-dot-str chart str))
    filename)

(defmethod pert-to-dot-str ((chart pert) (stream stream))
  (with-slots 
      (resources resource-dependency-lists logical-dependencies) chart
    (format stream "digraph pert {~%")
    (format stream " rankdir=LR~%")
    ;; (format stream " nodesep=0.5~%")
    (format stream " node [shape=box];~%")
    ;; resources
    (format stream "/* Resources */~%")
    (format stream "{rank=source ~%")
    (loop for r in resources doing
	  (format stream " ~a;~%" (dot-id r)))
    (loop for r1 in resources for r2 in (cdr resources) doing
      (format stream " ~a -> ~a [style=invis weight=1000];~%" (dot-id r1) (dot-id r2)))
    (format stream " }~%")
    
    ;; define all task fragments
    
    (dolist (rl resource-dependency-lists)
      (dolist (edge rl)
	(princ-mrecord (edge-after edge) stream)))
    
    ;; print out the resource to first fragment lines
    
    (dolist (rl resource-dependency-lists)
      (let ((resource (edge-before (car rl)))
	    (firstfragment (edge-after (car rl))))
	(format stream "~a -> ~a  [weight=1000];~%"
		(dot-id resource)
		(dot-id firstfragment)
		)))

    ;; print out the remaining lists
    (dolist (rl resource-dependency-lists)
	(dolist (edge (cdr rl))
	  (format stream "~a -> ~a [style=dotted weight=1000];~%"
		  (dot-id (edge-before edge))
		  (dot-id (edge-after edge)))))
    
    
    ;; print out logical dependencies
    
    (dolist (edge logical-dependencies)
      (format stream "~a -> ~a [weight=0];~%"
	      (dot-id (edge-before edge))
	      (dot-id (edge-after edge))))

    
    ;; finish
    (format stream "~%}~%")
    ))