;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors-to-dot.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors-to-dot.lisp,v 1.12 2006/03/16 15:57:48 will Exp $


;;;
;;; create dot graphics ...
;;;

;;;Colors: darkseagreen: satisfied
;;;        salmon: unsatisfied
;;;        yellow: partially satisfied
;;;
;;;Shapes: filled box: complex episode -- 
;;;        filled ellipse: measurement or estimate
;;;        filled many-sided polygon: simple episode
;;;        empty box: variable bindings
;;;        plaintext: constraints
;;;
;;;Outlines: present: known (i.e., has been checked, and known to be 
;;;                   satisfied or unsatisfied)
;;;          absent: unknown (i.e., hasn't been checked)
;;;
;;;Text: 'unsatisfied' under unsatisfied or unknown monitors (otherwise
;;;either satisfied or partially satisfied; user can tell from context
;;;which). This provides redundancy for color-blindness.
;;;
;;;		
;;;Edge endings: arrows: indicate data flow (from sv histories to simple
;;;                      monitors).  
;;;             'O-dot' arrows indicate composition of
;;;                     complex monitors. Dotted lines indicate constraints or
;;;                      bindings connections.
						     
(in-package :user)



(defparameter *unknown-monitor-color* 'lightgrey)
(defparameter *unsatisfied-monitor-color* "\"#FF4240\"")
(defparameter *satisfied-monitor-color* 'darkseagreen)
(defparameter *partially-satisfied-monitor-color* "\"#FFCC66\"") ;; 'yellow)
(defparameter *history-color-background* "\"#9FC7E6\"" ) ;; 'slateblue)
(defparameter *history-color-foregound* "\"#B5D5FF\"") ;; "\"#B5D5FF\"") ;;  'lightslateblue)


(defmethod dot-id ((obj id-mixin))
  (substitute-if-not  #\_ #'alphanumericp (princ-to-string (id obj))))

(defmethod state-variable-histories-for ((monitor monitor) bindings)
  (if (null (task monitor))
    '()
    (state-variable-histories-for/1 monitor bindings)))

(defmethod state-variable-histories-for/1 ((monitor measurement-monitor) bindings)
  (if (eq (state (monitor-status  monitor))
	  :unknown)
    '()
    (let ((expr (substitute-bindings (expr monitor) bindings)))
      (let ((attr (measurement-form-attribute expr))
	    (obj/v  (measurement-form-object expr))
	    (mem (agent-sv-memory (agent (task monitor)))))
	(if (variable-p obj/v)
	  (mapcar
	   (lambda (object)
	     (sv-history mem
			 (make-state-variable attr object)))
	   (sv-memory-objects mem attr))
	  (list (sv-history mem
			    (make-state-variable attr obj/v))))))))


(defmethod state-variable-histories-for/1 ((monitor episode-monitor) bindings)
  (let ((expr (substitute-bindings (expr monitor) bindings)))
    (let ((attr (sv-form-attribute expr))
	  (obj/v  (sv-form-object expr))
	  (mem (agent-sv-memory (agent (task monitor)))))
      (if (variable-p obj/v)
	(mapcar
	 (lambda (object)
	   (sv-history mem
		       (make-state-variable attr object)))
	 (sv-memory-objects mem attr))
	(list (sv-history mem
			  (make-state-variable attr obj/v)))))))

(defmethod state-variable-histories-for/1 ((monitor atomic-episode-monitor) bindings)
  (declare (ignore bindings))
  '())


(defmethod state-variable-histories-for/1 ((monitor complex-episode-monitor) bindings)
  (remove-duplicates 
   (loop for submonitor in (submonitors monitor)
       appending
	 (state-variable-histories-for submonitor bindings))))


(defmethod sv-history-to-dot ((hist sv-history) (interval interval))
  (let* ((sv (sv-history-sv hist))
	 (g (make-dot-graph 
	     (gen-dot-id)
	     :source hist
	     :attributes
	     `((label ,(format nil "\"(~a ~a)\""
			       (sv-attribute sv) (monitor-expr-object-atom  (sv-object sv))))
	       (root . "\"\"")
	       (labelloc . b)
	       (style . filled)
	       (color . ,*history-color-background*))
	     :node-defaults
	      `((shape . box)
		(peripheries . 1))
	     :nodes 
	     (sv-history-map 
	      hist interval
	      (lambda (m)
		(make-dot-node 
		 (dot-id m)
		 :source hist
		 :attributes 
		 `((color . ,*history-color-foregound*)
		   (label ,(format nil "\"~a: ~a\""
				  ;;(sv-attribute sv)
				  ;;(monitor-expr-object-atom (sv-object sv))
				   (time-format (measurement-timestamp m))
				   (monitor-expr-object-atom (measurement-value m))
				  )))))))))
    (loop 
	for n1 in (graph-nodes g)
	for n2 in (cdr (graph-nodes g))
	do
	  (add-dot-edge g (make-dot-edge n2 n1
					 :attributes
					  `((style . invis)))))
    g))

(defun pprint-to-string (obj width)
  (let ((*print-right-margin* width))
    (with-output-to-string (str)
      (pprint obj str))))

(defun princ-to-qstring (obj &optional (width 45))
  (let ((first t))
    (with-output-to-string (str)
      (princ #\" str)
      (loop for ch across (if (stringp obj) obj (pprint-to-string obj width))
	  do
	    (cond ((and (char= ch #\Newline) (not first))
		   (princ #\\ str)
		   (princ #\n str))
		  ((and (char= ch #\Newline) first))
		  ((or (char= ch #\')
		       (char= ch #\"))
		   (princ #\\ str)
		   (princ ch str))
		  (t (setf first nil)
		     (princ ch str))))
      (princ #\" str))))

(defun make-constraint-node (constraint-list m)
  (make-dot-node (gen-dot-id)
		 :source m
		 :attributes
		 `((label . ,(princ-to-qstring constraint-list))
		   (shape . box)
		   (color . white)
		   (peripheries . 0))))


;;; commented out -- we're putting labels inside nodes for now, which usually saves
;;; space. We may want to add them back in.
(defmethod add-constraint-node-maybe ((m monitor)
				      (g dot-graph)
				      (node dot-node))
;;;  (let ((constraint-list (monitor-parameters m)))
;;;    (when constraint-list
;;;      (let ((cnode (make-constraint-node constraint-list m)))
;;;	(add-dot-node g cnode)
;;;	(add-dot-edge g (make-dot-edge node cnode
;;;				 :attributes 
;;;				 `((arrowhead . none)
;;;				   (style . dotted) 
;;;				   (constraint . false)))))))
  )


(defmethod bindings-to-node (bindings &optional (label "Bindings"))
  (flet ((blabel ()
	   (with-output-to-string (str)
	     (princ #\" str)
	     (format str "~a\\n" label)
	     (bindings-for-each 
	      (lambda (binding)
		(let ((val (binding-val binding)))
		  (when (print-binding-p binding)
		    (format str "~A=~A\\n" 
			    (binding-var binding)
			    (if (typep val 'appob)
			      (id val)
			      val)))))
	      bindings)
	     (princ #\" str))))
    (make-dot-node 
     (gen-dot-id)
     :source bindings
     :attributes
     `((style . box)
       (shape . plaintext)
       (peripheries . 1)
       (color . darkseagreen)
       (label . ,(blabel))))))

(defmethod leaf-roots ((graph dot-graph))
  (let ((leaves (list)))
    (dfs-graph graph
	       (lambda (graph)
		 (when (and (null (graph-subgraphs graph))
			    (first (graph-nodes graph)))
		   (setf leaves (cons (first (graph-nodes graph)) leaves)))))
    leaves))

(defmethod add-binding-edge-maybe ((g dot-graph) (binding-node dot-node) (leaf-node dot-node))
  (when (node-source leaf-node)
    (let ((variables (variables-in (pattern (node-source leaf-node)))))
      (when (some (lambda (variable)
		    (value-in variable (node-source binding-node)))
		  variables)
	(add-dot-edge g 
		      (make-dot-edge leaf-node  binding-node
				     :attributes
				     `((arrowhead . normal)
				       (style . dotted)
				       (constraint . false))))))))

(defmethod add-history-edge-maybe ((g dot-graph) (hg dot-graph) (leaf-node dot-node) bindings)
  (when (and (graph-source hg)
	     (node-source leaf-node)
	     (first (graph-nodes hg)))
    (let ((monitor (node-source leaf-node))
	  (hist (graph-source hg)))
      (when (member hist (state-variable-histories-for monitor bindings))
	(add-dot-edge g
		      (make-dot-edge 
		       leaf-node
		       (first (last (graph-nodes hg)))
		       :attributes
		       `((arrowhead . none)
			 (arrowtail . normal))))))))
			   
(defmethod add-leaf-nodes-graph-maybe ((g dot-graph) history-graphs)
  (let ((hgs (filter (lambda (h) (first (graph-nodes h)))
		     history-graphs)))
  (when (cdr hgs) ;; i.e., more than one ...
    (let ((subg (make-dot-graph (gen-dot-id)
			     :attributes 
			     `((label . "\"\"")
			       (root  .  "\"\"")
			       (rank  . same)))))
      (dolist (hg hgs)
	(add-dot-node subg (first (graph-nodes hg))))
      (add-dot-subgraph g subg)))))



(defmethod monitor-to-dot/1 ((m complex-episode-monitor))
  (let* ((node-id (gen-dot-id))
	 (g (make-dot-graph node-id ;; (dot-id m)
			    :source m
			   :attributes
			   `((ordering . out)
			     (root . ,node-id)
			     (peripheries . 0)
			     (label . "\"\\n\""))
			   :node-defaults
			   `((label . "\"\\n\"")
			     (shape . ellispe)
			     (style . filled)
			     (peripheries . 2))
			   :edge-defaults
			   `((color . gray) (arrowhead . odot))))
	 (root (make-dot-node (dot-id m) ;; node-id
			      :source m
			      :attributes
			      `((label . ,(monitor-label m (car (expr m))))
				(shape . box)
				(style . ,(shape-style m))
				(peripheries . ,(periphery-count m))
				(color . ,(shape-color m)))))
	 (children (mapcar (lambda (child)
			     (monitor-to-dot/1 child))
			   (submonitors m))))
    (setf (graph-nodes g)
      (cons root (filter #'identity (mapcar (lambda (child)
					      (first (graph-nodes child)))
					    children))))
    (dolist (fnode (cdr (graph-nodes g)))
      (add-dot-edge g (make-dot-edge root fnode)))
    (dolist (child children)
      (add-dot-subgraph g child))
    (add-constraint-node-maybe m g root)
    g))

(defmethod monitor-to-dot/1 ((m simple-episode-monitor))
  (let ((g  (make-dot-graph (gen-dot-id) ;; (dot-id m) 
			    :attributes 
			    `((label . "\"\\n\"")
			      (peripheries . 0)) 
			    :source m))
	(node (make-dot-node (dot-id m) ;; (gen-dot-id)
			     :source m
			     :attributes
			     `((label . ,(monitor-label m (expr m)))
			       (shape . polygon)
			       (sides . 14)
			       (style . ,(shape-style m))
			       (peripheries . ,(periphery-count m))
			       (color . ,(shape-color m))))))
    (add-dot-node g node)
    (add-constraint-node-maybe m g node)
    g))

(defmethod monitor-to-dot/1 ((m atomic-episode-monitor))
  (make-dot-graph (gen-dot-id) ;; (dot-id m)
		  :source m
		  :attributes
		  `((label . "\"\\n\"")
		    (peripheries . 0))
		  :nodes
		  (list 
		   (make-dot-node (dot-id m) ;; (gen-dot-id)
				  :source m
				  :attributes
				  `((label . ,(monitor-label m (expr m)))
				    (shape . ellipse)
				    (style . ,(shape-style m))
				    (peripheries . ,(periphery-count m))
				    (color . ,(shape-color m)))))))

(defmethod monitor-to-dot/1 ((m measurement-monitor))
  (let ((g (make-dot-graph (gen-dot-id) ;;(dot-id m) 
			   :attributes
			   `((label . "\"\\n\"")
			     (peripheries . 0))  
			   :source m))
	(enode (make-dot-node (dot-id m) ;; (gen-dot-id)
			      :source m
			      :attributes
			      `((label . ,(monitor-label m (expr m)))
				(shape . ellipse)
				(style . ,(shape-style m))
				(peripheries . ,(periphery-count m))
				(color . ,(shape-color m))))))
    (add-dot-node g enode)
    (add-constraint-node-maybe m g enode)
    g))

(defmethod shape-color ((monitor monitor))
  (ecase (state (monitor-status monitor))
    (:unknown *unknown-monitor-color*)
    (:unsatisfied *unsatisfied-monitor-color*)
    (:satisfied  *satisfied-monitor-color*)
    (:partially-satisfied *partially-satisfied-monitor-color*)))


(defmethod periphery-count ((monitor monitor))
  "Number of lines to draw around boxes"
  (ecase (state (monitor-status monitor))
    (:unknown 1)
    (:unsatisfied 1)
    (:satisfied 1)
    (:partially-satisfied 1)))

(defmethod shape-style ((monitor monitor))
  (ecase (state (monitor-status monitor))
    (:unknown 'filled)
    (:unsatisfied 'filled)
    (:satisfied 'filled)
    (:partially-satisfied 'filled)))

(defmethod monitor-expr-object-atom ((object t))
  object)

(defmethod monitor-expr-object-atom ((object task))
  ;; (intern (elided-string (princ-to-string object) 20)))
  (intern (string-upcase (symbol-name (id object)))))

(defmethod elided-string ((string string) (max-length integer))
  (let ((strlen (length string)))
    (if (<= strlen max-length)
      string
      (concatenate 'string (subseq string 0 (max (- max-length 4) 1))
		   "..."
		   (make-string 1 :initial-element (aref string (1- strlen)))))))
	

(defun cons-map (cons function)
  (cond 
    ((null cons) nil)
    ((atom cons) (funcall function cons))
    (t (cons (if (null (car cons)) ;; NIL as item ...
	       (funcall function (car cons))
	       (cons-map (car cons) function))
	     (cons-map (cdr cons) function)))))

(defun dot-expr (expr)
  (cons-map expr #'monitor-expr-object-atom ))

(defmethod monitor-label ((monitor monitor) expr  &optional width)
  (princ-to-qstring 
   (with-output-to-string (outstr)
     (princ (dot-expr expr) outstr)
     (case (state (monitor-status monitor))
       (:satisfied (princ "\\n" outstr)
		   (princ (interval-string (interval (monitor-status monitor))) outstr)))
     (when (monitor-parameters monitor)
       (princ "\\n" outstr)
       (princ (monitor-parameters monitor) outstr)
       (princ "\\n \\n" outstr))
     width)))


					 
(defmethod monitor-to-dot ((monitor monitor) &key (direction :top-to-bottom)
						  (print-histories-p t) 
						  (interval +always-dur+))
  "Convert a monitor into a (Lisp) dot graph. Use PRINT-GRAPH to print it out."
  (let ((bindings (binding-set (monitor-status monitor)))
	(task-bindings (if (task monitor) (binding-set-union (vars (task monitor))
							     (get-local-context (task monitor))) nil)))
    (let ((g (monitor-to-dot/1 monitor))
	  (histories (and print-histories-p
			  (state-variable-histories-for monitor (or bindings no-bindings))))
	  (dir-string (ecase direction (:left-to-right "LR") (:top-to-bottom "TB"))))
      (add-dot-attributes g
			  `((rankdir  . ,(princ-to-qstring dir-string))
			    (label    . ,(princ-to-qstring (expr monitor) 100))
			  ))
      
      (let ((leaves (leaf-roots g)))
	(when (and bindings (print-bindings-p bindings))
	  (let ((bnode (bindings-to-node bindings "Output")))
	    (add-dot-node g bnode)
	    (dolist (leaf leaves)
	      (add-binding-edge-maybe g bnode leaf))))
	
	(when (and print-histories-p histories)
	  (let ((history-graphs 
		 (mapcar (lambda (history)
			   (sv-history-to-dot history interval))
			 histories)))
	    (dolist (hg history-graphs)
	      (add-dot-subgraph g hg))
	    (add-leaf-nodes-graph-maybe g history-graphs)

	    (dolist (leaf leaves)
	      (dolist (hg history-graphs)
		(add-history-edge-maybe g hg leaf (or bindings no-bindings)))))))

      (when (and task-bindings (print-bindings-p task-bindings))
	(let ((tnode (bindings-to-node task-bindings "Input")))
	  (add-dot-node g tnode)
	  (add-dot-edge g
			(make-dot-edge tnode (first (graph-nodes g))
				       :attributes
				       `((arrowhead . normal)
					 (style . dotted) 
					 (constraint . false))))))
      g)))


;;; (defparameter +m-example+ '(:in-order (:and (color ?x = red) (:measurement (flashing ?x = true) :estimation (:persist :with-timeout P10s)))  (:episode (status alarm) :msi P1m :value (:all (eql on)) :timing (:duration (>= P30S))) (broken ?z)))


(defmethod monitor-to-dot-file ((monitor monitor) &key (filename (system:make-temp-file-name))
						   (direction :top-to-bottom)
						   (print-histories-p t) 
						       (interval (make-interval 0 +end-of-time+)))
  "Convert a monitor into a DOT file, and save it to an output file."
  (let ((g (monitor-to-dot monitor 
			   :direction direction
			   :print-histories-p print-histories-p
			   :interval interval)))
  (with-open-file (str filename
		   :direction :output 
		   :if-exists :supersede)
    (print-graph g str))
  filename))



(defmethod monitor-to-svg-file ((monitor monitor) &key (filename (system:make-temp-file-name))
						       (direction :top-to-bottom)
						       (print-histories-p t)
						       (compressed-p nil)
						       (interval (make-interval 0 +end-of-time+)))
  "Convert a monitor to an SVG file. "
  (let ((dot-file (monitor-to-dot-file monitor
		   :direction direction
		   :print-histories-p print-histories-p
		   :interval interval)))
    (convert-dot-file-to-svg-file dot-file filename compressed-p)))



(defmethod monitor-to-svg-string ((monitor monitor) &key (direction :top-to-bottom)
							 (print-histories-p t)
							 (interval (make-interval 0 +end-of-time+)))
  "Convert a monitor to an SVG string."
  (flet ((svg-file-to-string (svg-file)
	   (if (not (probe-file svg-file))
	     :svg-file-not-found
	     (with-output-to-string (str)
	       (with-open-file (in svg-file :direction :input)
		 (loop for line = (read-line in nil)
		     while line
		     do  (princ line str)
			 (terpri str)))))))
    (let ((dot-file (monitor-to-dot-file monitor
					 :direction direction
					 :print-histories-p print-histories-p
					 :interval interval)))
      (let ((svg-file (convert-dot-file-to-svg-file dot-file)))
	(if (keywordp svg-file) 
	  (values "" svg-file)
	  (svg-file-to-string svg-file))))))


