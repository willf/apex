;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/dot.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: dot.lisp,v 1.6 2006/01/15 03:43:02 dalal Exp $


(in-package :cl-user)

;;;
;;; tools to create dot graphics
;;;
;;;


(let ((tab-table (make-hash-table))
      (cnt 0))
  (defun ntabs (n)
    (or (values (gethash n tab-table))
	(let ((str (coerce (make-array n :initial-element #\Tab) 'string)))
	  (when (< cnt 100) ;; hate to allow anything to grow without limit...
	    (incf cnt)
	    (setf (gethash n tab-table) str))
	  str))))

(defclass dot-attribute ()
  ((key :initarg :key :accessor attribute-key)
   (value :initarg :value :accessor attribute-value)))

(defun make-dot-attribute (key value)
  (make-instance 'dot-attribute
    :key key
    :value value))

(defun list->dot-attributes (list)
  (mapcar (lambda (el)
	    (cond
	     ((consp el)
	      (make-instance 'dot-attribute
		:key (car el)
		:value (if (consp (cdr el)) (second el) (cdr el))))
	     ((typep el 'dot-attribute) el)
	     (t (error "don't know how to convert ~a into dot-attribute") el)))
	  list))

(defmethod print-object ((dot-attribute dot-attribute) (stream stream))
  (format stream "~a=~a"
	  (attribute-key dot-attribute)
	  (attribute-value dot-attribute)))

(defun princ-dot-attributes (attributes stream)
  (format stream "[~{~a~^, ~}]" attributes))

(defun princ-named-attributes (name attributes stream tabs)
  (format stream "~a~a " (ntabs tabs) name)
  (princ-dot-attributes attributes stream)
  (format stream ";~%"))

(defclass dot-node ()
  ((name :initarg :name :accessor node-name)
   (node-attributes :initarg :node-attributes :initform '() :accessor node-attributes)
   (source :initarg :source :initform NIL :accessor node-source
	   :documentation "Not part of DOT standard")))

(defun make-dot-node (name &key source attributes)
  (make-instance 'dot-node :name name :source source :node-attributes (list->dot-attributes attributes)))

(defmethod print-node ((node dot-node) (stream stream) tabs)
  (princ (ntabs tabs) stream)
  (princ (node-name node) stream)
  (when (node-attributes node)
    (princ " " stream)
    (princ-dot-attributes (node-attributes node) stream))
  (format stream ";~%"))

(defclass dot-graph () 
  ((name :initarg :name :initform "digraph" :accessor graph-name)
   (graph-attributes :initarg :graph-attributes :initform '() :accessor graph-attributes)
   (node-defaults :initarg :node-defaults  :initform '() :accessor node-defaults)
   (edge-defaults :initarg :edge-defaults  :initform '() :accessor edge-defaults)
   (subgraphs :initarg :subgraphs      :initform '() :accessor graph-subgraphs)
   (nodes :initarg :nodes          :initform '() :accessor graph-nodes)
   (edges :initarg :edges          :initform '() :accessor graph-edges)
   (source :initarg :source :initform NIL :accessor graph-source
	   :documentation "Not part of DOT standard")))

(defun make-dot-graph (name &key source attributes node-defaults edge-defaults subgraphs nodes edges)
  (make-instance 'dot-graph
    :name name
    :source source
    :graph-attributes (list->dot-attributes attributes)
    :node-defaults (list->dot-attributes node-defaults)
    :edge-defaults (list->dot-attributes edge-defaults)
    :subgraphs subgraphs
    :nodes nodes
    :edges edges))

(defmethod print-graph ((graph dot-graph) &optional (stream *standard-output*))
  (when (or (null stream) (eq stream t))
    (setq stream *standard-output*))
  (let ((tabs 0))
    (format stream "digraph ~a {~%" (graph-name graph))
    (when (graph-attributes graph)
      (princ-named-attributes 'graph (graph-attributes graph) stream (1+ tabs) ))
    (when (node-defaults graph)
      (princ-named-attributes 'node (node-defaults graph) stream (1+ tabs)))
    (when (edge-defaults graph)
      (princ-named-attributes 'edge (edge-defaults graph) stream (1+ tabs)))
    (dolist (subgraph (graph-subgraphs graph))
      (print-subgraph subgraph stream (1+ tabs)))
    (dolist (node (graph-nodes graph))
      (print-node node stream (1+ tabs)))
    (dolist (edge (graph-edges graph))
      (print-edge edge stream (1+ tabs)))
    (format stream "~a}~%" (ntabs tabs)))
  (values))

(defmethod print-subgraph ((graph dot-graph) (stream stream) tabs)
  (princ (ntabs tabs) stream)
  (format stream "subgraph ~a {~%" (dot-id (gentemp "cluster-")))
  (when (graph-attributes graph)
    (princ-named-attributes 'graph (graph-attributes graph) stream (1+ tabs)))
  (when (node-defaults graph)
    (princ-named-attributes 'node (node-defaults graph) stream (1+ tabs)))
  (when (edge-defaults graph)
    (princ-named-attributes 'edge (edge-defaults graph) stream (1+ tabs)))
  (dolist (subgraph (graph-subgraphs graph))
      (print-subgraph subgraph stream (1+ tabs)))
  (dolist (node (graph-nodes graph))
    (print-node node stream (1+ tabs)))
  (dolist (edge (graph-edges graph))
    (print-edge edge stream (1+ tabs)))
  (format stream "~a}~%" (ntabs tabs)))

(defclass dot-edge ()
  ((from :initarg :from :accessor edge-from)
   (to   :initarg :to   :accessor edge-to)
   (from-field :initarg :from-field :initform nil :accessor edge-from-field)
   (to-field :initarg :to-field :initform nil :accessor edge-to-field)   
   (edge-attributes :initarg :attributes :initform '() :accessor edge-attributes)))

(defun make-dot-edge (from to &key from-field to-field attributes)
  (make-instance 'dot-edge
    :from from
    :to to
    :from-field from-field
    :to-field to-field
    :attributes (list->dot-attributes attributes)))

(defmethod print-edge ((edge dot-edge) (stream stream) tabs)
  (princ (ntabs tabs) stream)
  (format stream "~a" (node-name (edge-from edge)))
  (when (edge-from-field edge)
    (format stream ":~a" (edge-from-field edge)))
  (princ " -> " stream)
  (format stream "~a" (node-name (edge-to edge)))
  (when (edge-to-field edge)
    (format stream ":~a" (edge-to-field edge)))
  (when (edge-attributes edge)
    (princ " " stream)
    (princ-dot-attributes (edge-attributes edge) stream))
  (format stream ";~%" ))
	  
(defmethod dot-id ((obj t))
  (substitute-if-not  #\_ #'alphanumericp (princ-to-string obj)))

(defun gen-dot-id ()
  (dot-id (gensym)))


(defmethod add-dot-node ((g dot-graph) (n dot-node))
  (setf (graph-nodes g)
    (nconc (graph-nodes g) (list n))))

(defmethod add-dot-subgraph ((g dot-graph) (s dot-graph))
  (setf (graph-subgraphs g)
    (nconc (graph-subgraphs g) (list s))))

(defmethod add-dot-edge ((g dot-graph) (e dot-edge))
  (setf (graph-edges g)
    (nconc (graph-edges g) (list e))))

(defmethod add-dot-attributes ((g dot-graph) attributes)
  (setf (graph-attributes g)
    (append (graph-attributes g)
	    (list->dot-attributes attributes))))

(defmethod add-dot-attributes ((n dot-node) attributes)
  (setf (node-attributes n)
    (append (node-attributes n)
	    (list->dot-attributes attributes))))

(defmethod add-dot-attributes ((e dot-edge) attributes)
  (setf (edge-attributes e)
    (append (edge-attributes e)
	    (list->dot-attributes attributes))))

;;;
;;; ---- This, for fun, is code to generate box diagrams of 
;;;      Lisp cons structures. Circular lists are probably ok
;;;      but DOT can't display 'em. Use another mode.
;;; 

;;;(defun nil-node ()
;;;  (make-dot-node (gen-dot-id)
;;;		 :attributes 
;;;		 `((label . "NIL") 
;;;		   (style . plaintext) 
;;;		   (peripheries . 0))))
;;;
;;;(defun atom-node (value)
;;;  (make-dot-node (gen-dot-id)
;;;		 :attributes 
;;;		 `((label . ,(princ-to-string value))
;;;		   (style . plaintext) 
;;;		   (peripheries . 0))))
;;;
;;;
;;;(defun cons-node (kons g)
;;;  (or (find-kons-node kons)
;;;      (let ((left-field (gen-dot-id))
;;;	    (right-field (gen-dot-id)))
;;;	(let ((node (make-dot-node 
;;;		     (gen-dot-id)
;;;		     :attributes 
;;;		     (list->dot-attributes
;;;		      `((shape . "Mrecord")
;;;			(label,(format nil "\"<~a> car |<~a> cdr\"" left-field right-field)))))))
;;;	  (add-kons kons node)
;;;	  (let ((kar (car kons))
;;;		(kdr (cdr kons)))
;;;	    (let ((kar-node
;;;		   (cond 
;;;		    ((find-kons-node kar))
;;;		    ((null kar) (nil-node))
;;;		    ((atom kar) (atom-node kar))
;;;		    (t (cons-node kar g))))
;;;		  (kdr-node
;;;		   (cond
;;;		    ((find-kons-node kdr))
;;;		    ((null kdr) (nil-node))
;;;		    ((atom kdr) (atom-node kdr))
;;;		    (t (cons-node kdr g)))))
;;;	      (add-dot-node g node)
;;;	      (add-dot-node g kar-node)
;;;	      (add-dot-node g kdr-node)
;;;	      (add-dot-edge g (make-dot-edge node kar-node :from-field left-field))
;;;	      (add-dot-edge g (make-dot-edge node kdr-node :from-field right-field))))
;;;	  node))))
;;;
;;;(let ((konses '()))
;;;  (defun clear-konses () (setq konses '()))
;;;  (defun add-kons (kons node) (setq konses (cons (cons kons node) konses)))
;;;  (defun find-kons-node (kons)  (cdr (assoc kons konses))))
;;;
;;;(defun cons-graph (list)
;;;  (clear-konses)
;;;  (let ((g (make-dot-graph  (gen-dot-id) 
;;;			    :attributes `((ordering . out)))))
;;;    (if (null list)
;;;      (let ((node (nil-node)))
;;;	(add-dot-node g node))
;;;      (cons-node list g))
;;;    g))


(defmethod dfs-graph ((g dot-graph) fn)
  (let ((visited (make-hash-table)))
    (labels ((dfs-graph/1 (graph)
	       (unless (gethash graph visited)
		 (setf (gethash graph visited) t)
		 (funcall fn graph)
		 (dolist (subgraph (graph-subgraphs graph))
		   (dfs-graph/1 subgraph)))))
      (dfs-graph/1 g))))

(defparameter *dot-application-name*
    (first 
     (list 
      #+(:and :allegro :macosx) "/Applications/Graphviz.app/Contents/MacOS/dot"
      #+(:and :allegro :mswindows) "\\program files\\graphviz\\dot.exe"
      "dot"))
  "Name of the DOT application.")


(defun convert-dot-file (infname &optional (outfilen (system:make-temp-file-name))
					   outformat)
  "General DOT converter; format name must be ready for DOT command line."
  (cond
   ((not infname)
    :infile-not-given)
   ((not (probe-file infname))
    :infile-not-found)
   ((not (dot-application-available-p))
    :dot-application-not-found)
   (t 
   (let ((cmd (format nil "~a -o~a -T~a  ~a" *dot-application-name* outfilen outformat infname)))
     (let ((errno (run-shell-command cmd :wait t)))
       (if (zerop errno)
	 (values outfilen errno cmd)
	 (values :dot-application-failed errno cmd)))))))

(defun convert-dot-file-to-svg-file (infname &optional (outfilen (system:make-temp-file-name))
						       compressed-p)
  "DOT to SVG."
  (convert-dot-file infname outfilen 
		    (if compressed-p "svgz" "svg")))


(defun dot-application-available-p ()
  "Is the DOT application available?"
  (and *dot-application-name* 
       (probe-file *dot-application-name*)))

(defun set-dot-application-name (path)
  "Set the path of the DOT application. Must be in operating system format (not Lisp path format)"
  (setq *dot-application-name* path))
