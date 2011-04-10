;;;-*- Mode: Lisp; Package: TREE -*-
;;;
;;; apex/system/utility/red-black-trees.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: red-black-trees.lisp,v 1.3 2006/01/15 03:43:03 dalal Exp $
;; Created:   August, 2001
;; Author:   Will Fitzgerald, Matt Tennenbaum
;; 
;; Description: red black trees, a la corman
;; 
;; Changes:   
;;
;; rjf 09/26/2002 - Moved into inet.util.tree package.  Moved package
;;                   definition into file.
;; 
;;----------------------------------------------------------------------

#|
Copyright (c) 2002, I/NET, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met: 

- Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

- Neither the name of the I/NET, Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(cl:defpackage :inet.util.tree
  (:use :common-lisp
	:inet.util.process)
  (:export #:tree-node 
	   #:node-data 
	   #:node-left 
	   #:node-right 
	   #:node-parent 
	   #:node-color
	   #:black 
	   #:red
	   #:node-children
	   #:show-node
	   #:the-null-node
	   #:make-tree-node
	   #:tree-node-p
	   #:null-node-p
	   #:red-black-tree
	   #:synched-red-black-tree
	   #:tree-semaphore
	   #:tree-order-predicate 
	   #:order-predicate
	   #:tree-equal-predicate 
;;;	   #:equal-predicate
	   #:tree-key
	   #:tree-root
	   #:tree-search
	   #:tree-search<
	   #:tree-search<=
	   #:tree-search>
	   #:tree-search>=
	   #:tree-find
	   #:tree-find<
	   #:tree-find<=
	   #:tree-find>
	   #:tree-find>=
	   #:tree-find-all
	   #:tree-find-all<
	   #:tree-find-all<=
	   #:tree-find-all>
	   #:tree-find-all>=
	   #:tree-minimum
	   #:tree-maximum
	   #:tree-successor
	   #:tree-predecessor
	   #:in-order-walk
	   #:tree-for-each
	   #:tree-map
	   #:tree->list
	   #:tree-insert
	   #:tree-delete
	   #:tree->tree-list
	   #:tree-leaves
	   #:tree-interiors
	   #:list->tree
;;;	   #:predicate
	   #:tree-pop
	   #:tree-first
	   #:tree-empty-p
	   #:create-red-black-tree
	   ))

(in-package :inet.util.tree)

(defmacro tree-do-while (cond &body body)
  (let ((var (gensym)))
    `(do ((,var ,cond ,cond))
	 ((not ,var))
       ,@body)))

(defclass tree-node ()
  ((data :initarg :data :initform NIL :accessor node-data)
   (left :initarg :left :initform NIL :accessor node-left)
   (right :initarg :right :initform NIL :accessor node-right)
   (parent :initarg :parent :initform NIL :accessor node-parent)
   (color :initarg :color :initform :black :accessor node-color)))

(defun make-null-node ()
  (let ((node (make-instance 'tree-node :color :black)))
    (setf (node-left node) node)
    (setf (node-right node) node)
    (setf (node-parent node) node)
    node))

(defvar *null-node* (make-null-node))

(defun the-null-node () *null-node*)

(defun make-tree-node (data)
  (make-instance 'tree-node :data data :left (the-null-node) :right (the-null-node) :parent (the-null-node) :color :black))

(defmethod tree-node-p ((node tree-node)) t)
(defmethod tree-node-p ((node T)) nil)

(defmethod print-object ((node tree-node) (stream stream))
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "[~S] ~S" (node-data node) (node-color node))))

(defclass red-black-tree ()
  ((order-predicate :initarg :order-predicate :accessor tree-order-predicate)
   (equal-predicate :initarg :equal-predicate :initform 'eql :accessor tree-equal-predicate)
   (key :initarg :key :initform NIL :accessor tree-key)
   (root :initarg :root :initform NIL :accessor tree-root)
   (name :initarg :name :initform NIL :accessor tree-name)))

(defmethod print-object ((tree red-black-tree) (stream stream))
  (print-unreadable-object (tree stream :type t :identity t)
    (when (tree-name tree)
      (format stream "~A " (tree-name tree)))
    (format stream "[~S]" (node-data (tree-root tree)))))

(defmethod initialize-instance :after ((tree red-black-tree) &rest initargs)
  (declare (ignore initiargs))
  (unless (tree-root tree)
    (setf (tree-root tree) (the-null-node))))

(defmethod node= ((tree red-black-tree) data-i data-j)
  (let (ith jth)
    (if (tree-key tree)
      (setq ith (funcall (tree-key tree) data-i)
	    jth (funcall (tree-key tree) data-j))
      (setq ith data-i
	    jth data-j))
    (funcall (tree-equal-predicate tree) ith jth)))

(defmethod node< ((tree red-black-tree) data-i data-j)
  (let (ith jth)
    (if (tree-key tree)
      (setq ith (funcall (tree-key tree) data-i)
	    jth (funcall (tree-key tree) data-j))
      (setq ith data-i
	    jth data-j))
    (funcall (tree-order-predicate tree) ith jth)))

(defmethod node> ((tree red-black-tree) data-j data-i)
  (let (ith jth)
    (if (tree-key tree)
      (setq ith (funcall (tree-key tree) data-i)
	    jth (funcall (tree-key tree) data-j))
      (setq ith data-i
	    jth data-j))
    (funcall (tree-order-predicate tree) ith jth)))

(defmethod node<= ((tree red-black-tree) d1 d2)
  (or (node= tree d1 d2)
      (node< tree d1 d2)))

(defmethod node>= ((tree red-black-tree) d1 d2)
  (or (node= tree d1 d2)
      (node> tree d1 d2)))

(defmethod tree-search ((tree red-black-tree) (node tree-node) datum)
  (tree-do-while 
   (and (not (null-node-p node))
	(not (node= tree datum (node-data node))))
   (if (node< tree datum (node-data node))
     (setq node (node-left node))
     (setq node (node-right node))))
  (and (not (null-node-p node)) node))

(defmethod tree-search< ((tree red-black-tree) (node tree-node) datum)
  "find x from this node such that datum < x and x-datum is minimimized"
  (labels ((ts (p y)
	     (if (null-node-p p) 
	       y
	       (let ((node-data-p (node-data p))) 
		 (cond
					; ((node= tree node-data-p datum) p)
		  ((node> tree node-data-p datum)
		   (if (or (null-node-p y) (node< tree node-data-p (node-data y)))
		     (ts (node-left p) p)
		     (ts (node-left p) y)))
		  (t ;; node< tree node-data-p datum
		   (ts (node-right p) y)))))))
    (let ((res (ts node (the-null-node))))
      (if (null-node-p res) NIL res))))

(defmethod tree-search<= ((tree red-black-tree) (node tree-node) datum)
  "find x from this node such that datum <= x and x-datum is minimimized"
  (labels ((ts (p y)
	     (if (null-node-p p) 
	       y
	       (let ((node-data-p (node-data p))) 
		 (cond
		  ((node= tree node-data-p datum) p)
		  ((node> tree node-data-p datum)
		   (if (or (null-node-p y) (node< tree node-data-p (node-data y)))
		     (ts (node-left p) p)
		     (ts (node-left p) y)))
		  (t ;; node< tree node-data-p datum
		   (ts (node-right p) y)))))))
    (let ((res (ts node (the-null-node))))
      (if (null-node-p res) NIL res))))

(defmethod tree-search> ((tree red-black-tree) (node tree-node) datum)
  "find x from this node such that datum > x and datum-x is minimimized"
  (labels ((ts (p y)
	     (if (null-node-p p) 
	       y
	       (let ((node-data-p (node-data p))) 
		 (cond
					; ((node= tree node-data-p datum) p)
		  ((node< tree node-data-p datum)
		   (if (or (null-node-p y) (node> tree node-data-p (node-data y)))
		     (ts (node-right p) p)
		     (ts (node-right p) y)))
		  (t ;; node> tree node-data-p datum
		   (ts (node-left p) y)))))))
    (let ((res (ts node (the-null-node))))
      (if (null-node-p res) NIL res))))

(defmethod tree-search>= ((tree red-black-tree) (node tree-node) datum)
  "find x from this node such that datum >= x and datum-x is minimimized"
  (labels ((ts (p y)
	     (if (null-node-p p) 
	       y
	       (let ((node-data-p (node-data p))) 
		 (cond
		  ((node= tree node-data-p datum) p)
		  ((node< tree node-data-p datum)
		   (if (or (null-node-p y) (node> tree node-data-p (node-data y)))
		     (ts (node-right p) p)
		     (ts (node-right p) y)))
		  (t ;; node> tree node-data-p datum
		   (ts (node-left p) y)))))))
    (let ((res (ts node (the-null-node))))
      (if (null-node-p res) NIL res))))

(defmacro define-tree-find-method (name search)
  `(defmethod ,name ((tree red-black-tree) datum)
     (let ((node (,search tree (tree-root tree) datum)))
       (if (null-node-p node) (values nil nil) (values (node-data node) t)))))

(define-tree-find-method tree-find tree-search)
(define-tree-find-method tree-find< tree-search<)
(define-tree-find-method tree-find<= tree-search<=)
(define-tree-find-method tree-find> tree-search>)
(define-tree-find-method tree-find>= tree-search>=)


;;;(defmethod tree-find-all ((tree red-black-tree) datum)
;;; (let ((node (tree-search tree (tree-root tree) datum)))
;;;  (if (not node)
;;;   '()
;;;   (let ((finds (list )))
;;;	(tree-do-while 
;;;	 (and (not (null-node-p node))
;;;	   (node= tree
;;;		   datum
;;;		   (node-data node)))
;;;	 (push (node-data node) finds)
;;;	 (setq node (tree-successor tree node)))
;;;	(nreverse finds)))))

(defmethod walk-while ((node tree-node) predicate)
  (if (not (null-node-p node))
    (progn
      (walk-while (node-left node) predicate)
      (when (funcall predicate (node-data node))
	(walk-while (node-right node) predicate)))))

;;;(defmethod tree-find-all ((tree red-black-tree) datum)
;;;  (let ((node (tree-search tree (tree-root tree) datum)))
;;;  (if (not node)
;;;   '()
;;;   (let ((finds (list )))
;;;	(walk-while node 
;;;		  #'(lambda (new)
;;;		    (if (node= tree datum new)
;;;			 (progn (push new finds) T)
;;;			 nil)))
;;;	(nreverse finds)))))

;;;(defmethod tree-find-all2 ((tree red-black-tree) datum)
;;; (let ((results '()))
;;;  (labels ((recurse (this_node)
;;;	    (cond
;;;		((null-node-p this_node))
;;;		(t
;;;		 (push (node-data this_node) results)
;;;		 (recurse (tree-search tree (node-left this_node) datum))
;;;		 (recurse (tree-search tree (node-right this_node) datum))))))
;;;   (recurse (tree-search tree (tree-root tree) datum))
;;;   results)))


(defmethod tree-search-any< ((tree red-black-tree) (node tree-node) datum)
  (cond
   ((null-node-p node) NIL)
   ((node< tree (node-data node) datum) node)
   (T (tree-search-any< tree (node-right node) datum))))

(defmethod tree-search-any<= ((tree red-black-tree) (node tree-node) datum)
  (cond
   ((null-node-p node) NIL)
   ((node<= tree (node-data node) datum) node)
   (T (tree-search-any<= tree (node-right node) datum))))

(defmethod tree-search-any> ((tree red-black-tree) (node tree-node) datum)
  (cond
   ((null-node-p node) NIL)
   ((node> tree (node-data node) datum) node)
   (T (tree-search-any> tree (node-left node) datum))))

(defmethod tree-search-any>= ((tree red-black-tree) (node tree-node) datum)
  (cond
   ((null-node-p node) NIL)
   ((node>= tree (node-data node) datum) node)
   (T (tree-search-any>= tree (node-left node) datum))))

(defmacro define-tree-find-all-method (name search-method-name)
  `(defmethod ,name ((tree red-black-tree) datum)
     (let ((results '()))
       (labels ((recurse (this_node)
		  (cond
		   ((null-node-p this_node))
		   (t
		    (push (node-data this_node) results)
		    (recurse (,search-method-name tree (node-left this_node) datum))
		    (recurse (,search-method-name tree (node-right this_node) datum))))))
	 (recurse (,search-method-name tree (tree-root tree) datum))
	 results))))

(define-tree-find-all-method tree-find-all tree-search)

(define-tree-find-all-method tree-find-all< tree-search-any<)
(define-tree-find-all-method tree-find-all<= tree-search-any<=)
(define-tree-find-all-method tree-find-all> tree-search-any>)
(define-tree-find-all-method tree-find-all>= tree-search-any>=)

(defun null-node-p (node) 
  (or (null node)
      (and (tree-node-p node)
	   (null (node-data node)))))

(defmethod tree-minimum ((tree red-black-tree) &optional (node (tree-root tree)))
  (tree-do-while 
   (not (null-node-p (node-left node)))
   (setq node (node-left node)))
  node)

(defmethod tree-maximum ((tree red-black-tree) &optional (node (tree-root tree)))
  (tree-do-while 
   (not (null-node-p (node-right node)))
   (setq node (node-right node)))
  node)

(defmethod tree-successor ((tree red-black-tree) &optional (node (tree-root tree)))
  "note -- assumes all values are distinct."
  (if (not (null-node-p (node-right node)))
    (tree-minimum tree (node-right node))
    (let ((y (node-parent node)))
      (tree-do-while 
       (and (not (null-node-p y))
	    (eq node (node-right y)))
       (setq node y)
       (setq y (node-parent y)))
      y)))

(defmethod tree-predecessor ((tree red-black-tree) &optional (node (tree-root tree)))
  "note -- assumes all values are distinct."
  (if (not (null-node-p (node-left node)))
    (tree-minimum tree (node-left node))
    (let ((y (node-parent node)))
      (tree-do-while 
       (and (not (null-node-p y))
	    (eq node (node-left y)))
       (setq node y)
       (setq y (node-parent y)))
      y)))

(defmethod basic-tree-insert ((tree red-black-tree) (node tree-node))
  (let ((y (the-null-node))
	(x (tree-root tree)))
    (tree-do-while 
     (not (null-node-p x))
     (setq y x)
     (if (node< tree (node-data node) (node-data x))
       (setq x (node-left x))
       (setq x (node-right x))))
    (setf (node-parent node) y)
    (if (null-node-p y)
      (setf (tree-root tree) node)
      (progn
	(if (node< tree (node-data node) (node-data y))
	  (setf (node-left y) node)
	  (setf (node-right y) node)))))
  node)

(defmethod basic-tree-delete ((tree red-black-tree) (node tree-node))
  (when (tree-search tree (tree-root tree) node)
    (let* ((y 
	    (if (or (null-node-p (node-left node)) (null-node-p (node-right node)))
	      node
	      (tree-successor tree node)))
	   (x 
	    (if (not (null-node-p (node-left y)))
	      (node-left y)
	      (node-right y))))
      (when (not (null-node-p x))
	(setf (node-parent x) (node-parent y)))
      (if (null-node-p (node-parent y))
	(setf (tree-root tree) x)
	(if (eq y (node-left (node-parent y)))
	  (setf (node-left (node-parent y)) x)
	  (setf (node-right (node-parent y)) x)))
      (when (not (eq y node))
	(setf (node-data node) (node-data y)))
      ;; just in case the root turned null as the last node is deleted
      (when (null (tree-root tree))
	(setf (tree-root tree) (the-null-node)))
      y)))

(defmethod in-order-walk ((tree red-black-tree) function)
  (in-order-walk (tree-root tree) function))

(defmethod in-order-walk ((node tree-node) function)
  (unless (null-node-p node)
    (in-order-walk (node-left node) function)
    (funcall function node)
    (in-order-walk (node-right node) function)))

(defun tree-for-each (tree function)
  (in-order-walk tree function))

(defun tree-map (tree function)
  (let ((results (list)))
    (in-order-walk tree #'(lambda (node)
			    (push (funcall function node)
				  results)))
    (nreverse results)))

(defmethod tree->list-identity ((tree red-black-tree))
  (tree-map tree #'identity))

(defmethod tree->list ((tree red-black-tree))
  (tree-map tree #'node-data))

(defmethod left-rotate ((tree red-black-tree) (node tree-node))
  (unless (null-node-p (node-right node))
    (let ((y (node-right node)))
      (setf (node-right node) (node-left y))
      (when (not (null-node-p (node-left y)))
	(setf (node-parent (node-left y)) node))
      (setf (node-parent y) (node-parent node))
      (if (null-node-p (node-parent node))
	(setf (tree-root tree) y)
	(if (eq node (node-left (node-parent node)))
	  (setf (node-left (node-parent node)) y)
	  (setf (node-right (node-parent node)) y)))
      (setf (node-left y) node)
      (setf (node-parent node) y)))
  (values))

(defmethod right-rotate ((tree red-black-tree) (node tree-node))
  (unless (null-node-p (node-left node))
    (let ((x (node-left node)))
      (setf (node-left node) (node-right x))
      (when (not (null-node-p (node-right x)))
	(setf (node-parent (node-right x)) node))
      (setf (node-parent x) (node-parent node))
      (if (null-node-p (node-parent node))
	(setf (tree-root tree) x)
	(if (eq node (node-left (node-parent node)))
	  (setf (node-left (node-parent node)) x)
	  (setf (node-right (node-parent node)) x)))
      (setf (node-right x) node)
      (setf (node-parent node) x)))
  (values))

(defmethod tree-insert ((tree red-black-tree) (node tree-node))
  (basic-tree-insert tree node)
  (setf (node-color node) :red)
  (tree-do-while
   (and (not (eq node (tree-root tree)))
	(eq (node-color (node-parent node)) :red))
   (if (eq (node-parent node) (node-left (node-parent (node-parent node))))
     (let ((y (node-right (node-parent (node-parent node)))))
       ;; (print "Entering 'LEFT' branch.")
       (if (eq (node-color y) :red)
	 (progn
	   ;; (print "Case Left 1.")
	   (setf (node-color (node-parent node)) :black)
	   (setf (node-color y) :black)
	   (setf (node-color (node-parent (node-parent node))) :red)
	   (setf node (node-parent (node-parent node))))
	 (progn
	   (when (eq node (node-right (node-parent node)))
	     ;; (print "Case Left 2.")
	     (setq node (node-parent node))
	     (left-rotate tree node))
	   ;; (print "Case Left 3.")
	   (setf (node-color (node-parent node)) :black)
	   (setf (node-color (node-parent (node-parent node))) :red)
	   (right-rotate tree (node-parent (node-parent node))))))
     (let ((y (node-left (node-parent (node-parent node)))))
       ;; (print "Entering 'RIGHT' branch.")
       (if (eq (node-color y) :red)
	 (progn
	   ;; (print "Case Right 1.")
	   (setf (node-color (node-parent node)) :black)
	   (setf (node-color y) :black)
	   (setf (node-color (node-parent (node-parent node))) :red)
	   (setf node (node-parent (node-parent node))))
	 (progn
	   (when (eq node (node-left (node-parent node)))
	     ;; (print "Case Right 2.")
	     (setq node (node-parent node))
	     (right-rotate tree node))
	   ;; (print "Case Right 3.")
	   (setf (node-color (node-parent node)) :black)
	   (setf (node-color (node-parent (node-parent node))) :red)
	   (left-rotate tree (node-parent (node-parent node))))))))
  ;; (print "Recoloring root node.")
  (setf (node-color (tree-root tree)) :black)
  node)

(defmethod tree-insert ((tree red-black-tree) (node T))
  (tree-insert tree (make-tree-node node)))

(defmethod tree-delete-fixup ((tree red-black-tree) (node tree-node))
  (tree-do-while 
   (and (not (eq node (tree-root tree)))
	(eq (node-color node) :black))
   (if (eq node (node-left (node-parent node)))
     (let ((w (node-right (node-parent node))))
       ;; (print-all "LEFT CASES. Setting W. (node-parent node) is " (node-parent node))
       ;; (print-all "Setting W. (node-right (node-parent node)) is " w)
       (when (eq (node-color w) :red)
	 ;; (print "Case Left 1.") 
	 (setf (node-color w) :black)
	 (setf (node-color (node-parent node)) :red)
	 (left-rotate tree (node-parent node))
	 (setq w (node-right (node-parent node))))
       (if (and (eq (node-color (node-left w)) :black)
		(eq (node-color (node-right w)) :black))
	 (progn
	   ;; (print "Case Left 2.") 
	   (setf (node-color w) :red)
	   (setf node (node-parent node)))
	 (progn
	   (when (eq (node-color (node-right w)) :black)
	     ;; (print "Case Left 3.") 
	     (setf (node-color (node-left w)) :black)
	     (setf (node-color w) :red)
	     (right-rotate tree w)
	     (setq w (node-right (node-parent node))))
	   ;; (print "Case Left 4.") 
	   (setf (node-color w) (node-color (node-parent node)))
	   (setf (node-color (node-parent node)) :black)
	   (setf (node-color (node-right w)) :black)
	   (left-rotate tree (node-parent node))
	   (setq node (tree-root tree)))))
     (let ((w (node-left (node-parent node))))
       ;; (print-all "RIGHT CASES. Setting W. (node-parent node) is " (node-parent node))
       ;; (print-all "Setting W. (node-left (node-parent node)) is " w)
       (when (eq (node-color w) :red)
	 ;; (print "Case Right 1.") 
	 (setf (node-color w) :black)
	 (setf (node-color (node-parent node)) :red)
	 (right-rotate tree (node-parent node))	;;; rotation here
	 (setq w (node-left (node-parent node))))
       (if (and (eq (node-color (node-right w)) :black)
		(eq (node-color (node-left w)) :black))
	 (progn
	   ;; (print "Case Right 2.") 
	   (setf (node-color w) :red)
	   (setf node (node-parent node)))
	 (progn
	   (when (eq (node-color (node-left w)) :black)
	     ;; (print "Case Right 3.") 
	     (setf (node-color (node-right w)) :black)
	     (setf (node-color w) :red)
	     (left-rotate tree w) ;;; rotation here
	     (setq w (node-left (node-parent node))))
	   ;; (print "Case Right 4.") 
	   (setf (node-color w) (node-color (node-parent node)))
	   (setf (node-color (node-parent node)) :black)
	   (setf (node-color (node-left w)) :black)
	   (right-rotate tree (node-parent node)) ;;; rotation here
	   (setq node (tree-root tree))))
       )))
  (setf (node-color node) :black)
  node)

(defmethod tree-delete ((tree red-black-tree) (node tree-node))
  (let* ((y 
	  (if (or (null-node-p (node-left node))
		  (null-node-p (node-right node)))
	    node 
	    (tree-successor tree node)))
	 (x
	  (if (not (null-node-p (node-left y)))
	    (node-left y)
	    (node-right y)))
	 )
    (setf (node-parent x) (node-parent y))
    (if (null-node-p (node-parent y))
      (setf (tree-root tree) x)
      (if (eq y (node-left (node-parent y)))
	(setf (node-left (node-parent y)) x)
	(setf (node-right (node-parent y)) x)))
    (when (not (eq y node))
      (setf (node-data node) (node-data y)))
    (when (eq (node-color y) :black)
      (tree-delete-fixup tree x))
    y))

(defmethod tree-delete ((tree red-black-tree) datum)
  (let ((node (tree-search tree (tree-root tree) datum)))
    (if node (tree-delete tree node) NIL)))

;; -- below is a bunch of testing code.
;; -------------------------------------

(defmethod tree->tree-list ((tree red-black-tree) &optional (node (tree-root tree)))
  (cond
   ((null-node-p node) '())
   ((null node) '())
   ((and (null-node-p (node-left node))
	 (null-node-p (node-right node)))
    (node-data node))
   ((null-node-p (node-right node))
    (list (node-data node) (tree->tree-list tree (node-left node)) NIL))
   (t 
    (list (node-data node) 
	  (tree->tree-list tree (node-left node))
	  (tree->tree-list tree (node-right node))))))

(defmethod node-children ((node tree-node))
  (list (node-left node) (node-right node)))

(defmethod show-node ((node tree-node)
		      &optional (stream *standard-output*) 
		      &aux shown)
  (labels ((show (inode prefix)
	     (let ((specs (node-children inode)))
	       (cond ((eq inode (the-null-node))
		      (format stream "#<THE-NULL-NODE ~s>~%" (node-color inode)))
		     ((member inode shown)
		      (format stream "~S <-- duplicate~%" inode))
		     (t
		      (format stream "~S~%" inode)
		      (push inode shown)
		      (when specs
			(do ((next-prefix (format nil "~A |  " prefix))
			     (last-prefix (format nil "~A   " prefix))
			     (l specs (cdr l)))
			    ((null (cdr l))
			     (format stream "~A +-- " prefix)
			     (show (car l) last-prefix))
			  (format stream "~A |-- " prefix)
			  (show (car l) next-prefix))))))))
    (show node "")
    (values)))

(defmethod tree-leaves ((tree red-black-tree))
  (let ((leaves (list)))
    (in-order-walk tree
		   #'(lambda (node)
		       (when (and (null-node-p (node-left node))
				  (null-node-p (node-right node)))
			 (push node leaves))))
    (nreverse leaves)))

(defmethod tree-interiors ((tree red-black-tree))
  (let ((leaves (list)))
    (in-order-walk tree
		   #'(lambda (node)
		       (unless (and (null-node-p (node-left node))
				    (null-node-p (node-right node)))
			 (push node leaves))))
    (nreverse leaves)))



(defmethod all-paths ((tree red-black-tree) (node tree-node))
  (declare (notinline all-paths))
  (cond
   ((null-node-p node) 
    '())
   ((every #'null-node-p (node-children node))
    (list (list node)))
   (t
    (loop for child in (node-children node)
	appending
	  (loop for path in (all-paths tree child)
	      collecting
		(cons node path))))))

(defun black-count (path)
  (loop for node in path
      if (eq (node-color node) :black)
      count node))

(defmethod satisfies-rb-property-1 ((tree red-black-tree) (node tree-node))
  (or (eq (node-color node) :black)
      (eq (node-color node) :red)))

(defmethod satisfies-rb-property-2 ((tree red-black-tree) (node tree-node))
  (if (null-node-p node)
    (eq (node-color node) :black)
    t))

(defmethod satisfies-rb-property-3 ((tree red-black-tree) (node tree-node))
  (if (eq (node-color node) :red)
    (every #'(lambda (child)
	       (eq (node-color child) :black))
	   (node-children node))
    t))

(defmethod satisfies-rb-property-4 ((tree red-black-tree) (node tree-node))
  "every simple path from a node to a descendant left contains same number of black nodes"
  (apply #'= (mapcar #'black-count (all-paths tree node))))

(defmethod red-black-tree-p ((tree red-black-tree))
  (every #'(lambda (node)
	     (and
	      (satisfies-rb-property-1 tree node)
	      (satisfies-rb-property-2 tree node)
	      (satisfies-rb-property-3 tree node)
	      (satisfies-rb-property-4 tree node)))
	 (tree->list-identity tree)))

(defmethod red-black-tree-inspect ((tree red-black-tree) (node tree-node) &optional (stream *standard-output*))
  (unless (satisfies-rb-property-1 tree node)
    (format stream "~&;; Node ~S fails property 1" node))
  (unless (satisfies-rb-property-2 tree node)
    (format stream "~&;; Node ~S fails property 2" node))
  (unless (satisfies-rb-property-3 tree node)
    (format stream "~&;; Node ~S fails property 3" node))
  (unless (satisfies-rb-property-4 tree node)
    (format stream "~&;; Node ~S fails property 4" node)))

(defmethod red-black-tree-inspection ((tree red-black-tree) &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (declare (special *standard-output*))
    (in-order-walk tree #'(lambda (node) (red-black-tree-inspect tree node)))))


(defmethod tree-test-still-red-black ((tree red-black-tree) &optional (stream *standard-output*))
  (if (red-black-tree-p tree)
    T
    (let ((*standard-output* stream))
      (progn (red-black-tree-inspection tree) NIL))))

(defmethod tree-test-still-deleted ((tree red-black-tree) datum &optional (stream *standard-output*))
  (if (not (tree-search tree (tree-root tree) datum))
    T
    (format stream "~&Deletion failure on ~S" datum)))

(defmethod tree-test-inserted ((tree red-black-tree) datum &optional (stream *standard-output*))
  (if (tree-search tree (tree-root tree) datum)
    T
    (format stream "~&Insertion failure on ~S" datum)))

(defmethod tree-test-still-remains ((tree red-black-tree) data &optional (stream *standard-output*))
  (let ((good? T))
    (dolist (datum data)
      (when (not (tree-search tree (tree-root tree) datum))
	(setq good? NIL)
	(format stream "~&Find failure on ~S" datum)))
    good?))

(defvar *max-size* 100)
(defun random-item-not-in-list (currents)
  (do ((n (random (* 2 *max-size*)) (random (* 2 *max-size*))))
      ((not (member n currents)) n)))

(defmethod tree-test-insertion ((tree red-black-tree) currents)
  (let ((rfloat (random-item-not-in-list currents)))
    (tree-insert tree (make-tree-node rfloat))
    (cons rfloat currents)))

(defmethod tree-test-deletion ((tree red-black-tree) currents)
  (flet ((nshuffle (sequence)
	   "return a sequence in shuffled, random order."
	   (let ((l (length sequence)))
	     (dotimes (i l)
	       (rotatef (elt sequence i)
			(elt sequence (random l))))
	     sequence)))
    (setq currents (nshuffle currents))
    (let ((rfloat (car currents)))
      (if rfloat
	(progn
	  (tree-delete tree rfloat)
	  (values rfloat (cdr currents)))
	(values NIL currents)))))

(defmethod test-once ((tree red-black-tree) currents max-size &optional (stream *standard-output*))
  (if (> (/ (length currents) max-size) (random 1.0))
    (multiple-value-bind (deleted newcurr) (tree-test-deletion tree currents)
      (if deleted
	(progn 
	  (setq currents newcurr)
	  (tree-test-still-red-black tree stream)
	  (tree-test-still-deleted tree deleted stream)
	  (tree-test-still-remains tree currents stream))))
    (let ((added (tree-test-insertion tree currents)))
      (setq currents added)
      (tree-test-still-red-black tree stream)
      (tree-test-inserted tree (car currents) stream)
      (tree-test-still-remains tree (rest currents) stream)))
  currents)

(defun run-tree-tests (trials max-size &optional (report-after 1000) (stream *standard-output*))
  (let ((tree (make-instance 'red-black-tree :order-predicate #'<))
	(currents (list))
	(*max-size* max-size))
    (declare (special *max-size*))
    (dotimes (i trials)
      (if (and report-after (= 0 (mod (1+ i) report-after)))
	(format stream "~&~S Trials run." (1+ i)))
      (setq currents (test-once tree currents max-size stream)))
    (cons tree currents)))

(defun run-tree-tests-insertions (trials &optional (report-after 10) (stream *standard-output*))
  (let ((tree (make-instance 'red-black-tree :order-predicate #'<))
	(currents (list)))
    (dotimes (i trials)
      (when (and report-after (= 0 (mod (1+ i) report-after)))
	(format stream "~&~S Trials run." (1+ i))
	(setq tree (make-instance 'red-black-tree :order-predicate #'<))
	(setq currents (list)))
      (setq currents (tree-test-insertion tree currents))
      (tree-test-still-red-black tree stream))
    (cons tree currents)))

(defun list->tree (l &optional (predicate #'<))
  (let ((tr (make-instance 'red-black-tree :order-predicate predicate)))
    (dolist (i l) (tree-insert tr (make-tree-node i)))
    tr))

(defmethod make-load-form ((node tree-node) &optional environment)
  (declare (ignore environment))
  `(let ((snode (make-tree-node ,(if (find-method #'make-load-form '() (list (class-of (node-data node))) nil)
				   (make-load-form (node-data node))
				   (node-data node)))))
     (setf (node-color snode) ,(node-color node))
     ,@(if (not (null-node-p (node-left node)))
	 `((setf (node-left snode) ,(make-load-form (node-left node)))
	   (setf (node-parent (node-left snode)) snode)))
     ,@(if (not (null-node-p (node-right node)))
	 `((setf (node-right snode) ,(make-load-form (node-right node)))
	   (setf (node-parent (node-right snode)) snode)))
     snode))

(defmethod make-load-form ((tree red-black-tree) &optional environment)
  (declare (ignore environment))
  `(make-instance 'red-black-tree
     :order-predicate ',(tree-order-predicate tree)
     :equal-predicate ',(tree-equal-predicate tree)
     :key ,(tree-key tree)
     :root ,(make-load-form (tree-root tree))))


(defmethod tree-pop ((tree red-black-tree))
  (if (eq (tree-root tree)
	  (the-null-node))
    NIL
    (let ((timen (tree-minimum tree (tree-root tree))))
      (tree-delete tree timen)
      (node-data timen))))

(defmethod tree-first ((tree red-black-tree))
  (if (eq (tree-root tree)
	  (the-null-node))
    NIL
    (let ((timen (tree-minimum tree (tree-root tree))))
      (node-data timen))))

(defmethod tree-empty-p ((tree red-black-tree))
  (eq (tree-root tree)
      (the-null-node)))


;;-- trees with locks ...

(defclass synched-red-black-tree (red-black-tree)
  ((semaphore :accessor tree-semaphore :initform (proc.make-queue "Queue semaphore"))))

(defmethod tree-root :around ((tree synched-red-black-tree))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-search :around ((tree synched-red-black-tree) (node tree-node) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-search< :around ((tree synched-red-black-tree) (node tree-node) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-search<= :around ((tree synched-red-black-tree) (node tree-node) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-search> :around ((tree synched-red-black-tree) (node tree-node) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-search>= :around ((tree synched-red-black-tree) (node tree-node) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find< :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find<= :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find> :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find>= :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find-all :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find-all< :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find-all<= :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find-all> :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-find-all>= :around ((tree synched-red-black-tree) datum)
  (declare (ignore datum))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-minimum :around ((tree synched-red-black-tree) &optional (node (tree-root tree)))
  (declare (ignore node))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-maximum :around ((tree synched-red-black-tree) &optional (node (tree-root tree)))
  (declare (ignore node)) 
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-successor :around ((tree synched-red-black-tree) &optional (node (tree-root tree)))
  (declare (ignore node))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-predecessor :around ((tree synched-red-black-tree) &optional (node (tree-root tree)))
  (declare (ignore node))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-in-order-walk :around ((tree synched-red-black-tree) function)
  (declare (ignore function))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-insert :around ((tree synched-red-black-tree) (node tree-node))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-delete :around ((tree synched-red-black-tree) (node tree-node))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree->tree-list :around ((tree synched-red-black-tree) &optional (node (tree-root tree)))
  (declare (ignore node))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))

(defmethod tree-pop :around ((tree synched-red-black-tree))
  (proc.with-queue (tree-semaphore tree) (call-next-method)))


(defun create-red-black-tree (order-predicate &key (equal-predicate 'eql) (key nil) (locked-p nil) (name nil))
  (if locked-p
    (make-instance 'synched-red-black-tree
      :order-predicate order-predicate
      :equal-predicate equal-predicate
      :name name
      :key key)
    (make-instance 'red-black-tree
      :order-predicate order-predicate
      :equal-predicate equal-predicate
      :name name
      :key key)))

