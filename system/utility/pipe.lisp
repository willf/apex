;;;-*- Mode: Lisp; Package: apex.utility.pipe -*-
;;;
;;; apex/system/utility/pipe.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: pipe.lisp,v 1.7 2006/01/15 03:43:03 dalal Exp $
;;; Created:     28 January 1996

;; Description: Pipes, as described in Norvig, and elaborated.
;; 
;;
;;----------------------------------------------------------------------

#|
"
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
"
|#

(defpackage :apex.utility.pipe 
  (:use :common-lisp)
  (:export 
   #:append-pipes #:empty-pipe #:enumerate #:filter-pipe #:enumerate-pipe
   #:make-pipe #:map-pipe #:map-pipe-filtering #:mappend-pipe
   #:mappend-pipe-filtering #:pipe-elt #:pipe-head #:pipe-tail
   #:interleave-pipes #:pairs-pipe #:merge-pipes #:multi-append-pipes #:list-pipe
   #:combine-pipes #:cartesian-product-pipe
   #:empty-pipe-p)
  )

(in-package :apex.utility.pipe)


(defmacro make-pipe (head tail)
  "create a pipe by eval'ing head and delaying tail."
  `(cons ,head #'(lambda () ,tail)))

(defun pipe-tail (pipe)
  "return tail of pipe or list, and destructively update 
   the tail if it is a function."
  ;; This assumes that pipes will never contain functions as values...
  (if (functionp (rest pipe))
    (setf (rest pipe) (funcall (rest pipe)))
    (rest pipe)))

(defun pipe-head (pipe) (first pipe))

(defun pipe-elt (pipe i)
  "ith element of pipe, 0 based."
  (if (= i 0) (pipe-head pipe)
     (pipe-elt (pipe-tail pipe) (- i 1))))

(defconstant empty-pipe nil)

(defun enumerate-pipe (pipe &key count key (result pipe))
  "go through all or count elements of pipe,
   possibly applying the key function. "
  (if (or (eq pipe empty-pipe) (eql count 0))
    result
    (progn
      (unless (null key) (funcall key (pipe-head pipe)))
      (enumerate-pipe (pipe-tail pipe)
                 :count (if count (1- count))
                 :key key
                 :result result))))

;;;(defun force (pipe)
;;;  (enumerate-pipe pipe))

;;; incorrect version-- as in Norvig.
;(defun filter-pipe (predicate pipe)
;  "keep only items in (non-null) pipe satisfying predicate"
;  (if (funcall predicate (head pipe))
;    (make-pipe (head pipe) (filter-pipe predicate (tail pipe)))
;    (filter-pipe predicate (tail pipe))))

(defun filter-pipe (predicate pipe)
  "keep only items in (non-null) pipe satisfying predicate"
     (if (eq pipe empty-pipe)
      empty-pipe
      (let ((head (pipe-head pipe))
            (tail (pipe-tail pipe)))
      (if (funcall predicate head)
        (make-pipe head (filter-pipe predicate tail))
        (filter-pipe predicate tail)))))
               

(defun map-pipe (fn pipe)
  "Map fn over pipe, delaying all but the first fn call,
   collecting results"
  (if (eq pipe empty-pipe)
    empty-pipe
    (make-pipe (funcall fn (pipe-head pipe))
               (map-pipe fn (pipe-tail pipe)))))


;;;(defun map-pipe-filtering (fn pipe &optional filter-test)
;;;  "Map fn over pipe, delaying all but the first fn call,
;;;   collecting results if non-NULL or filter-test on element is 
;;;   non-NULL"
;;;  (if (eq pipe empty-pipe)
;;;    empty-pipe
;;;    (let* ((head (pipe-head pipe))
;;;           (tail (pipe-tail pipe))
;;;           (result (funcall fn head)))
;;;      (if (or (and filter-test (funcall filter-test result))
;;;              result)
;;;        (make-pipe result (map-pipe-filtering fn tail filter-test))
;;;        (map-pipe-filtering fn tail filter-test)))))
;;;
;;; the 'functional' version above resulted in a stack overflow error
;;; under ACL. Here's an iterative version instead. Man, it's ugly.

(defun map-pipe-filtering (fn pipe &optional filter-test)
  (do* ((pipe pipe (pipe-tail pipe))
	;; it's possible that the last item will not pass the test, so we might still get
	;; an empty pipe.
	;; note: I hate using WHEN for NIL side-effects, but IF is too long ... - wf
	(result (when (not (eq pipe empty-pipe)) (funcall fn (pipe-head pipe)))
		(when (not (eq pipe empty-pipe)) (funcall fn (pipe-head pipe))))
	(test-result (and (not (eq pipe empty-pipe)) filter-test (funcall filter-test result))
		     (and (not (eq pipe empty-pipe)) filter-test (funcall filter-test result))))
      ((or (eq pipe empty-pipe)
	   test-result
	   result)
       (if (or test-result result)
	 (if (eq (pipe-tail pipe) empty-pipe)
	   (list result) ;; a mild efficiency
	   (make-pipe result (map-pipe-filtering fn (pipe-tail pipe) filter-test)))
	 empty-pipe))))


(defun append-pipes (pipex pipey)
  "return a pipe that appends two pipes"
  (if (eq pipex empty-pipe)
    pipey
    (make-pipe (pipe-head pipex)
               (append-pipes (pipe-tail pipex) pipey))))

(defun mappend-pipe (fn pipe)
  "lazily map fn over pipe, appending results"
  (if (eq pipe empty-pipe)
    empty-pipe
    (let ((x (funcall fn (pipe-head pipe))))
      (make-pipe (pipe-head x)
                 (append-pipes (pipe-tail x)
                               (mappend-pipe fn (pipe-tail pipe)))))))

(defun mappend-pipe-filtering (fn pipe &optional filter-test)
  "Map fn over pipe, delaying all but the first fn call,
   appending results, filtering along the way"
  (if (eq pipe empty-pipe)
    empty-pipe
    (let* ((head (pipe-head pipe))
           (tail (pipe-tail pipe))
           (result (funcall fn head)))
      (if (or (and filter-test (funcall filter-test result))
              result)
        (make-pipe (pipe-head result)
                   (append-pipes (pipe-tail result)
                                 (mappend-pipe-filtering fn tail filter-test)))
        (mappend-pipe-filtering fn tail filter-test)))))


;; if none given, interleaves one from P1, then from P2, etc.
(defun merge-pipes (p1 p2 &optional (compare (lambda (a b) (declare (ignore a b)) t)))
  (cond
   ((eq p1 empty-pipe) p2)
   ((eq p2 empty-pipe) p1)
   (t 
    (if (funcall compare (pipe-head p1) (pipe-head p2))
      (make-pipe 
       (pipe-head p1)
       (merge-pipes p2 (pipe-tail p1) compare))
      (make-pipe
       (pipe-head p2)
       (merge-pipes p1 (pipe-tail p2) compare))))))

;; interleave two pipes -- like merge, but no compare
(defun interleave-pipes (p1 p2)
  (if (eq p1 empty-pipe)
    p2
    (make-pipe (pipe-head p1)
               (interleave-pipes p2 (pipe-tail p1)))))

;;;;; return all pairs from p1 and p2 (cartesian product, as it were).
;;;(defun pairs-pipe (p1 p2)
;;;  (if (eq p1 empty-pipe) 
;;;    empty-pipe
;;;    (append-pipes 
;;;     (map-pipe
;;;      (lambda (p)
;;;	(list (pipe-head p1) p))
;;;      p2)
;;;     (pairs-pipe (pipe-tail p1) p2))))
;;; see below ... the above code doesn't work on (pairs-pipe (integers) (integers))
  
;; random

(defun empty-pipe-p (pipe)
  (eq pipe empty-pipe))

(defun multi-append-pipes (pipe-of-pipes)
  (if (empty-pipe-p pipe-of-pipes)
    empty-pipe
    (if (empty-pipe-p (pipe-head pipe-of-pipes))
      (multi-append-pipes (pipe-tail pipe-of-pipes))
      (make-pipe (pipe-head (pipe-head pipe-of-pipes))
		 (multi-append-pipes 
		  (make-pipe (pipe-tail (pipe-head pipe-of-pipes))
			     (pipe-tail pipe-of-pipes)))))))


(defun list-pipe (&rest items)
  (make-pipe (car items) (cdr items)))

;;; ((a b) (1 2 3) (r s t)) --> ((a 1 r) (a 1 s) (a 1 t) (a 2 r) (a 2 s) (a 2 t) (a 3 r) (a 3 s) (a 3 t) (b 1 r) ...)
(defun combine-pipes (pipes)
  (if (empty-pipe-p pipes) empty-pipe
      (let ((combos (combine-pipes (pipe-tail pipes))))
	(mappend-pipe 
	 (lambda (item)
	   (if (not (empty-pipe-p combos))
	     (map-pipe 
	      (lambda (combo)
		(cons item (enumerate-pipe combo))) ;; enumerate this particular combination
	      combos)
	     (list (list item))))
	 (pipe-head pipes)))))

(defun cartesian-product-pipe (&rest pipes)
  (combine-pipes pipes))

(defun pairs-pipe (pipe1 pipe2)
  (cartesian-product-pipe pipe1 pipe2))

#|

(defun integers (&optional (start 0) end)
  "a pipe of integers from START to END."
  (if (or (null end) (<= start end))
    (make-pipe start (integers (+ start 1) end))
    nil))

          

                 
|#