;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/rat.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: rat.lisp,v 1.1 2006/03/13 19:22:37 will Exp $

(in-package :common-lisp-user)

;;; ------ Resource Allocation Tables

;;; The resource-allocation-table agent slot contains a structure of
;;; type RAT.  The ASA uses it to keep track of owenerships and attempts
;;; to gain ownership by tasks.  Note that each resource is assumed to
;;; be ownable at multiple levels ranging from 1 to 10, with each level
;;; representing a time-scale over which the designated task is the
;;; primary owner.  Tasks controlling a resource over the smallest
;;; time-scale (10) can issue control-signals to the resource, thereby
;;; changing its state.  (E.g. controlling the visual-attention resource
;;; at level 10 allows a task to shift attn locus).

;;; ! move from 1-10 notation to linear time units.

(defparameter *default-ownership-level* 10)

(defclass rat (appob)
  ((owners
    :type list  ; of own declarations of form (<res> <range> <task>)
    :accessor owners
    :initform nil
    :initarg :owners)
   (monitors 
    :type hash-table  ; ! type?
    :accessor monitors
    :initform nil
    :initarg :monitors)
   (best
    :type list  ; of pending ownership changes: (<task> <ints> <bumps>)
    :accessor best
    :initform nil
    :initarg :best)))

;;; The following is an ADT for RAT, under development.
;;; ! (KMD) RAT's slots would better be converted into structures

;;; ---- begin

(defun res-owner-resource (owner)       ; (? (int int) ?) -> ?
  (first owner))

(defun res-owner-range (owner)          ; (? (int int) ?) -> (int int)
  (second owner))

(defun res-owner-task (owner)           ; (? (int int) ?) -> ?
  (third owner))

;;; ---- end

;;; The <ints> value is a list of tasks to interrupt if the <task> remains best
;;; through to the point where the internal loop of ASAMain becomes quiescent.
;;; At that point, the listed interrupts are generated and the <ints> value set
;;; to nil.  The <task> may still get bumped from the best list, particularly by
;;; a winddown task associated with an interrupted task.

;;; The <bumps> value lists tasks which were bumped from the best list by
;;; <task>.  If <task> is itself bumped, these tasks get a new resource grab
;;; attempt.  Entries in <bumps> are of the form (<task> <res> <low> <high>)
;;; indicating the task to be bumped and the resource conflict (possibly of
;;; several) that caused it.

(defun add-apex-resource-monitor (mon agent)
  (when (null (monitors (resource-allocation-table agent)))
    (setf (monitors (resource-allocation-table agent))
      (make-hash-table :test 'eq)))
  (setf (gethash (first (expr mon))
		 (monitors (resource-allocation-table agent)))
    (cons mon (gethash (first (expr mon))
		 (monitors (resource-allocation-table agent))))))

(defun remove-resource-monitor (mon agent)
  (when (null (monitors (resource-allocation-table agent)))
    (setf (monitors (resource-allocation-table agent))
      (make-hash-table :test 'eq)))
  (setf (gethash (first (expr mon))
		 (monitors (resource-allocation-table agent)))
    (remove mon (gethash (first (expr mon))
		 (monitors (resource-allocation-table agent))))))
