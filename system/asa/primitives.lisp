;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/primitives.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: primitives.lisp,v 1.14 2006/01/15 03:43:00 dalal Exp $

;;; PDL Primitive Procedures

(in-package :user)

;;; Structure into which a PRIMTIVE form is compiled.
;;;  - the attributes-mixin is solely to support the LOCALS clause

(defclass primitive-procedure (procedure attributes-mixin)
  ;; !- refine (e.g. make immutable where possible and sensible)
  ((suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(update-action start-action completion-action))
   (returnval                           ; LispExpression
    :accessor returnval
    :initarg :returnval
    :initform nil)
   (update-interval                     ; TimeExpression
    :accessor update-interval
    :initarg :update-interval
    :initform nil)
   ;; Action to perform upon when associated activity is updated (always
   ;; co-exists with update-interval)
   (update-action                           ; LispExpression
    :accessor update-action
    :initarg :update-action
    :initform nil)
   ;; Action to perform when associated activity starts
   (start-action                            ; LispExpression
    :accessor start-action
    :initarg :start-action
    :initform nil)
   ;; Action to perform when associated activity completes
   (completion-action                       ; LispExpression
    :accessor completion-action
    :initarg :completion-action
    :initform nil)))

(defmethod copy-procedure ((source primitive-procedure))
  ;; -> primitive-procedure
  (let ((copy (copy-procedure1 source)))
    (setf (attributes copy) (attributes source))
    copy))

;; !- temporary: proctype will be obsoleted
(defmethod initialize-instance :after ((p primitive-procedure) &rest initargs)
  (declare (ignore initargs))
  (setf (proctype p) 'primitive))


;;; -------------- PRIMITIVE form

(defmacro primitive (&rest specs)
  (let ((pvar (gensym)))
  `(let ((,pvar (make-primitive ',specs)))
     (install-procedure-in-library ,pvar)
     ,pvar)))

(defun make-primitive (specs)
  (let* ((canon (make-canonical-primitive specs))
	 (proc (make-pdl-object canon)))
    (setf (pdl proc) `(primitive ,@specs))
    (setf (canonical-pdl proc) canon)
    proc))

(defun make-canonical-primitive (specs)
  `(primitive ,@(cleanup-primitive-specs specs)))

(defun cleanup-primitive-specs (specs)
  (cleanup-primitive-specs1
   (canonicalize-index specs 'primitive)))

(defun cleanup-primitive-specs1 (specs)
  (loop for spec in specs appending
        (cond 
         ((eql (car spec) 'profile)
          (cleanup-profile-parameters (cdr spec)))
         (t (list spec)))))

;;;=====================================================================
;;; PRIMITIVE AST
;;;=====================================================================


(defvar *primitive-ast-feature-requirements* '()
  "AST feature requirements for Primitives")

(defmacro define-primitive-requirement (requirement)
  `(define-assoc-in-global *primitive-ast-feature-requirements* ',requirement))

(defmethod ast-feature-requirements ((type (eql 'primitive)))
  *primitive-ast-feature-requirements*)

(defmethod initialize-pdl-object ((type (eql 'primitive)))
  (make-instance 'primitive-procedure))


;;;---------------------------------------------------------------------
;;; Primitive clause: INDEX 
;;;---------------------------------------------------------------------
;;; User canonical form: 
;;;  (INDEX (<name> . <parameters>)) | (<name> . <parameters>)
;;; Syntax canonical form:
;;;  (INDEX (<name> . <parameters>)) | (<name> . <parameters>)

(define-primitive-requirement 
    (index (? . ?) :unique))


;;;---------------------------------------------------------------------
;;; Primitive clause: PROFILE
;;;---------------------------------------------------------------------
;;; user canonical form:
;;; (PROFILE (<resource> <duration> <continuity>)+ )
;;; syntax canonical form: for each resource:
;;; (PROFILE (resource <resource>)
;;;          (duration <duration>)
;;;          (continuity <continuity>))
;;; for each resource.

(define-primitive-requirement
    (profile ((resource ?)
             (duration ?)
             (continuity ?))
    :kleene-star))



;;;---------------------------------------------------------------------
;;; Primitive clause: LOCALS
;;;---------------------------------------------------------------------
;;; user and syntax canonical form:
;;; (LOCALS ((<name> <value>)+)

(define-primitive-requirement
 (locals (? . ?)
         :optional))

(defmethod define-pdl-feature ((proc primitive-procedure)
                               (attr (eql 'locals))
                               (locals list))
  (u-setf (attributes proc) 
          (mapcar
           (lambda (list-pair) (cons (car list-pair) (cadr list-pair)))
           locals)))


;;;---------------------------------------------------------------------
;;; Primitive clause: ON-START
;;;---------------------------------------------------------------------
;;; user and syntax canonical form:
;;; (ON-START <lisp-form>+)

(define-primitive-requirement
 (on-start (? . ?)
           :optional))

(defmethod define-pdl-feature ((proc primitive-procedure)
                               (attr (eql 'on-start))
                               (on-start list))
  (u-setf (start-action proc) (cons 'progn on-start)))


;;;---------------------------------------------------------------------
;;; Primitive clause: ON-COMPLETION
;;;---------------------------------------------------------------------
;;; user and syntax canonical form:
;;; (ON-COMPLETION <lisp-form>+)

(define-primitive-requirement
 (on-completion (? . ?)
                :optional))

(defmethod define-pdl-feature ((proc primitive-procedure)
                               (attr (eql 'on-completion))
                               (on-completion list))
  (u-setf (completion-action proc) (cons 'progn on-completion)))


;;;---------------------------------------------------------------------
;;; Primitive clause: UPDATE
;;;---------------------------------------------------------------------
;;; user and syntax canonical form:
;;; (ON-UPDATE <lisp-form>+)

(define-primitive-requirement
  (update ?
          :optional))

(defmethod define-pdl-feature ((proc primitive-procedure)
                               (attr (eql 'update))
                               update)
  (cond ((time-expression? (car update) t)
         (u-setf (update-interval proc) (car update))
         (u-setf (update-action proc) (cons 'progn (cdr update))))
        ((quoted-time-expression? (car update) t)
         (u-setf (update-interval proc) (cadar update))
         (u-setf (update-action proc) (cons 'progn (cdr update))))
        (t (u-setf (update-action proc) (cons 'progn update)))))


;;;---------------------------------------------------------------------
;;; Primitive clause: DURATION
;;;---------------------------------------------------------------------
;;; user and syntax canonical form:
;;; (DURATION {<lisp-form>|<duration>})

(define-primitive-requirement
  (duration ?
            :optional))

(defmethod define-pdl-feature ((proc primitive-procedure)
                               (attr (eql 'duration))
                               duration)
  (cond ((time-expression? (car duration) t)
         (u-setf (duration proc) (car duration)))
        ((quoted-time-expression? (car duration) t)
         (u-setf (duration proc) (cadar duration)))
        (t (u-setf (duration proc) (cons 'progn duration)))))

;;;---------------------------------------------------------------------
;;; Primitive clause: RETURN
;;;---------------------------------------------------------------------
;;; user and syntax canonical form:
;;; (RETURN <lisp-form>)

(define-primitive-requirement
  (return ? :optional))

(defmethod define-pdl-feature ((proc primitive-procedure)
                               (attr (eql 'return))
                               return)
  (u-setf (returnval proc) (car return)))
