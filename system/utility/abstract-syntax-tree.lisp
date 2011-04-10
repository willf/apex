;;;-*- Mode: Lisp; Package:  :apex.utility.ast -*-
;;;
;;; apex/system/utility/abstract-syntax-tree.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: abstract-syntax-tree.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $
;;; Created:        January, 2005

;;;
;;; Abstract syntax trees.
;;;

(defpackage :apex.utility.ast
  (:use :common-lisp)
  (:use :apex.utility.patmatch)
  (:export
   #:ast-convert
   #:ast-feature-forms
   #:ast-feature-requirements
   #:ast-object-form
   #:canonical-ast-p
   #:check-ast-features
   #:ast-feature-attribute
   #:ast-feature-requirement-attribute
   #:ast-feature-requirement-count
   #:ast-feature-requirement-pattern
   #:ast-feature-value
   #:find-ast-feature-requirements
   #:unique-ast-attributes
   #:values-meet-count-requirements-p
   #:values-meet-requirement-pattern-p
   ;; these are modifcation macros.
   #:consf
   #:enqueuef
   #:u-setf
   #:appendf
   )
  )

(in-package :apex.utility.ast)

;;; The basic model for our 'abstract syntax tree' will be object +
;;; features*. The AST is the 'canonical syntactic form' of PDL. For
;;; example, a procedure AST is (PROCEDURE features*) where
;;; PROCEDURE is the object, and attributes are INDEX, STEP, etc. Thus,
;;; syntactically, we go from, eg.
;;;
;;; (procedure (do-something)
;;;  (step s1 (do-a))
;;;  (step (do-b))
;;;
;;; to a canonical form:
;;;
;;; (procedure (index (do-something))
;;;            (step (tag s1) (activity (do-a)))
;;;            (step (activity (do-b)))))
;;; etc.
;;;


;;; -- all the AST code here ...


;;; Simple accessors: note this is the same as a 'ast feature'
(defun ast-object-form (ast)
  (car ast))

(defun ast-feature-forms (ast)
  (cdr ast))

;; a 'feature' is an attribute/value cons

(defun ast-feature-attribute (feature)
  (car feature))

(defun ast-feature-value (feature)
  (cdr feature))

;;; Code is in canonical AST form iff
;;; - it's a list,
;;; - headed by a symbol
;;; - and each item in the CDR of the form
;;;   is either a (attribute value) pair,
;;;   or a canonical AST form.
;;;
(defun canonical-ast-p (code)
  (and (consp code)
       (symbolp (car code))
       (every (lambda (feature)
		(and 
		 (consp feature)
		 (symbolp (car feature))))
	      (cdr code))))

;;; ASTs also allow the setting of requirements for feature values, where 
;;; is required, one of a list, etc.
;;;
;;; A requirement is a list:
;;;
;;; (<attribute> <pattern> <count>) that a list of feature values
;;; must meet.
;;; the attribute is the key for the list.
;;; <pattern> is a PAT-MATCH pattern for checking syntactic validity
;;; <count> is one of:
;;; :unique : exactly one must be defined
;;; :optional : 0 or 1 can be defined
;;; :kleene-plus: 1 or more must be defined
;;; :kleene-star: 0 or more can be defined
;;;
;;;  define a requirements list with:
;;;
;;;  (defmethod ast-feature-requirements ((object-type (eql <name>)))
;;;    (<requirement>*))
;;; 
;;; for example:
;;; (defmethod ast-feature-requirements ((type (eql 'procedure)))
;;;  '((index   (index (?is ?name listp)) :unique)
;;;    (step    (step . ?)                :kleene-plus)
;;;    (logging (logging . ?)             :kleene-star)
;;;    ))
;;;
;;; This can be 'macroized' as appropriate.
;;;

(defun ast-feature-requirement-attribute (requirement)
  (car requirement))

(defun ast-feature-requirement-pattern (requirement)
  (cadr requirement))

(defun ast-feature-requirement-count (requirement)
  (third requirement))


(defmethod ast-feature-requirements (form)
  (error "No feature requirements defined for ~a" form))

(defun find-ast-feature-requirements (attribute requirements)
  (assoc attribute requirements))

(defun unique-ast-attributes (features)
  (remove-duplicates (mapcar #'ast-feature-attribute features)))

(defun ast-attribute-values (attribute features)
  (loop for feature in features
      when (eql attribute (ast-feature-attribute feature))
      collect (ast-feature-value feature)))

(defun values-meet-requirement-pattern-p (values requirement)
  (every #'(lambda (value)
	     (if (pat-match (ast-feature-requirement-pattern requirement)
			    value)
	       t
	       (error "Invalid ~a value ~a; does not match ~a"
		      (ast-feature-requirement-attribute requirement)
		      value
		      (ast-feature-requirement-pattern requirement))))
	 values))

(defun values-meet-count-requirements-p (values requirement)
  (ecase (ast-feature-requirement-count requirement)
    (:unique (if (= (length values) 1) t
		 (error "More than one value for ~a given: ~a"
			(ast-feature-requirement-attribute requirement)
			values)))
    (:kleene-plus (if (>= (length values) 1) t
		      (error "No values for ~a given"
			     (ast-feature-requirement-attribute requirement))))
    (:optional (if (<= (length values) 1) t
		   (error "More than one value for ~a given: ~a"
			  (ast-feature-requirement-attribute requirement)
			  values)))
    (:kleene-star t)))


(defun ast-attribute-values-ok (values requirement)
  (and (values-meet-requirement-pattern-p values requirement)
       (values-meet-count-requirements-p values requirement)))
  
(defun check-ast-features (features-to-check requirements)
  (every #'(lambda (attribute)
	     (let ((requirements (find-ast-feature-requirements attribute requirements)))
	       (unless requirements
		 (error "~a is not a defined attribute." attribute))
	       (ast-attribute-values-ok
		(ast-attribute-values attribute features-to-check)
		requirements)))
	 (unique-ast-attributes features-to-check)))


;;;
;;; this is the main method ...
;;; 
;;; (a) check that it is in canonical AST format
;;; (b) check that the features meet the syntactic requirements
;;; (c) initialize the object with initializer
;;; (d) for each feature form:
;;; (e)   call the 'featurer' function
;;; (f) call any postprocessing
;;; (g) return the object.
;;; 

(defun ast-convert (canonical-ast initializer featurer &optional postprocessor)
  (assert (canonical-ast-p canonical-ast)
      nil
    "~s is not in canonical AST format." canonical-ast)
  (check-ast-features (ast-feature-forms canonical-ast)
		      (ast-feature-requirements (ast-object-form canonical-ast)))
  (let ((object (funcall initializer (ast-object-form canonical-ast))))
    (loop for form in (ast-feature-forms canonical-ast) doing
	  (let ((attribute (car form))
		(value (cdr form)))
	    (funcall featurer object attribute value)))
    (when postprocessor (funcall postprocessor object))
    object
    ))

;;;
;;; Some new kinds of errors ...
;;;

;;;(defclass invalid-ast-object-form (error) ())
;;;(defclass invalid-ast-feature-form (error) ())
;;;(defclass invalid-ast-attribute-value-form (error) ())

;;; (defclass unique-ast-feature-error (error) ())
;;;(defclass missing-ast-feature-error (error) ())

;;;
;;; Some useful macros to set values. This is a bit redundant
;;; on the syntactic checks earlier.
;;;
;;; These are:
;;;
;;; U-SETF   - 'unique' setf: ensure there is only one.
;;; ENQUEUEF - adds item to end of list 
;;; APPENDF  - appends at end of a list
;;; CONSF    - adds item to beginning of list (like 'push')
;;;
;;; The last three can be used even if the place is not yet
;;; bound for an object.
;;;
;;; Example uses:
;;;
;;; (enqueuef (steps procedure) step) --> adds step to end (even if
;;;   steps is not yet bound).
;;;
;;; (u-setf (index procedure) '(do-a)) --> ensures there isn't already
;;; an index
;;;


(defmacro set-unique-value (place value)
  (let ((cval (gensym)))
    `(let ((,cval (ignore-errors ,place)))
       (when ,cval (error "already has a value: ~a"  ,cval))
       (setf ,place ,value))))

(define-modify-macro u-setf (value) set-unique-value)

(defmacro cons-value-at-end (place value)
  (let ((cval (gensym)))
    `(let ((,cval (ignore-errors ,place)))
       (when (not (listp ,cval))
	 (error "~a is not a list: ~a" ',place ,cval))
       (setf ,place (nconc ,cval (list ,value))))))

(define-modify-macro enqueuef (value) cons-value-at-end)

(defmacro append-at-end (place value)
  (let ((cval (gensym)))
    `(let ((,cval (ignore-errors ,place)))
       (when (not (listp ,cval))
	 (error "~a is not a list: ~a" ',place ,cval))
       (setf ,place (append ,cval ,value)))))

(define-modify-macro appendf (value) append-at-end)

(defmacro cons-at-beginning (place value)
  (let ((cval (gensym)))
    `(let ((,cval (ignore-errors ,place)))
       (when (not (listp ,cval))
	 (error "~a is not a list: ~a" ',place ,cval))
       (setf ,place (cons ,value ,cval )))))

(define-modify-macro consf (value) cons-at-beginning)

