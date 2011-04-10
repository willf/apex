;;;-*- Mode: Lisp; Package: :apex.utility.patmatch -*-
;;;
;;; apex/system/utility/patmatch.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: patmatch.lisp,v 1.21 2006/01/15 03:43:03 dalal Exp $


(defpackage :apex.utility.patmatch
  (:use :common-lisp)
  (:export
   #:?
   #:fail
   #:no-bindings
   #:*current-bindings*
   #:*variable-types*
   #:pat-match
   #:variable-p
   #:anonymous-variable-p   
   #:variable-type
   #:convert-variable
   #:get-binding
   #:binding-var
   #:binding-val
   #:make-binding
   #:lookup
   #:value-in
   #:extend-bindings
   #:match-variable
   #:?is #:?or #:?and #:?not #:?* #:?+ #:?+ #:?? #:?if
   #:remove-binding
   #:unbind
   #:make-binding-set
   #:copy-bindings
   #:bindings-for-each
   #:binding-set-union
   #:binding-set-intersection
   #:substitute-bindings
   #:generate-variable-name
   #:variables-in
   #:variable-bound-p
   #:with-bindings
   #:variable-name
   #:variable-prefix
   ;;; stuff to allow variable definition outside this file
   #:add-variable-type
   #:convert-variable
   #:is-variable-p
   #:variable-name-of
   #:variable-type-of
   #:get-binding-dispatch
   #:match-variable-dispatch
   #:make-variable
   #:patmatch-eval
   #:patmatch-constraint   
   #:make-bindings-stack
   #:push-bindings
   #:pop-bindings
   #:value-in-stack
   #:substitute-bindings-stack
   #:binding-set-p
   #:bindings-stack-p
   #:pat-match-special-form-p
   )
  )

(in-package :apex.utility.patmatch)

(defmacro ignore-warnings (&body body)
  (let ((warnv (gensym))
	(valv (gensym))
	(cv (gensym)))
    `(let* ((,warnv (list))
	   (,valv (handler-bind ((warning 
				  #'(lambda (,cv)
				      (push ,cv ,warnv)
				      (muffle-warning)
					    )))
		    ,@body)))
       (if ,warnv (values ,valv (nreverse ,warnv)) ,valv))))

(defun variable-p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (not (symbolp x))
    nil
    (let* ((sname (symbol-name x))
	   (len (length sname)))
      (if (= len 0) nil
	  (let ((ch1 (char sname 0)))
	    (if (char= ch1 #\?)
	      t
	      (if (<= len 2) nil
		  (let ((ch2 (char sname (1- len))))
		    (or (and (char= #\< ch1)
			     (char= #\> ch2))
			(char= #\+ ch1 ch2))))))))))

(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")

(defvar *current-bindings* no-bindings
  "Dynamically scoped binding set")

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File pat-match.lisp: Pattern matcher from section 6.2

;;; Two bug fixes By Richard Fateman, rjf@cs.berkeley.edu  October 92.

;;; ! Refactor the following two defs!

;;; if we get any warnings on 'rebound' variables, we'll save them till the
;;; end before warning, and only warning if the overall match succeeds.

(defun pat-match (pattern input &optional (bindings no-bindings))
  (multiple-value-bind (bs warnings)
      (ignore-warnings 
       (pat-match* pattern input bindings))
    (if (eq bs fail)
      fail
      (progn
	(when warnings (dolist (warning warnings) (warn warning)))
	bs))))

#+:MCL
(defun pat-match* (pattern input bindings)
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	((anonymous-variable-p pattern) bindings)	
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
	((and (stringp pattern) 
	      (stringp input)
	      (string-equal pattern input))
	 bindings)
        ((segment-pattern-p pattern)                
         (segment-matcher pattern input (reverse (cdr (reverse bindings)))))  ;; this is the line that's different
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match* (rest pattern) (rest input)
                    (pat-match* (first pattern) (first input) 
                               bindings)))
        (t fail)))



#-:MCL
(defun pat-match* (pattern input bindings)
  "Match pattern against input in the context of the bindings"
  (declare (inline anonymous-variable-p variable-p))
  (cond ((eq bindings fail) fail)
	((anonymous-variable-p pattern) bindings)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
	((and (stringp pattern) 
	      (stringp input)
	      (string-equal pattern input))
	 bindings)
        ((segment-pattern-p pattern)                
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match* (rest pattern) (rest input)
                    (pat-match* (first pattern) (first input) 
                               bindings)))
        (t fail)))

;;;
;;; To support different types of bindings is fairly easy to do. Variables in binding
;;; sets should always have the x form. 
;;; 
;;; To create a new variable type, the programmer has to take the following steps:
;;; -. decide what it's 'type' is (a keyword, probably)
;;; -. decide what it's first letter is, used to determine a lot of things 
;;; -. add the type, call it :FOO, using ADD-VARIABLE-TYPE <type>
;;; -. add a specialized method CONVERT-VARIABLE (from-var (to-type (eql <type>)))
;;; -. add a specialized method IS-VARIABLE-P ((symbol-name string) (len integer) (type (eql <type>)))
;;; -. add a specialized method VARIABLE-NAME-OF (name (char (eql <ch>)))
;;; -. add a specialized method VARIABLE-TYPE-OF ((char (eql <ch>)))
;;; -. add a specialized method GET-BINDING-DISPATCH ((type (eql <type>)) var bindings)
;;;    which should return a *binding pair* (not a binding value) based on the type, var & bindings
;;; -. add a specialized method MATCH-VARIABLE-DISPATCH ((type (eql <type>)) var input bindings)
;;;    which should return a possibly new binding set based on type, var, input and bindings.
;;; -. Make sure you've added the same symbol in all the different places ...
;;;
;;; It's very simple, really ...

(defvar *variable-types* '(:free :bound))

(defun add-variable-type (type)
  (unless (member type *variable-types* :test #'eq)
    (setf *variable-types*
    (append *variable-types* (list type)))))

(defmethod convert-variable (from-var (to-type (eql :free)))
  (intern (concatenate 'string "?" (string-upcase (symbol-name (variable-name from-var))))))

(defmethod convert-variable (from-var (to-type (eql :bound)))
  (intern (concatenate 'string "<?" (string-upcase (symbol-name (variable-name from-var)))
		       ">")))

(defmethod convert-variable (from-var (other T))
  (error "Unknown variable type ~S; can't convert ~S" other from-var))

(defmethod is-variable-p ((symbol-name string) (len integer) (type (eql :free)))
  (char= (char symbol-name  0) #\?))

(defmethod is-variable-p ((symbol-name string) (len integer) (type (eql :bound)))
  ;;; "<?A>"
  (and 
   (>= len 4)
   (char= (char symbol-name  0) #\<)
   (char= (char symbol-name  1) #\?)
   (char= (char symbol-name  (1- len)) #\>)))
  


(defmethod is-variable-p ((symbol-name string) (len integer) (other T))
  (error "Unknown variable type ~S; can't check whether ~a is a variable of this type." other symbol-name))
  
;;;(defun variable-p (x)
;;;  "Is x a variable?"
;;;  (declare (optimize (speed 3) (safety 0)))  ;;a most speed critical fn in file
;;;    (and (symbolp x)
;;;	 (let* ((sname (symbol-name x))
;;;		(len (length sname)))
;;;	   (some #'(lambda (type)
;;;		     (is-variable-p sname len type))
;;;		 *variable-types*))))





(defun anonymous-variable-p (x)
  "Is this equal to ?"
  (declare (optimize (speed 3) (safety 0)))
  (eq x '?))

(defmethod variable-name-of (name (char (eql #\?)))
  (intern (nstring-upcase (subseq name 1))))

(defmethod variable-name-of (name (char (eql #\<)))
  (intern (nstring-upcase (subseq name 2 (1- (length name))))))

(defmethod variable-name-of (name (char (eql #\+)))
  (intern (nstring-upcase (subseq name 1 (1- (length name))))))

(defmethod variable-name-of (name char)
  (error "Unknown first character ~S; can't decide variable name for ~A" char name))

(defun variable-name (variable)
  "Get the variable name from a variable."
  (declare (optimize (speed 3) (safety 0)))  ;; a most speed critical fn in file
  (assert (variable-p variable))  
  (let ((name (symbol-name variable)))
  (variable-name-of name (char name 0))))

(defmethod variable-type-of ((char (eql #\?)))
  :free)

(defmethod variable-type-of ((char (eql #\<)))
  :bound)

(defmethod variable-type-of (char)
  (error "Unknown first character ~S; can't decide variable type." char))

(defun variable-type (variable)
  "returns variable type."
  (declare (optimize (speed 3) (safety 0)))  ;; a most speed critical fn in file  
  (assert (variable-p variable))
  (variable-type-of (char (symbol-name variable) 0)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (if (variable-p var)
      (get-binding-dispatch (variable-type var) var bindings)
    (assoc var bindings)))

(defmethod get-binding-dispatch ((type (eql :free)) var bindings)
  (assoc (variable-name var) bindings))

(defmethod get-binding-dispatch ((type (eql :bound)) var bindings)
  (assoc (variable-name var) bindings))


;; handles the case in which the var is from another binding set
(defmethod get-binding-dispatch ((type T) var bindings)
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun (setf binding-var) (var binding)
  (setf (car binding) var))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun (setf binding-val) (val binding)
  (setf (cdr binding) val))

;;; make sure the varible name is used only.
(defun make-binding (var val)
  (if (variable-p var)
      (cons (variable-name var) val)
    (cons var val)))

(defun make-binding-set ()
  no-bindings)

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun value-in (variable binding-set)
  "return the value of a variable in a binding list -- recursive lookup of variables.."
  (labels ((value-in* (var seen-variables)
             (let ((val (binding-val (get-binding var binding-set))))
               (cond
                ((and (variable-p val) (member val seen-variables :test #'eql))
                 nil)
                ((variable-p val) (value-in* val (cons val seen-variables)))
                (t val)))))
    (value-in* variable '())))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (if (variable-p var) 
	    (make-binding (variable-name var) val)
	  (make-binding var val))
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

;;;(defun match-variable (var input bindings)
;;;  "Does VAR match input?  Uses (or updates) and returns bindings."
;;;  (let ((binding (get-binding var bindings)))
;;;    (cond ((not binding) (extend-bindings var input bindings))
;;;          ((equal input (binding-val binding)) bindings)
;;;          (t fail))))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (match-variable-dispatch (variable-type var) var input bindings))

(defmethod match-variable-dispatch ((type (eql :free)) var input bindings)
  (let ((binding (get-binding var bindings)))
    (if binding
	(if (equal (binding-val binding) input) bindings fail)
      (extend-bindings var input bindings))))

(defmethod match-variable-dispatch ((type (eql :bound)) var input bindings)
  (let ((binding (get-binding var bindings)))
    (if binding
	(if (equal (binding-val binding) input) bindings fail)
      (progn
	(warn "Binding variable ~S to ~S although it should already be bound." var input)
	(extend-bindings (variable-name var) input bindings)))))

(defmethod match-variable-dispatch ((type T) var input bindings)
  (declare (ignore input bindings))
  (error "No variable matching method for ~S" var))


(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern)) 
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (declare (inline single-match-fn)
	   (optimize (speed 3) (safety 0)))
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (declare (optimize (speed 3) (safety 0)))
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns) 
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (progv (mapcar #'car bindings)
              (mapcar #'cdr bindings)
          (eval (substitute-bindings (second (first pattern)) bindings)))
       (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev) 
    (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator 
       (input rules &key (matcher #'pat-match) 
        (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some 
    #'(lambda (rule)
        (let ((result (funcall matcher (funcall rule-if rule) 
                               input)))
          (if (not (eq result fail))
              (funcall action result (funcall rule-then rule)))))
    rules))


;;; -- Not from Norvig. From Will Fitzgerald's class code.
;;;
(defun remove-binding (binding binding-set)
  "remove a binding pair from a binding set"
  (let ((newbindings (remove binding binding-set :test #'equal)))
    (if (null newbindings)
      no-bindings
      newbindings)))

(defun unbind (var binding-set)
  (let ((binding (get-binding var binding-set)))
    (if binding 
      (remove-binding (get-binding var binding-set)
		      binding-set)
      binding-set)))
  

(defun copy-bindings (binding-set)
  "copies a binding set, copying the bindings"
  (let ((new-set (make-binding-set)))
    (bindings-for-each #'(lambda (binding)
                      (setf new-set
                            (extend-bindings
                             (binding-var binding)
                             (binding-val binding)
                             new-set)))
                  binding-set)
    new-set))

(defun bindings-for-each (function binding-set)
  (when (not (eq binding-set no-bindings))
    (dolist (binding binding-set)
      (funcall function binding))))


(defun binding-set-union (&rest binding-sets)
  "union of binding sets; non-destructive"
  (flet ((binding-set-union1 (bs1 &optional bs2)
	   (if (null bs2)
	       bs1
	       (bindings-for-each #'(lambda (binding)
			         (let* ((var (binding-var binding))
				        (val (binding-val binding))
				        (b2 (get-binding var bs1)))
                                   (cond
				    ((and b2 (not (eql val (binding-val b2))))
				     (warn "Value for ~S found in two binding sets,  using ~S"
					   var (binding-val b2)))
                                    (b2) ; i.e., they're equal; no warning
                                    (t (setf bs1 (extend-bindings var val bs1))))))
			   bs2))
	   bs1))
    (if (null binding-sets)
	(make-binding-set)
      (reduce #'binding-set-union1 
	      (cons (copy-bindings (car binding-sets))
		    (cdr binding-sets))))))

(defun binding-set-intersection (&rest binding-sets)
  "insection of binding sets; non-destructive"
  (flet ((binding-set-inter1 (bs1 &optional bs2)
	   (unless (null bs2)
	     (bindings-for-each #'(lambda (binding)
			       (let* ((var (binding-var binding))
				      (val (binding-val binding))
				      (b2 (get-binding var bs2)))
                                 (unless (and b2 (eql (binding-val b2) val))
                                   (setf bs1 (remove-binding binding bs1))
				   )))
			   bs1))
	   bs1))
    (if (null binding-sets)
	(make-binding-set)
      (reduce #'binding-set-inter1 
	      (cons (copy-bindings (car binding-sets))
		    (cdr binding-sets))))))

(defun substitute-bindings (form binding-set)
  (cond
   ((variable-p form)
    (let ((value (value-in form binding-set)))
      (or value form)))
   ((atom form) form)
   (t 
    (cons 
     (substitute-bindings (car form) binding-set)
     (substitute-bindings (cdr form) binding-set) ))))




(defun variables-in (form)
  "Returns a list of all the variables in FORM"
  (assert (or (atom form) (consp form)))
  (let ((variables (list)))
    (labels ((variables-in* (iform)
	       (cond
		((variable-p iform)
		 (pushnew iform variables))
		((atom iform)) ;; do naught
		(t
		 (variables-in* (car iform))
		 (variables-in* (cdr iform))))))
      (variables-in* form)
      (nreverse variables))))


(defun variable-bound-p (variable binding-set)
  (if (get-binding variable binding-set) T 
    NIL))

(defun variable-name-from-symbol (name)
  (if (variable-p name) name
    (intern (format nil "?~A" (symbol-name name)))))

(defun var-in-current-bindings-reader (stream subchar char)
  (declare (ignore subchar char)
	   (inline variable-name-from-symbol))
  `(value-in (variable-name-from-symbol (quote ,(read stream t nil t))) *current-bindings*))

(defmacro with-bindings ((bindings) &body body)
  `(let ((*current-bindings* ,bindings))
     (declare (special *current-bindings*))
     ,@body))

(eval-when (:compile-top-level :load-top-level :execute)
  (set-dispatch-macro-character #\# #\? #'apex.utility.patmatch::var-in-current-bindings-reader))

(defun generate-variable-name  (&optional (prefix "VAR-"))
  (gentemp (format nil "?~A" prefix)))

(defun make-variable (sym)
  (intern (concatenate 'string "?" (symbol-name sym))))



(defun peval* (form)
  "Like Lisp's eval, only ..."
  (flet ((eval-cdr (l)
	   (mapcar #'peval* l)))
    (cond
     ((or (atom form)
	  (pat-match-special-form-p form))
      form)
     ((fboundp (car form))
      (apply (car form)
	     (eval-cdr (cdr form))))
     (t (error "Undefined function ~a in ~a." (car form) form)))))

(defun patmatch-eval (form &optional (binding-set no-bindings))
  (peval* (substitute-bindings form binding-set)))


;; (patmatch-constraint '?a '(< (+ 3 4)) ((a . 4)(b . 5)) ==
;; (patmatch-eval '(< ?a (+ 3 4)) ((a . 4)(b . 5))) == T
(defun patmatch-constraint (variable form &optional binding-set)
  (assert (consp form) nil
    "PATMATCH-CONSTRAINT called with non-list: ~s." form)
  (patmatch-eval `(,(car form) ,variable ,@(cdr form)) binding-set))


(defun make-bindings-stack (&rest binding-sets)
  binding-sets)

(defmacro push-bindings (bindings stack)
  `(push ,bindings ,stack))

(defmacro pop-bindings (stack)
  `(pop ,stack))

(defun substitute-bindings-stack (form stack)
  (dolist (bindings stack)
    (setf form (substitute-bindings form bindings)))
  form)

(defun value-in-stack (variable stack)
  (loop for bindings in stack
	  when (value-in variable bindings)
	  return it))


;;;

(defmethod  binding-set-p ((object t)) nil)

(defmethod binding-set-p ((set list))
  (or (eq set no-bindings)
      (every (lambda (x)
	       (and (consp x)
		    (car x) 
		    (atom (cdr x))))
	     set)))

(defmethod bindings-stack-p ((object t)) nil)

(defmethod bindings-stack-p ((object list))
  (every 'binding-set-p object))

;;

(defun pat-match-special-form-p (x)
  (and 
   (or (segment-pattern-p x)
       (single-pattern-p x))
   t))


