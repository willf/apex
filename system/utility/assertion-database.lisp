;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/assertion-database.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: assertion-database.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $

;;; An assertion database.

;;; This is an assertion database based on the implementation in Chapter
;;; 4 of Abelson and Sussman's "Structure and Interpretation of Computer
;;; Programs" (first edition).

;; ! The following code is candidate for its own package, since most of
;; it is internal (not exported), and had some name clashes with
;; Norvig's pattern matcher.

(in-package :user)

;;; -------------- Definitions needed up front -------------


;;; Placeholders reset by initialize-data-base

(defvar qget nil)      ; used in assert, query
(defvar qput nil)      ; assert
(defvar qshow nil)     ; display
(defvar qclear nil)    ; reset
(defvar qcontents nil) ; query
(defvar opget nil)     ; query

;;; -------------- End of Definitions needed up front ---------

;;; Convenience wrappers to the basic database

(defun assert* (assertion)
  (assert! (process-query-syntax assertion)))

(defun query* (assertion &rest frame-stream)
  (qeval (process-query-syntax assertion)
         (if (null frame-stream)
             (singleton-stream *the-empty-stream*)
             (car frame-stream))))

(defun qeval (query frame-stream)
  (let ((qproc (funcall opget (qtype query))))
    (if qproc
        ;;
        ;; procedure for compound queries (e.g. for AND, OR, NOT)
        ;;
        (funcall qproc (query-contents query) frame-stream)
      (simple-query query frame-stream))))

;;; Simple queries

(defun simple-query (query-pattern frame-stream)
  (stream-flatmap
   #'(lambda (frame)
      (find-assertions query-pattern frame))
   frame-stream))

;;; Finding Assertions by Pattern Matching

(defun find-assertions (pattern frame)
  (stream-flatmap
   #'(lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(defun check-an-assertion (assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq match-result 'failed)
        *the-empty-stream*
        (singleton-stream match-result))))

(defun pattern-match (pat dat frame)
  (cond ((eq frame 'failed) 'failed)
        ((equal pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (consp pat) (consp dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (t 'failed)))

(defun extend-if-consistent (var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
      (extend-frame var dat frame))))


;;; Compound queries

(defun conjoin (conjuncts frame-stream) ; "and"
  (if (null conjuncts)
      frame-stream
    (conjoin (cdr conjuncts)
             (qeval (car conjuncts)
                    frame-stream))))

(defun disjoin (disjuncts frame-stream) ; "or"
  (if (null disjuncts)
      *the-empty-stream*
    (interleave-delayed
     (qeval (first disjuncts) frame-stream)
     (delay (disjoin (rest disjuncts)
                     frame-stream)))))

;;; Supporting functions for query

(defun unbound-pattern-var-handler (v f)
  (declare (ignore f))
  (contract-question-mark v))

(defun instantiate (exp frame unbound-var-handler)
  (labels ((copy (exp)
             (cond ((var? exp)
                    (let ((binding (binding-in-frame exp frame)))
                      (if binding
                          (copy (binding-value binding))
                        (funcall unbound-var-handler exp frame))))
                   ((consp exp)
                    (cons (copy (car exp)) (copy (cdr exp))))
                   (t exp))))
    (copy exp)))


;;; Filters

(defun lisp-value (call frame-stream)
  (stream-flatmap
   #'(lambda (frame)
       (if (execute
            (instantiate
             call
             frame
             #'(lambda (v f)
                 (declare (ignore f))
                 (error "Unknown pattern variable ~a~%" v))))
           (singleton-stream frame)
         *the-empty-stream*))
   frame-stream))

(defun execute (exp)
  (labels ((predicate (x)  (car x))
           (args (x)  (cdr x)))
    (apply (eval (predicate exp)) (args exp))))

(defun negate (operands frame-stream)
  (labels ((negated-query (x) (car x)))
    (stream-flatmap
     #'(lambda (frame)
       (if (stream-empty? (qeval (negated-query operands)
                                 (singleton-stream frame)))
           (singleton-stream frame)
         *the-empty-stream*))
     frame-stream)))


#|| probably not needed

;;; Converts strings that look like numbers into numbers; leaves
;;; everything else alone.
;
(defun numberify-if-possible (x)              ; Value -> Value
  (cond ((and (stringp x) (string->number x)))
        (t x)))


;;; Comparison Predicates

(defun numeric-compare (op opname x y)
  (let ((xx (numberify-if-possible x))
        (yy (numberify-if-possible y)))
    (if (and (numberp xx) (numberp yy))
        (op xx yy)
        (error "Arguments must be numbers: ~s ~s~%" xx yy))))


(defun eq (x y)
  (let ((xx (numberify-if-possible x))
        (yy (numberify-if-possible y)))
    (cond ((and (numberp xx) (numberp yy))
           (= xx yy))
          ((and (stringp xx) (stringp yy))
           (equal xx yy))
          ((and (symbolp xx) (symbolp yy))
           (eq xx yy))
          (t (error "Arguments must be of same type: ~s ~s~%" xx yy)))))

(defun lt (x y) (numeric-compare < 'lt x y))
(defun gt (x y) (numeric-compare > 'gt x y))
(defun leq (x y) (numeric-compare <= 'leq x y))
(defun geq (x y) (numeric-compare >= 'geq x y))

||#

;;; Assertion and Fluency  (might not need fluency)

;;; Non-fluent assertion (a special case of fluent assertion)
;
(defun assert! (assertion)
  (assert-fluent! assertion 0))

;;; Fluent assertion
;;; fluent-tail-size is the tail length of the assertion to treat as a
;;; value.  For example, if the assertion is (a b c d e) and
;;; fluent-tail-size is 3, then (c d e) will be considered the "value"
;;; of the assertion (i.e. the portion that is updated when a new
;;; assertion of the form (a b X Y Z) enters.
;
(defun assert-fluent! (assertion fluent-tail-size)
  (store-assertion assertion fluent-tail-size))

;;; Store assertion in indexed table
;
(defun store-assertion (assertion fluent-tail-size)
  (let* ((key (index-key-of assertion))
         (current-assertion-stream (get-stream qget key)))
    (funcall qput key
          (stream-cons-fluently assertion current-assertion-stream
                                fluent-tail-size))))

(defun stream-cons-fluently (ass str n)
  (labels ((iter (old new)
             (cond ((stream-empty? old) (stream-cons ass new))
                   ((fluently-equal? n ass (stream-car old))
                    (stream-append (make-stream ass) (stream-cdr old) new))
                   (t (iter (stream-cdr old)
                            (stream-cons (stream-car old) new))))))
    (iter str *the-empty-stream*)))

;;; Test equality of two lists, ignoring last n elements (i.e. the "value")
;
(defun fluently-equal? (n e1 e2)
  (let* ((len1 (length e1))
         (index (- len1 n)))            ; add check for negative
    (and (= len1 (length e2))
         (equal (leftmost index e1) (leftmost index e2)))))


;;; Maintaining the Data Base

;;; Obtain relevant assertions for a given pattern
;
(defun fetch-assertions (pattern frame)
  (declare (ignore frame))
  ;;
  ;; Note: frame could be utilized for more efficient search
  ;;
  (if (indexable? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(defun indexable? (pattern)
  (symbolp (car pattern)))

(defun get-all-assertions ()
  (funcall qcontents))

(defun get-indexed-assertions (pattern)
  (get-stream qget (index-key-of pattern)))

(defun get-stream (accessor key)
  (or (funcall accessor key) *the-empty-stream*))

;;; Return leftmost n elements of a list.
;;; Nat * List --> List
;
(defun leftmost (n e)
  (if (or (= n 0) (null e))
      '()
      (cons (car e) (leftmost (1- n) (cdr e)))))

(defun index-key-of (pattern-or-assertion)
  (car pattern-or-assertion))

;;; Query syntax procedures

(defun qtype (exp)
  (if (consp exp)
      (car exp)
    (error "Unknown expression type ~a~%" exp)))

(defun query-contents (exp)
  (if (consp exp)
      (cdr exp)
    (error "Unknown expression type ~a~%" exp)))

(defun process-query-syntax (exp)
   (map-over-symbols #'expand-question-mark exp))

(defun expand-question-mark (symbol)
  (let ((chars (format nil "~a" symbol)))
    (if (string= (subseq chars 0 1) "?")
        (list '?
              (intern
               (subseq chars 1 (length chars))))
        symbol)))

(defun var? (exp) (tagged-list? exp '?))

(defun contract-question-mark (variable)
  (intern
   (format nil "?~a"
     (if (numberp (cadr variable))
         (format nil "~a-~a"
                 (format nil "~a" (caddr variable))
                 (format nil "~a" (cadr variable)))
       (format nil "~a" (cadr variable))))))

(defun process-pattern-variables (frame-stream)
  (stream-map
   #'(lambda (frame)
     (mapcar
      ;;
      ;; ((? var) . val) => (?var . val)
      ;;
      #'(lambda (b)  
        (make-binding (contract-question-mark (binding-variable b))
                      (binding-value b)))
      frame))   
   frame-stream))

(defun tagged-list? (exp tag)
  (and (consp exp)
       (eq (car exp) tag)))

;;; Frames and bindings

;;; in patmatch.lisp
;;; (defun make-binding (variable value) (cons variable value))

(defun binding-variable (b) (car b))
(defun binding-value (b) (cdr b))

(defun binding-in-frame (variable frame)
  (assoc variable frame :test #'equal))

(defun extend-frame (variable value frame)
  (cons (make-binding variable value) frame))

(defun frame-empty? (frame) (null frame))
(defun first-binding (frame) (car frame))
(defun rest-bindings (frame) (cdr frame))

;;; Stream operations

(defun stream-flatmap (proc s)
  (flatten-stream (stream-map proc s)))

(defun flatten-stream (stream)
  (if (stream-empty? stream)
      *the-empty-stream*
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(defun interleave-delayed (s1 delayed-s2)
  (if (stream-empty? s1)
      (force delayed-s2)
      (stream-cons
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

(defun singleton-stream (x)
  (stream-cons x *the-empty-stream*))


;;; Stream support

(defun stream-map (proc s)
  (if (stream-empty? s)
      *the-empty-stream*
      (stream-cons (funcall proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(defun stream-for-each (proc s)
  (when (not (stream-empty? s))
    (funcall proc (stream-car s))
    (stream-for-each proc (stream-cdr s))))

(defun display-stream (s)
  (stream-for-each #'display-line s))

(defun display-line (x)
  (format t "~a~%" x))

(defun stream-filter (pred stream)
  (cond ((stream-empty? stream) *the-empty-stream*)
        ((funcall pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (t (stream-filter pred (stream-cdr stream)))))

(defun interleave (s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;;; Table support

;;; Create a one dimensional table with specified association predicate
;;; and display format string
;;;   (A * A -> bool) * string --> Table
;;;   Table ::= symbol --> (A -> B)  (a message passing object)
;
(defun make-assoc-table (association-pred display-format
                   &optional (modifier #'identity))
  (let ((local-table (list '*table*)))
    (labels
        ((lookup (key)
           (let ((record (funcall association-pred key (cdr local-table))))
             (if record
                 (cdr record)
               nil)))
         (insert (key value)
           (let ((record (funcall association-pred key (cdr local-table))))
             (if record
                 (rplacd record value)
               (rplacd local-table
                         (cons (cons key value)
                               (cdr local-table))))))
         (display ()
           (mapc
               #'(lambda (row)            ; row is an a-list entry pair
                 (mapc
                     #'(lambda (entry)    ; entry is a key value
                         (format t display-format (funcall modifier entry)))
                   (cdr row)))
             (cdr local-table)))
         (clear () (setq local-table (list '*table*)))
         (contents () (flatmap #'cdr (cdr local-table))))
      #'(lambda (m)
        (cond ((eq m 'lookup-proc) #'lookup)
              ((eq m 'insert-proc) #'insert)
              ((eq m 'display-proc) #'display)
              ((eq m 'clear-proc) #'clear)
              ((eq m 'contents-proc) #'contents)
              (t (error "Unknown operation ~a~%" m)))))))


(defun make-memory-table ()
  (make-assoc-table #'assoc "~s~%"))

;;; General functions (candidates for Scheme library)

(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
               (accumulate op initial (cdr sequence)))))

(defun accumulate-stream (op initial sequence)
  (if (stream-empty? sequence)
      initial
      (funcall op (stream-car sequence)
               (accumulate op initial (stream-cdr sequence)))))

(defun flatmap (proc seq)
  (accumulate #'append '() (mapcar proc seq)))

(defun map-over-strings (proc exp)
  (map-over-elements #'stringp proc exp))

(defun map-over-symbols (proc exp)
  (map-over-elements #'symbolp proc exp))

(defun map-over-atoms (proc exp)
  (map-over-elements #'atom proc exp))

(defun map-over-elements (pred proc exp)
  (cond ((null exp) '())
        ((funcall pred exp) (funcall proc exp))
        ((consp exp)
         (cons (map-over-elements pred proc (car exp))
               (map-over-elements pred proc (cdr exp))))
        (t exp)))

;;; Standalone interactive test driver.

(defun query-test ()
  (let ((fluent-tail-size 1))
    (labels
        ((prompt () (format t "~%Query> "))
         (usage ()
           (format t "Available commands:~%")
           (format t "  q, quit, exit  : exit the program~%")
           (format t "  show           : display database~%")
           (format t "  clear          : clear database~%")
           (format t "  help           : display this message~%")
           (format t "  (assert (...)) : assert a non-fluent~%")
           (format t "  (fluent (...)) : assert a fluent~%")
           (format t "  <integer>      : set tail size for fluents~%"))
         (standard-assertion? (exp)
           (eq (qtype exp) 'assert))
         (fluent-assertion? (exp)
           (eq (qtype exp) 'fluent))
         (add-assertion-body (exp)
           (car (query-contents exp)))
         (process-input ()
           (prompt)
           (let ((q (process-query-syntax (read))))
             (cond
              ((integerp q)
               (setq fluent-tail-size q)
               (format t "Last ~a elements of list are now considered values.~%"
                       fluent-tail-size))
              ((symbolp q)
               (case q
                 ((exit quit q) (throw 'exit 'exit))
                 ((show) (funcall qshow))
                 ((clear) (funcall qclear))
                 ((help) (usage))
                 (default (format t "Input not understood: ~a~%" q))))
              ((standard-assertion? q)
               (assert! (add-assertion-body q))
               (format t "Assertion added.~%"))
              ((fluent-assertion? q)
               (assert-fluent! (add-assertion-body q) fluent-tail-size)
               (format t "Fluent added.~%"))
              (t
               ;;
               ;; input interpreted as a query; perform it and display results
               ;;
               (let ((result
                      (stream-map
                       #'(lambda (frame)
                         (instantiate q frame
                                      #'(lambda (v f)
                                          (declare (ignore f))
                                          (contract-question-mark v))))
                       (qeval q (singleton-stream *the-empty-stream*)))))
                 (if (stream-empty? result)
                     (format t "No match.~%")
                   (display-stream result))))))))
      (catch 'exit
        (loop (process-input))))))



(defun initialize-default-data-base ()
  (let* ((query-table (make-memory-table))
         (operation-table (make-assoc-table #'assoc "~s~%")))
         ;; Note: only this function inserts operations!
;;         (opput (funcall operation-table 'insert-proc)))
    ;;
    ;; Define table accessors
    ;; 
    (setq qget (funcall query-table 'lookup-proc))
    (setq qput (funcall query-table 'insert-proc))
    (setq qshow (funcall query-table 'display-proc))
    (setq qclear (funcall query-table 'clear-proc))
    (setq qcontents (funcall query-table 'contents-proc))
    (setq opget (funcall operation-table 'lookup-proc))
    ;;
    ;; Define query operators
    ;;
    (init-query-operators operation-table)))
;    (apply opput `(compare ,#'lisp-value))
;    (apply opput `(and ,#'conjoin))
;    (apply opput `(or ,#'disjoin))
 ;   (apply opput `(not ,#'negate))))

(defun init-query-operators (table)
  (let ((put (funcall table 'insert-proc)))
    (apply put `(compare ,#'lisp-value))
    (apply put `(and ,#'conjoin))
    (apply put `(or ,#'disjoin))
    (apply put `(not ,#'negate))
    table))

;;; Startup 

;;; For testing
;;; (initialize-default-data-base)


