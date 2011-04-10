;;;-*- Mode: Lisp; Package: :apex.utility.unit-test -*-
;;;
;;; apex/system/utility/unit-test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: unit-test.lisp,v 1.5 2006/01/15 03:43:03 dalal Exp $
;;; Created:        August, 2004

;;;
;;;  Unit testing tools. See bottom for documentation.
;;; 

(defpackage :apex.utility.unit-test
  (:use :common-lisp)

  (:export
   ;; classes and slots 
   #:test
   #:test-no-error
   #:test-error
   #:with-tests
   #:define-test-function
   #:test-with-timeout
   #:current-test-tally
   #:current-test-successes
   #:current-test-errors
   #:current-test-failures
   #:*current-result*
   )
  )

(in-package :apex.utility.unit-test)

(defparameter +pass-prompt+ "Passed")
(defparameter +fail-prompt+ "Failed")
(defparameter +error-prompt+ "Error")


(defstruct test-result name (tallies 0) (successes 0) (errors 0) (failures 0))

(defvar *current-result* (make-test-result))

(defun current-test-tally ()
  (declare (special *current-result*))
  (when (test-result-name *current-result*)
    (test-result-tallies *current-result*)))

(defun current-test-successes ()
  (declare (special *current-result*))
  (when (test-result-name *current-result*)
    (test-result-successes *current-result*)))

(defun current-test-errors ()
  (declare (special *current-result*))
  (when (test-result-name *current-result*)
    (test-result-errors *current-result*)))

(defun current-test-failures ()
  (declare (special *current-result*))
  (when (test-result-name *current-result*)
    (test-result-failures *current-result*)))

(defun record-success ()
  (declare (special *current-result*))
  (when (test-result-name *current-result*)
    (incf (test-result-successes *current-result*))
    (incf (test-result-tallies *current-result*))))

(defun record-error ()
  (declare (special *current-result*))
  (when (test-result-name *current-result*)  
  (incf (test-result-errors *current-result*))
  (incf (test-result-tallies *current-result*))))

(defun record-failure ()
  (declare (special *current-result*))
  (when (test-result-name *current-result*)  
  (incf (test-result-failures *current-result*))
  (incf (test-result-tallies *current-result*))))


;;; woo! check out those format statements ...
(defun print-one-result (result form err if-success if-fail if-error)
  (cond
   (err 	  
    (format t "~a~@[~{ ~a~}~]: ~a: ~a~%"
	    +error-prompt+
	    (test-result-name *current-result*)
	    (or if-error form)	    
	    (apply #'format nil (simple-condition-format-control err)
		   (simple-condition-format-arguments err))
	    ))
   (result 
    (format t "~a~@[~{ ~a~}~]: ~a~%"
	    +pass-prompt+
	    (test-result-name *current-result*)
	    (or if-success form)))
   (t 
    (format t "~a~@[~{ ~a~}~]: ~a~%"
	    +fail-prompt+
	    (test-result-name *current-result*)
	    (or if-fail form)))))
    
(defmacro test-no-error (form &key if-error if-success)
  (let ((resvar (gensym))
	(fvar (gensym))
	(errvar (gensym)))
    `(progn
       (let ((,fvar nil))
       (multiple-value-bind (,resvar ,errvar)
	   (ignore-errors (progn (setq ,fvar ,form) t))
	 (print-one-result ,resvar ',form ,errvar ,if-success nil ,if-error)
	 (cond
	  (,errvar (record-error))
	  (T (record-success)))
	 ,fvar)))))

(defmacro test-error (form &key if-error if-success)
  (let ((resvar (gensym))
	(errvar (gensym)))
    `(progn
       (multiple-value-bind (,resvar ,errvar)
	   (ignore-errors (progn ,form t))
	 (print-one-result (not (eq ,errvar nil)) 
			   ',form nil ,if-success nil ,if-error)
	 (cond
	  (,errvar (record-success))
	  (T (record-failure)))
	 (not (eq ,errvar nil))))))

(defmacro test (target probe 
		&key (test #'equal) 
		     (if-success nil) 
		     (if-failure nil) 
		     (if-error nil))
  (let ((resvar (gensym))
	(errvar (gensym)))
    `(progn
       (multiple-value-bind (,resvar ,errvar)
	 (ignore-errors 
	  (funcall ,test ,target ,probe))
	 (print-one-result ,resvar 
			   ',probe ,errvar ,if-success ,if-failure ,if-error)
       (cond
	(,errvar (record-error))
	(,resvar (record-success))
	(T (record-failure)))
       (eq ,resvar T)))))

(defun display-test-stats ()
  (declare (special *current-result*))
  ;;; display only 'top level' results -- i.e., one name
  (let ((test-result-name (test-result-name *current-result*)))
  (when (and test-result-name
	     (null (cdr test-result-name)))
    (let ((test-tallies (test-result-tallies *current-result*)))
      (if (= test-tallies 0)
	(format t   "Warning: no tests defined~%")
	(let ((test-errors (test-result-errors *current-result*))
	      (test-successes (test-result-successes *current-result*))
	      (test-failures (test-result-failures *current-result*)))
	  (progn
	    (format t "Results for: ~{~a~}~%"  test-result-name)
	    (format t "Errors:    ~a~%" test-errors)
	    (format t "Failures:  ~a~%" test-failures)
	    (format t "Successes: ~a~%" test-successes)
	    (format 
	     t 
	     "Success rate: ~5,2f%~@[ <---------------------- Failures or Errors!~]~%" 
	     (* 100 (/ test-successes test-tallies))
	     (/= test-successes test-tallies)
	     ))))))))

(defun display-banner (which)
  (declare (special *current-result*))
  (when (eql which :end)
    (format t "End   ~{~a/~}~%" (test-result-name *current-result*)))
  (when (and (test-result-name *current-result*)
	     (null (cdr (test-result-name *current-result*))))
    (format t "***************************~%"))
  (when (eql which :begin)
    (format t "Begin ~{~a/~}~%" (test-result-name *current-result*))))


(defun all-tests-ok? ()
  (declare (special *current-result*))
  (= (test-result-successes *current-result*)
     (test-result-tallies *current-result*)))

(defun propagate-results (old new)
  (when (test-result-name old)
    (incf (test-result-tallies old) (test-result-tallies new))
    (incf (test-result-failures old) (test-result-failures new))
    (incf (test-result-successes old) (test-result-successes new))
    (incf (test-result-errors old) (test-result-errors new))
    old))
  
(defmacro with-tests ((&key name &allow-other-keys) &body body)
  (let ((namevar (gensym))
	(oldvar (gensym)))
    `(let ((,oldvar *current-result*)
	   (,namevar 
	    (append (test-result-name *current-result*) 
		    (list ',(or name (gensym "Unit Test "))))))
       (let ((*current-result* (make-test-result :name ,namevar)))
	 (declare (special *current-result*))
	 (display-banner :begin)
	 ,@body	;;; tests should be running here ...
	 (propagate-results ,oldvar *current-result*)
	 (display-banner :end)
	 (display-test-stats)
	 (all-tests-ok?)))))
	 

(defmacro define-test-function (name parameters &body body)
  `(defun ,name ,parameters
    (with-tests (:name ,name)
      ,@body)))

#+allegro(defmacro test-with-timeout ((seconds) &body body)
      `(test-no-error (mp:with-timeout (,seconds) (prog1 t ,@body))))

#-allegro(defmacro test-with-timeout ((seconds) &body body)
	  `(test-no-error (prog1 t ,@body)))

;;; --------------------------------------------------------------------
;;;
;;; Yet another unit testing facility in Lisp
;;;
;;;
;;; This is based on ACL's tester, but is a bit simpler to use.
;;; It also has a couple ideas from _Practical Common Lisp_
;;; by Peter Siebel. 
;;; <http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html>
;;;
;;; The following macros are defined:
;;;
;;; (TEST target probe &key (test #'equal))
;;; (TEST-NO-ERRROR form) 
;;; (TEST-ERRROR form) 
;;; (WITH-TESTS (:key (name "Unit Tests")) &body body)
;;; (DEFINE-TEST-FUNCTION  (name parameters &body body)
;;; (TEST-WITH-TIMEOUT (seconds) &body body)
;;; --------------------------------------------------------------------
;;;
;;; (TEST target probe &key (test #'equal))
;;;
;;; Evaluates *target* and *probe* and compares them using *test*
;;; Returns T if test returns T, otherwise NIL. Returns NIL on error.
;;; Prints a message about the result.
;;; Any errors raised are caught. 
;;;
;;; > (test 1 (/ 2 2))
;;;   Passed: (/ 2 2)
;;;   t
;;; > (test 1 (/ 2 1))
;;;   Failed: (/ 2 1)
;;;   nil
;;; > (test 1 (/ 2 0))
;;;    Error (Attempt to divide 2 by zero.): (/ 2 0)
;;;   nil
;;;
;;; (TEST-NO-ERROR form) 
;;;
;;; Evaluates *form*, and returns T if no error is raised, NIL otherwise.
;;; Prints a message about the result
;;; 
;;; > (test-no-error (/ 2 2))
;;;   Passed: (/ 2 2)
;;;   t
;;; > (test-no-error (/ 2 0))
;;;   Error (Attempt to divide 2 by zero.): (/ 2 0)
;;;   nil
;;;
;;; (WITH-TESTS (:key (name "Unit Tests")) &body body)
;;;
;;; Evaluates *body* as a PROGN, collecting test statistics.
;;; Returns result of evaluating *body*.
;;; Displays results at the end. 
;;; 
;;; > (with-tests (:name "Simple Test") (test 1 (/ 2 2)) (test 1 (/ 2 1)) (test 1 (/ 2 0)))
;;;  +++++++++++++++++++
;;;  Begin Simple Test
;;;  Passed Simple Test: (/ 2 2)
;;;  Failed Simple Test: (/ 2 1)
;;;  Error Simple Test (Attempt to divide 2 by zero.): (/ 2 0)
;;;  End Simple Test
;;;  +++++++++++++++++++
;;;  Errors:    1
;;;  Failures:  1
;;;  Successes: 1
;;;  Success rate: 33.33%
;;;  nil
;;;
;;; It is possible to nest WITH-TESTS forms. 
;;; Only the 'top' test has stats printed (which are collected for internal tests).
;;; 
;;; > (with-tests (:name "Outer") 
;;;     (with-tests (:name "1") (test 1 1)) 
;;;     (with-tests (:name "2") (test 2 2)))
;;;  +++++++++++++++++++
;;;  Begin Outer/
;;;  Begin Outer/1/
;;;  Passed: 1
;;;  End Outer/1/
;;;  Begin Outer/2/
;;;  Passed: 2
;;;  End Outer/2/
;;;  End Outer/
;;;  +++++++++++++++++++
;;;  Results for: Outer
;;;  Errors:    0
;;;  Failures:  0
;;;  Successes: 2
;;;  Success rate: 100.00%
;;;  T
;;;
;;; (DEFINE-TEST-FUNCTION  (name parameters &body body)
;;;
;;; This is like DEFUN, with a WITH-TESTS form wrapped around
;;; the function name. The cool thing that this allows
;;; is writing test functions that are nested in the right
;;; way. For example:
;;;
;;; (define-test-function test-plus (n)
;;;   (test n (+ n 0))
;;;   (test 0 (- n n))
;;;   (test (1+ n) (+ n 1)))
;;;
;;; (define-test-function test-mult (n)
;;;   (test n (* n 1))
;;;   (test 0 (* 0 n))
;;;   (test (* n n) (expt n 2)))
;;;
;;; (define-test-function test-math (n)
;;;   (test-plus n)
;;;   (test-mult n))
;;;
;;; > (test-math 10)
;;; +++++++++++++++++++
;;; Begin TEST-MATH/
;;; Begin TEST-MATH/TEST-PLUS/
;;; Passed: (+ N 0)
;;; Passed: (- N N)
;;; Passed: (+ N 1)
;;; End TEST-MATH/TEST-PLUS/
;;; Begin TEST-MATH/TEST-MULT/
;;; Passed: (* N 1)
;;; Passed: (* 0 N)
;;; Passed: (EXPT N 2)
;;; End TEST-MATH/TEST-MULT/
;;; End TEST-MATH/
;;; +++++++++++++++++++
;;; Results for: TEST-MATH
;;; Errors:    0
;;; Failures:  0
;;; Successes: 6
;;; Success rate: 100.00%
;;; T
;;;
;;; (TEST-WITH-TIMEOUT (seconds) &body body)
;;; 
;;; Defined only for Allegro
;;; equivalent to (test-no-error (mp:with-timeout (,seconds) (prog1 t ,@body)))
;;;


