;;;-*- Mode: Lisp; Package: :pickle
;;;
;;; apex/system/utility/pickle.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: pickle.lisp,v 1.2 2006/01/15 03:43:03 dalal Exp $
;;; Created:        April, 2005

(defpackage :pickle
  (:use :common-lisp)
  (:export
   #:pickle
   #:pickle-object
   #:*pickle-jar*
   #:with-pickle-jar
   #:load-pickle-jar
   #:pickling
   #:taste-pickle
   )
  )

;;; ---------------------------------------------------------------------
;;;
;;; PICKLE. A package for 'pickling' (saving) Lisp objects to a file.
;;; 
;;; This is inspired by various scripting languages' pickle functions,
;;; well as the 'save-object' code which has been floating around. 
;;;
;;; It uses only ANSI Common Lisp to work its majick (at least, that
;;; is the goal :). 
;;; 
;;; Prerequisites:
;;;
;;; The objects to be saved must be one of the following:
;;;
;;; - A 'simple', self-printing object (like numbers, symbols, complex
;;;   numbers, strings,  etc.
;;; - A vector or array of 'pickleable' objects,
;;; - Structures whose print forms are readable,
;;; - hashtables whose keys and values are 'pickleable' and use
;;;   Ansi Common Lisp equality tests
;;; - CLOS objects or structure instances whose classes have a
;;;   MAKE-LOAD-FORM defined (or PICKLE-OBJECT) method defined.
;;;
;;;
;;; To load a pickle object, one just uses a LOAD form:
;;;
;;; (LOAD <path>)
;;;
;;; If <path> has been saved using pickling, a variable *PICKLE-JAR*
;;; will contain a list of pickled objects.
;;;
;;; (PICKLE-OBJECT ((object t)) )
;;;  returns the (pickle) load form for object.
;;;
;;; (TASTE-PICKLE ((object t) &optional (<compare-fn> nil)))
;;;  does a basic test to see if the object can be pickled, and read
;;;  in again. If a compare-fn is provided, its used to compare a 
;;;  result from evaluating a pickled form to the oritinal object.
;;;  returns two values: T/NIL for sucess; second value is
;;;  either the object read, or the error produced.
;;;
;;;
;;; Optionally, one can also use:
;;;
;;; (LOAD-PICKLE-JAR <path> &optional <then-empty-jar-p>)
;;;
;;; which does a load of <path> and returns the value of *PICKLE-JAR*,
;;; optionally setting *PICKLE-JAR* to NIL.
;;;
;;; To save a single object:
;;;
;;; (PICKLE <object> <path> &optional (<package> *package*)) => path
;;; 
;;;  saves a load-form for <object> to a file at <path>; the 
;;;  load form will be set inside <package>.
;;;
;;; The following macros are provided:
;;;
;;; (WITH-PICKLE-JAR (<path> &optional <package> *package*) 
;;;  <body>)
;;; 
;;;  stores the result of executing <body> to the pickle jar
;;;  at <path>.
;;;
;;; For example:
;;;  (with-pickle-jar "/tmp/temp.lisp" 
;;;    (loop for i from 1 to 1000 collecting i))
;;;
;;;
;;; (PICKLING (<path> &optional <package> *package*) 
;;;   <body>)
;;;
;;;  executes body, which probably contains PICKLE-OBJECT calls
;;;  creating a pickle jar at <path>
;;;
;;;  For example: 
;;;   (let ((numbers (loop for i from 1 to 1000 collecting i)))
;;;    (pickling "/tmp/temp.lisp"
;;;      (dolist (n numbers)
;;;       (when (oddp n) (pickle-object n)))))
;;;
;;; 
;;; Caveats:
;;;
;;; 1. You really do need MAKE-LOAD-FORM for CLOS objects.
;;; 2. Circular and recursively referential objects are not supported
;;;    -- but see the Hyperspec on how to do this for CLOS objects in
;;;       their discussion of MAKE-LOAD-FORM.
;;; 3. One is likely to get warnings about redefinitions of *pickle-jar*
;;;

(in-package :pickle)

(defmacro with-pickle-jar ((path &optional (package *package*)) &body body)
  (if package 
    `(pickle ,@body ,path ,package)
    `(pickle ,@body ,path)))

(defmacro pickling ((path &optional (package *package*))  &body body)
  `(let ((*pickling-stream* 
	  (open ,path 
		   :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)))
     (declare (special *pickling-stream*))
     (print-pickle-jar-label *pickling-stream* ,package :pickling)
     (unwind-protect
	 (progn ,@body)
       (print-pickle-jar-coda  *pickling-stream* :pickling)
       (close *pickling-stream*))))


(defparameter *pickle-jar* nil "List of pickled objects")
(defvar *pickling-stream* nil "Pathname to pickle file pickling")
(defvar *pickling-p* nil "Am I currently pickling?")


(defmethod pickle ((object t) path &optional (package *package*))
  (with-open-file (str path 
		   :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (print-pickle-jar-label str package :pickling)
    (pprint (pickle-object object) str)
    (print-pickle-jar-coda str :pickling))
  path)

(defmethod load-pickle-jar (path &optional (then-empty-jar-p t))
  (load path)
  (let ((val *pickle-jar*))
    (when then-empty-jar-p (setq *pickle-jar* nil))
    val))

(defmethod taste-pickle ((object T) &optional compare-fn)
  "pickle the object, and see if it at least EVALs. If compare-fn, called to compare object with pickled object.
If success, returns two values: t plus pickled object.
If error, returns nil plus error
If compare-fn, returns result plus pickled object."
  (multiple-value-bind (pickle-form error)
      (ignore-errors (pickle-object object))
    (if error 
      (values nil error)
      (multiple-value-bind (pickled-object error)
	  (ignore-errors (car (multiple-value-list (eval (read-from-string (format nil "~S" pickle-form))))))
	(if error
	  (values nil error)
	  (if compare-fn 
	    (values (funcall compare-fn object pickled-object)
		    pickled-object)
	    (values t pickled-object)))))))


;;;
;;; The PICKLE-OBJECT methods. 
;;;

(defmethod pickle-object ((object t))
  object)

(defmethod pickle-object ((object symbol))
  (if (or (keywordp object)
	  (null object)
	  (eql object t))
    object 
    `',object))

(defmethod pickle-object ((object standard-object))
  (multiple-value-bind (l e)
      (ignore-errors (multiple-value-list (make-load-form object)))
    (when e
	(error "Can't pickle ~s--needs MAKE-LOAD-FORM" object))
    (if (cdr l)
      `(PROGN ,@l)
      (car l))))

(defmethod pickle-object ((object structure-object))
  (multiple-value-bind (l e)
      (ignore-errors (multiple-value-list (make-load-form object)))
    (if e
      object
      (if (cdr l)
	`(PROGN ,@l)
	(car l)))))

(defmethod dotted-pair-p ((cons cons))
  (and (not (consp (cdr cons)))
       (not (null (cdr cons)))))
   
(defmethod dotted-list-p ((l list))
  (cond
   ((null l) nil)
   ((dotted-pair-p l) t)
   (t (dotted-list-p (cdr l)))))

(defmethod pickle-object ((l list))
  (if (dotted-list-p l)
    `(list* ,@(loop for cl on l by #'cdr collect (pickle-object (car cl)))
	    ,(pickle-object (cdr (last l))))
    `(list ,@(mapcar 'pickle-object l))))

(defmethod pickle-object ((ht hash-table))
  (let ((var (gentemp)))
    
    `(let ((,var (make-hash-table 
		  :test (hash-table-test-name ht)
		  :size ,(hash-table-size ht)
		  :rehash-size ,(hash-table-rehash-size ht)
		  :rehash-threshold ,(hash-table-rehash-threshold ht))))
       ,@(loop for key being the hash-key in ht using (hash-value val)
	     collect
	       `(setf (gethash ,(pickle-object key) ,var)
		  ,(pickle-object val)))
       ,var)))

(defmethod hash-table-test-name ((ht hash-table))
  (ecase (hash-table-test ht)
    ((eq #'eq) 'eq)
    ((eql #'eql) 'eql)
    ((equal #'equal) 'equal)
    ((equalp #'equalp) 'equalp)))
    

(defmethod pickle-object ((string string))
  string)

;;; need special string pickler cuz strings are arrays...

(defmethod pickle-object ((array array)) 
  `(make-array ,(pickle-object (array-dimensions array))
	       :element-type ,(pickle-object (array-element-type array))
	       :adjustable ,(adjustable-array-p array)
	       :initial-contents ,(pickle-object (array-to-initial-contents-list array))))

;;; this is based on a similar function in save-object called list-array-aux
(defun array-to-initial-contents-list (array)
  (let* ((dims (array-dimensions array))
	(len-dims-1 (1- (length dims))))
    (labels ((array-to-list* (array level subscript-list)
	       (loop for i from 0 to (1- (nth level dims)) collecting
		     (if (= level len-dims-1)
		       (pickle-object (apply #'aref array `(,@subscript-list ,i)))
		       (array-to-list* array (1+ level) `(,@subscript-list ,i))))))
      (array-to-list* array 0 nil))))

;;; utility functions



;;;
;;; This around method ensures that PICKLE-OBJECT in the context of pickling
;;; sends results to the pickle file.
;;; 

(defmethod pickle-object :around ((object t))
  (declare (special *pickling-stream* *pickling-p*))
  (if *pickling-p*
    (call-next-method)
    (let ((*pickling-p* t))
      (declare (special *pickling-p*))
      (let ((pickled (call-next-method)))
	(when *pickling-stream* (pprint pickled *pickling-stream*))
	    pickled))))

(defmethod print-pickle-jar-label (stream package &optional how)
  (let ((pname 
	 (cond ((symbolp package) package)
	       ((typep package 'package)
		(or (find-symbol (package-name package) (find-package :keyword))
		    (intern (package-name package))))
	       (t package)))) ;; ??
    (format stream "~&;;;-*- Mode: Lisp; Package: ~s~%" pname)
    (format stream ";;;~%")
    (multiple-value-bind (second minute hour date month year)
	(decode-universal-time (get-universal-time) 0)
      (format stream ";;; Created by pickle.lisp at ~4,'0,d-~2,'0,d-~2,'0,dT~2,'0,d:~2,'0,d:~2,'0,dZ~%"
	      year month date hour minute second))
    (format stream ";;;~%~%")
    (format stream "(in-package ~s)~%~%" pname)
    (format stream "(defparameter *pickle-jar* nil \"Stored object.\")~%")
    (format stream "(setq *pickle-jar* ")
    (when (eql how :pickling)
      (format stream "(list "))
    (format stream "~%")
    ))

(defmethod print-pickle-jar-coda (stream &optional how)
  (when (eql how :pickling)
    (format stream ")"))
  (format stream ")~%")
  (format stream ";;;~%"))

    

		     

