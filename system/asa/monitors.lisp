;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/monitors.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: monitors.lisp,v 1.38 2006/03/13 19:22:37 will Exp $
;;; Created:        September. 2004

;;
;;
;; Monitors 

;;; ------ Monitors

;;; Monitors are triggers (cf. demons) set up when tasks are
;;; instantiated, each representing a task precondition and
;;; corresponding to a WAITCONJ in the task's WAITS slot. A monitor may
;;; also appear in (monitors (resource-allocation-table <agent>)) in
;;; which case it is a resource precondition determined by the source
;;; procedure's PROFILE slot.  In the former case, the value of the wait
;;; slot is a waitconj whose -expr slot is to be matched against each
;;; cogevent; in the latter, its value is of the form (available <res>
;;; <val>) and matched against events directly.  Either way, the -task
;;; slot points to the source task.

;;; A waitconj is a single conjunct in the conjunction created when a
;;; procedure step's waitfor clause is instantiated.  The expr slot
;;; contains a variablized form to be matched against new cogevents.
;;; The val slot is T if a matching event has been detected, else nil.
;;; The type slot is either nil or special.  Specials are lisp
;;; expressions, though they may contain variables.

;;; A task's waits slot contains preconditions (waitconj structures) in
;;; disjunctive normal form --- i.e. disjunction of conjunctions.  Each
;;; disjunct corresponds to a single WAITFOR clause in the procedure
;;; specification.


(in-package :cl-user)

;;; debugging macros ...
(defparameter pv-debugging nil)

(defmacro pv (x)
  (cond
   ((stringp x)
    `(when pv-debugging (format t "~a " ,x)))
   ((eq x :newline)
    `(terpri))
   (t (let ((v (gensym)))
	`(when pv-debugging (let ((,v ,x)) (format t "~a: ~s " ',x ,v) ,v))))))

(defmacro pvs (&rest xs)
  `(when pv-debugging (prog1 (progn ,@(loop for x in xs collecting `(pv ,x))) (terpri))))

(defmacro pv! (x)
  `(let ((pv-debugging t))
     (pv ,x)))

(defmacro pvs! (&rest xs)
  `(let ((pv-debugging t))
     (pvs ,@xs)))


;;; some constants. They should probably be used for
;;; comparisons only.

(defconstant +end-of-time+ most-positive-single-float)
(defconstant +beginning-of-time+ 0)
(defconstant +pos-infinity+ most-positive-single-float)
(defconstant +neg-infinity+ most-negative-single-float)
(defconstant +always-dur+ (make-interval 0 +end-of-time+))
;;;;

;;; Printing utilities
;;; ----------------------------------------------------------------------

(defun ->monitor (thang)
  (cond
   ((symbolp thang)
    (find-class thang))
   (t thang)))

(defun monitor-class-add-constraint-methods (monitor-class)
  (loop for meth in (mop:specializer-direct-methods monitor-class)
      when (eql (mop:generic-function-name (mop:method-generic-function meth))
		'add-monitor-constraint)
	   collect meth))

(defun keywords-for-monitor-class (monitor-class)
  (let ((methods (monitor-class-add-constraint-methods monitor-class)))
    (let ((keys nil))
      (dolist (meth methods (sort (nreverse keys) #'string< :key #'symbol-name))
	(let* ((llist (mop:method-lambda-list meth))
	       (typp (car llist)))
	  (when (and (consp typp)
		     (consp (cadr typp)))
	    (push (cadr (cadr typp)) keys)))))))

(defun display-monitor-hierarchy (name 
				  &optional (show-keywords t) (stream *standard-output*) 
				  &aux shown)
  (labels ((display (name prefix)
             (let* ((monitor (->monitor name))
                    (specs (mapcar #'class-name (reverse (mop:class-direct-subclasses monitor))))
                   (keywords (and show-keywords (keywords-for-monitor-class monitor))))
               (cond ((member name shown)
                      (format stream
			      (if (or specs keywords) "~S...~%" "~S~%") name))
                     (t
                      (format stream "~S~%" name)
                      (push name shown)
                      (when keywords
                        (let ((bar (if specs "|" " ")))
                          (dolist (keyword keywords)
			    (format stream "~A ~A ~S~%" prefix bar
				    keyword))))
                      (when specs
                        (do ((next-prefix (format nil "~A |   " prefix))
                             (last-prefix (format nil "~A     " prefix))
                             (l specs (cdr l)))
                            ((null (cdr l))
                             (format stream "~A +-- " prefix)
                             (display (car l) last-prefix))
                          (format stream "~A |-- " prefix)
                          (display (car l) next-prefix))))))))
    (display name "")
    (values)))

(defun parameter-value (keyword parameters)
  (second (member keyword parameters)))

;;; To create a new class of monitor 
;;;
;;; Let's say we want to create a monitor of type :FOOBAR and its
;;; 'constraints' are :TIMESTAMP :OLDEST :FRIENDLIEST
;;; for example:
;;;
;;; (waitfor (:foobar o1 (age ac1 32) :oldest t :friendliest (> P30M)))
;;;
;;; 
;;; 1. create the CLOS class inheriting in just the right way from existing monitors
;;;   (defclass foobar-monitor (monitor) .... )
;;;
;;; 2. write a CREATE-MONITOR-FROM-PATTERN that does instance creation & initialization
;;;    (defmethod compile-monitor-from-pattern ((type (eql :foobar)) expr parameters pattern)
;;;     (make-instance 'foobar-monitor :oldest nil :friendliest nil :timestamp nil))
;;;
;;; 3. write a ADD-MONITOR-CONSTRAINT method for each parameter/constraint
;;;   (defmethod add-montior-constraint ((type (eql :oldest)) (mon foobar-monitor) val)
;;;    (setf (oldest mon) val))
;;;   (defmethod add-montior-constraint ((type (eql :friendliest)) (mon foobar-monitor) val)
;;;    (setf (friendliest mon) (cons val (friendliest mon))))
;;;   (defmethod add-montior-constraint ((type (eql :timestamp)) (mon foobar-monitor) val)
;;;    (setf (timestamp-constraints mon) (append val (timestamp-constraints mon))))
;;;
;;;  You might have to write even methods for different types of monitors (as in
;;;  the measurment estimation monitors)
;;;
;;;  Then, you should write two methods to handle event signalling:
;;;
;;;  (defmethod SIGNAL-MONITOR ((monitor <newclass>) (task task) (time (eql :creation))) 
;;;  (defmethod SIGNAL-MONITOR ((monitor <newclass>) (task task) (cogevent cogevent))
;;;
;;;  These should return EMPTY-PIPE if the monitor does not complete, or a pipe
;;;  of binding sets (any of which could be NO=BINDINGS).
;;;
;;;  The first method will get called at the time the monitor gets created.
;;;  The second method will get called with every new event that comes in.
;;; 
;;;  For efficiency, it's probably a good idea to have:
;;;
;;;    (if (not (eq (event-type cogevent) 
;;;	        (first (monitor-expr monitor))))
;;;        fail
;;;
;;;  so that less time is taken in processing the cogevent. But, this will depend
;;;  on the monitor, of course.
;;;

;;; -- *this* is what a result should look like ...

(defclass monitor-result ()
  ((binding-set :initarg :binding-set :initform no-bindings :accessor binding-set)
   (interval    :initarg :interval :initform nil :type interval :accessor interval)
   (partial-interval :initarg :partial-interval :initform nil :type interval :accessor partial-interval)
   (monitor     :initarg :monitor  :initform nil :type monitor :accessor monitor)))

(defvar *print-monitor-result-verbosely-p* NIL "Print monitor result verbosely?")

(defmethod print-object ((result  monitor-result) (stream stream))
  (if *print-monitor-result-verbosely-p*
    (print-unreadable-object (result stream :type t :identity t)
      (with-slots (binding-set interval partial-interval monitor) result
	(format stream "~S " (monitor result))
	(if (not (eq binding-set no-bindings))
	  (progn
	    (format stream " :bindings ~s" binding-set)
	    (format stream " :interval ~s" interval))
	  (progn
	    (format stream " <no bindings>")))))
  (print-unreadable-object (result stream :type t :identity t)
    (format stream "~a"
	    (or (and (monitor result) (name (monitor result)))
		(monitor result))))))

(defun make-monitor-result (binding-set interval &optional monitor partial-interval)
  (make-instance 'monitor-result 
    :binding-set binding-set 
    :interval interval
    :monitor monitor
    :partial-interval partial-interval))

(defmethod start-of ((r monitor-result))
  (start-of (interval r)))

(defmethod end-of ((r monitor-result))
  (end-of (interval r)))

(defclass monitor-status ()
  ((binding-set :initarg :binding-set :initform NIL :accessor binding-set)
   (state :initarg :state :initform :unknown :accessor state)
   (interval    :initarg :interval :initform nil :type interval :accessor interval)))

(defmethod print-object ((status monitor-status) (stream stream))
  (print-unreadable-object (status stream :type t :identity nil)
    (format stream "~a" (state status))))

(defvar *monitor-state-descriptors* 
    '(:satisfied :unsatisfied :partially-satisfied :unknown)
  "States a monitor can be in.")

	      
(defclass monitor (appob)
  ((task :accessor task :initarg :task :initform nil
	 :documentation "task that the monitor checks precondition for.")
   (pattern :initarg :pattern :initform NIL :accessor monitor-pattern
	 :documentation "most general pattern of monitor pattern, including type, etc.")
   (expr :initarg :expr :initform NIL :accessor monitor-expr
	 :documentation "Typically, the actual form that gets pattern matched.")
   (val :accessor val :initarg :val :initform nil
	:documentation "whether or not the monitor has been satisified")
   (type :initarg :type :initform NIL :accessor monitor-type 
	 :documentation "Type of monitor")
   (tag :accessor tag :initarg :tag :initform nil 
	:documentation "A symbolic name for the monitor")
   (relevant-types :accessor relevant-types 
		   :initarg :relevant-types :initform nil
		   :documentation "cogevents types relevant to this monitor")
   (parameters :initarg :parameters :initform NIL :accessor monitor-parameters)
   (start           :initarg :start :initform (current-time) :accessor monitor-start)
   (time-recognized :initarg :time-recognized :initform nil :accessor monitor-time-recognized)
   (status :initarg :status :initform (make-instance 'monitor-status)
	   :accessor monitor-status)
   (parent-monitor :initarg :parent-monitor :initform nil :accessor parent-monitor)
   (containing-array                    ; monitor-array in agent
    :accessor containing-array)
   (parent-slot
    :allocation :class
    :initform 'containing-array
    :reader parent-slot)
   (recognition-interval
    :initarg :recognition-interval
    :initform nil
    :accessor recognition-interval
    :documentation "Interval over which monitor was recognized; transient, used for complex monitors")
   (satisfied-by
    :initarg :satisfied-by
    :accessor satisfied-by
    :initform NIL
    :documentation "Event satisfying a monitor")
   ))

(defmethod monitor-state ((monitor monitor))
  (ecase (state (monitor-status monitor))
    (:unknown "unsatisfied")
    (:unsatisfied "unsatisfied")
    (:satisfied "satisfied")
    (:partially-satisfied "partially satisfied")))

    
;;;(defmethod colorized-monitor-state ((monitor monitor))
;;;  (let* ((state (monitor-state monitor))
;;;	 (color  (cond ((string-equal state "unsatisfied")
;;;			"#FF0000")
;;;		       ((string-equal state "satisfied")
;;;			"#008000")
;;;		       ((string-equal state "partially satisfied")
;;;			"#FFD700")
;;;		       (t
;;;			"#000000"))))
;;;    (wrap-html state 
;;;	       (format nil "<FONT COLOR=~a>" color)
;;;	       "</FONT>")))

(defmethod colorized-monitor-state ((monitor monitor))
  (wrap-html (monitor-state monitor)
	     (format nil "<FONT COLOR=~a>" (color-for-state monitor))
	     "</FONT>"))

(defun wrap-html (s prefix &optional (suffix ""))
  (format nil "~a~a~a" prefix s suffix))

(defmethod color-for-state ((monitor monitor))
  (ecase (state (monitor-status monitor))
    (:unknown "#FF0000")
    (:unsatisfied "#FE1005")
    (:satisfied "#008000")
    (:partially-satisfied "#f77b00"))) 

(defmethod colorized-monitor-string ((monitor monitor) string)
  (wrap-html (escape-html-entities string)
	     (format nil "<FONT COLOR=~a>" (color-for-state monitor))
	     "</FONT>"))

;;;(defmethod monitor-summary ((monitor monitor))
;;;  (let ((bindings (binding-set (monitor-status monitor)))
;;;	(interval (interval (monitor-status monitor))))
;;;    (with-output-to-string (str)
;;;      (princ (monitor-pattern monitor) str)
;;;      (princ "<BR>" str)  ;;(terpri str)
;;;      (princ ":state " str)
;;;      (princ (colorized-monitor-state monitor) str)
;;;      (when (and bindings (not (eq bindings no-bindings)))
;;;	(bindings-for-each 
;;;	 (lambda (binding)
;;;	   (princ (binding-var binding) str)
;;;	   (princ "=" str)
;;;	   (princ (binding-val binding) str)
;;;	   (princ "<BR>" str)  ;;(terpri str)
;;;	   )
;;;	 bindings)
;;;	(princ "<BR>" str)  ;;(terpri str)
;;;	)
;;;      (when interval
;;;	(princ (interval-string interval) str)
;;;	(princ "<BR>" str)  ;;(terpri str)
;;;	)
;;;      (when bindings
;;;	(princ (substitute-bindings (monitor-expr monitor) bindings)
;;;	       str)
;;;	(princ "<BR>" str)  ;;(terpri str)
;;;	)
;;;      )))

(let ((tab-table (make-hash-table))
      (cnt 0))
  (defun space-tabs (n)
    (or (values (gethash n tab-table))
	(let ((str
	       (with-output-to-string (str)
		 (dotimes (i n)
		   (princ "&nbsp;" str)))))
	  (when (< cnt 100) ;; hate to allow anything to grow without limit...
	    (incf cnt)
	    (setf (gethash n tab-table) str))
	  str))))

(defun bindings-to-list (bindings) ;; Bindings -> List; should go in pat-match.lisp
  (if (eq bindings no-bindings) nil
      bindings))

(defun print-bindings-p (bindings)
  (and (not (eq bindings no-bindings))
       (some 'print-binding-p
	      (bindings-to-list bindings))))

(defun print-binding-p (binding)
  (and 
   (binding-val binding)
   (not (typep (binding-val binding)
	       'monitor-result))
   (not (typep (binding-val binding)
	       'task))))

(defmethod monitor-summary ((monitor monitor))
  (with-output-to-string (str)
    (monitor-summary* monitor str t 0)))

(defmethod monitor-summary* ((monitor monitor) (stream stream) print-inners tabs)
  (when (> tabs 0)
    (princ (space-tabs tabs) stream))
  (let ((bindings (binding-set (monitor-status monitor)))
	(interval (interval (monitor-status monitor))))
    (if (submonitors monitor)
      (let ((firstpart (format nil "(~S " (first (monitor-pattern monitor)))))
	(princ (colorized-monitor-string monitor firstpart) stream)
	(dolist (submon (submonitors monitor))
	  (princ "<BR>"stream)
	  (monitor-summary* submon stream nil (+ tabs (length firstpart))))
	(princ ")" stream))
      ;; else ...
      (princ (colorized-monitor-string 
	      monitor
	      (princ-to-string 
	       (monitor-pattern monitor))) stream))
      (when print-inners
	(princ "<BR>" stream)  ;;(terpri str)
	(when (and bindings (print-bindings-p bindings))
	  (bindings-for-each 
	   (lambda (binding)
	     (when (print-binding-p binding)
	       (princ (escape-html-entities (binding-var binding)) stream)
	       (princ "=" stream)
	       (princ (escape-html-entities (binding-val binding)) stream)
	       (princ "<BR>" stream)  ;;(terpri stream)
	       ))
	   bindings)
	  (princ "<BR>" stream)  ;;(terpri stream)
	  )
	(when interval
	  (princ (escape-html-entities (interval-string interval)) stream)
	  (princ "<BR>" stream)  ;;(terpri stream)
	  ))
      ))
			     
  

(defmethod submonitors ((monitor monitor)) nil)

(defclass non-complex-monitor (monitor)
  (
   (value-constraints :initarg :value-constraints :initform NIL :accessor monitor-value-constraints)
   (object-constraints :initarg :object-constraints :initform NIL :accessor monitor-object-constraints)   
   )
  (:documentation "The parent class for atomic episodes, simple episodes, measurements, estimation"))


;;; 
;;; (will) I usually personally prefer CLASS-SLOT type names, so we need to create some methods
;;; to support legacy names for these slot accessors.
;;;

(defmethod expr ((mon monitor))
  (monitor-expr mon))

(defmethod (setf expr) (val (mon monitor))
  (setf (monitor-expr mon) val))

(defmethod pattern ((mon monitor))
  (monitor-pattern mon))

(defmethod (setf pattern) (pat (mon monitor))
  (setf (monitor-pattern mon) pat))

(defmethod containing-object ((x monitor)) ; -> Task
  (task x))

(defmethod start-of ((x monitor))
  (if (recognition-interval x)
    (interval-start (recognition-interval x))
    (monitor-start x)))

(defmethod end-of ((x monitor))
  (if (recognition-interval x)
    (interval-end (recognition-interval x))
    (monitor-time-recognized x)))

(defmethod print-object ((monitor  monitor) (stream stream))
  (format stream "{~a ~s : ~a}" (id monitor) (monitor-pattern monitor) (monitor-state monitor)))

(defmethod signal-monitor ((monitor monitor) (task task) event &optional added-bindings)
  (declare (ignore added-bindings))
  (error "SIGNAL-MONITOR on top-level MONITOR class called; ignoring: ~a ~a ~a"
	 monitor task event))

(defmethod set-monitor-status ((monitor monitor) state &optional bindings interval)
  (assert (member state *monitor-state-descriptors*))
  (let ((status-state (monitor-status monitor)))
    (setf (state status-state) state)
    (setf (binding-set status-state) bindings)
    (setf (interval status-state) interval)
    state))

;; for syntax ...

(defmethod create-monitor-from-pattern ((monitor-type T) expr parameters pattern)
  (declare (ignore expr parameters))
  (error "Unknown monitor type: ~s in ~s" monitor-type pattern))

(defmethod add-monitor-constraint ((type T) (monitor monitor) value)
  (error "Unknown parameter ~a with value ~a for monitor ~a (pattern: ~A)"
	 type value monitor (pattern monitor)))

;;; (waitfor (:measurement o1 (x b c) :timestamp P32))
;;; (waitfor (:holding w1 (x b c) :start ((> (+ (start-of +this-task+) P10M)))))
;;; 

;;; convert things like (:range 40 50) -> ((>= 40) (<= 50))

(defmethod convert-constraint-function ((name t) (pattern list))
  (list pattern))

(defmethod convert-constraint-function ((name (eql :range)) (pattern list))
  `((>= ,(cadr pattern)) (<= ,(third pattern))))

(defmethod convert-constraint-function ((name (eql :min)) (pattern list))
  `((>= ,(cadr pattern))))

(defmethod convert-constraint-function ((name (eql :max)) (pattern list))
  `((<= ,(cadr pattern))))


(defun convert-constraints (constraint)
  (if (consp constraint)
    (convert-constraint-function (car constraint) constraint)
    (list constraint)))

(defmethod add-monitor-constraints ((mon monitor) (keyword-parameters list) current-keyword)
  (when (not (null keyword-parameters))
    (let ((kp (car keyword-parameters))
	  (kps (cdr keyword-parameters)))
      (if (keywordp kp)
	(add-monitor-constraints mon kps kp)
	(progn
	  (dolist (constraint (convert-constraints kp))
	    (add-monitor-constraint current-keyword mon constraint))
	  (add-monitor-constraints mon kps current-keyword))))))

(defun potential-monitor-type-p (x)
  (keywordp x))

;;;#+allegro(defun is-monitor-type-p (x)
;;;  (and (keywordp x)
;;;       (let ((methods (compute-applicable-methods #'create-monitor-from-pattern (list x nil nil nil))))
;;;	 (and methods
;;;	      (equal (car (mop:method-lambda-list (car methods)))
;;;		     `(type (eql ,x)))))))
;;;
;;;#-allegro(defun is-monitor-type-p (x)
;;;       (keywordp x))

;;;
;;; If the pattern isn't atomic, then we should be able to match to 
;;; its pieces. The only difficulty is that the tag is optional
;;; we return four values: 
;;;  - the *type*, 
;;;  - the *tag* (which might be nil)
;;;  - the *form* 
;;;  - the *parameters* (which might be nil)
(defun non-atomic-pattern-p (pattern)
  (if (valid-measurement-query-p pattern)
    (values :measurement nil pattern)
    (let ((bs (pat-match '((?is ?type potential-monitor-type-p)
			   (?or 
			    (?is ?tag stringp)
			    (?is ?tag symbolp))
			   (?or 
			    (?is ?form consp)
			    (?is ?form variable-p))
			   . ?rest)
			 pattern)))
      (if (not (eq bs fail))
	(values (lookup '?type bs)
		(lookup '?tag bs)
		(lookup '?form bs)
		(lookup '?rest bs))
	(let ((bs (pat-match '((?is ?type potential-monitor-type-p)
			       (?or 
				(?is ?form consp)
				(?is ?form variable-p))
			       . ?rest)
			     pattern)))
	  (if (not (eq bs fail))
	    (values (lookup '?type bs)
		    nil
		    (lookup '?form bs)
		    (lookup '?rest bs))
	    nil))))))

;;;(defun measurement-form-p (pattern) ;; list -> binding or nil
;;;  (or (pat-match '(?attr ?obj = ?val)
;;;		 pattern)
;;;      (pat-match '(?attr ?obj = ?val +/- ?range)
;;;		 pattern)
;;;      nil))

(defun valid-measurement-query-p (expr)
  (or (not (eq fail (pat-match  '(?attr ?obj (?or = < <= > >=) ?val) expr)))
      (not (eq fail (pat-match  '(?attr ?obj = ?val +/- ?range) expr)))))

(defmethod special-monitor-pattern-p ((tag T))
  nil)

(defmethod compile-special-monitor-pattern ((tag T) pattern)
  (error "Invalid call to COMPILE-SPECIAL-MONITOR-PATTERN with ~s" pattern))

(defun short-name (symbol)
  (let ((name (symbol-name symbol)))
    (let ((len (length name)))
      (let ((len-2 (- len 2)))
	(intern 
	 (with-output-to-string (str)
	   (princ (char-upcase (aref name 0)) str)
	   (loop for i from 1 to (1- len) doing
		 (cond
		  ((and (char= (aref name i) #\-)
			(<= i len-2))
		   (princ #\- str)
		   (princ  (char-upcase (aref name (1+ i))) str))
		  ((and (digit-char-p (aref name i))
			(digit-char-p (aref name (1- i))))
		   (princ (aref name i) str))))))))))

(defun force-monitor-tag (monitor)
  (unless (tag monitor)
    (setf (tag monitor)
      (short-name (id monitor)))))


(defmethod name ((monitor monitor))
  (with-output-to-string (str)
    (princ "(" str)
    (mon-name (expr monitor) str)))

(defmethod mon-name (expr str)
  (if (null expr)
    (princ ")" str)
    (let ((f (car expr)))
      (cond
       ((keywordp f)
	(format str "~S" f))
       ((typep f 'id-mixin)
	(format str "~a" (string-downcase (symbol-name (id f)))))
       ((consp f)
	(princ "(" str)
	(mon-name f str))
       (t (format str "~a" f)))
      (when (not (null (cdr expr)))
	(princ #\Space str))
      (mon-name (cdr expr) str))))

(defun compile-monitor (pattern)
  (if (special-monitor-pattern-p (car pattern))
    (let ((mon 
	   (compile-special-monitor-pattern (car pattern) pattern)))
      (setf (pattern mon) pattern)
      (setf (expr mon) pattern)
      (setf (task mon) *this-task*)
      (setf (monitor-start mon) (current-time))
      (compile-monitor-post-process mon)
      mon)
    (let ((the-monitor 
	 (multiple-value-bind (monitor-type tag expr potential-parameters)
	     (non-atomic-pattern-p pattern)
	   (if monitor-type
	     (let* ((parameters (member-if #'keywordp potential-parameters))
		    (mon (create-monitor-from-pattern monitor-type expr parameters pattern)))
	       (setf (pattern mon) pattern)
	       (if tag 
		 (setf (tag mon) tag)
		 (force-monitor-tag mon))
	       (setf (monitor-expr mon) expr)
	       (setf (task mon) *this-task*)
	       (setf (monitor-start mon)
		 (current-time))
	       (when parameters
		 (setf (monitor-parameters mon) parameters)
		 (add-monitor-constraints mon (cdr parameters) (car parameters)))
	       mon)
	       ;; ok, it's atomic -- build a form that can be compiled ...
	     (compile-monitor `(:atomic-episode ,pattern))))))
      (compile-monitor-post-process the-monitor)
      the-monitor)))

;;; override if necessary
(defmethod compile-monitor-post-process ((monitor monitor)) 
  (values))


;;;
;;; Utilities for monitors.
;;;

(defun convert-duration-specs (l)
  (if (duration-expression? l)
    (duration-read l)
    (if (atom l) l
	(mapcar (lambda (item)
		  (convert-duration-specs item))
		l))))


(defun metric-comparison-operator-p (x)
  (case x
    ((> < = /= >= <=) t)
    (otherwise nil)))

;;;
;;; Methods for checking constraints ....
;;;

;;; no constraints? Pass!
(defmethod value-constraints-met-p ((monitor monitor) (constraints NULL) (value T) bindings/s)
  (declare (ignore bindings/s))
  t)

;;; no value? Fail!
(defmethod value-constraints-met-p ((monitor monitor) (constraints list) (value NULL) bindings/s)
  (declare (ignore bindings/s))
  (if (cdr constraints) ;; proper list ...
    NIL
    T))

(defun contains-variable-p (form)
  (not (null (variables-in form))))

;;; 
(defmethod subject-to-variable-bound-precondition-p ((operator t)) t)


;;; value AND constraints? check each constraint!
;;; will fail if any variables in constraint -- not an error
(defmethod value-constraints-met-p ((monitor monitor) (constraints list) (value T) bindings/s)
  (let ((val  (substitute-bindings-stack value bindings/s)))
    (every (lambda (constraint)
	     (let ((form (convert-duration-specs 
			  (substitute-bindings-stack constraint bindings/s))))
	       (if (and 
		    (contains-variable-p form)
		    (or (not (consp form))
			(subject-to-variable-bound-precondition-p (car form))))
		 nil
		 (patmatch-constraint val form))))
	   constraints)))


;;; -- value constraints 
(defmethod add-monitor-constraint :after (type (monitor monitor) value)
  (pvs "Adding" type "constraint to" monitor value))

(defmethod add-monitor-constraint ((type (eql :value)) (monitor non-complex-monitor) value)
  (setf (monitor-value-constraints monitor)
    (append (monitor-value-constraints monitor)
	    (list value))))


;;;-- object 

(defmethod add-monitor-constraint ((type (eql :object)) (monitor non-complex-monitor) value)
  (setf (monitor-object-constraints monitor)
    (append (monitor-object-constraints monitor)
	    (list value))))


;;; --- ok, functions for determining success/failure:

(defun eval-timstamp-constraint-times (constraints bs)
  (mapcar  #'(lambda (constraint)
	       (patmatch-eval (convert-duration-specs (cadr constraint)) bs))
	   constraints))

(defun eval-constraints (constraints bs)
  (mapcar #'(lambda (constraint)
	      `(,(car constraint)
		,(patmatch-eval (convert-duration-specs (cadr constraint)) bs)))
	  constraints))



;;; ---
;;; Measurment/estimation monitors.
;;;
;;; These are measurments which are based on *estimation rules*. They inherit a lot from
;;; measurment monitors, but also require a separate method for estimating. The estimation
;;; methods are defined in estimation-methods.lisp
;;; ---

(defclass estimation-mixin ()
  ((estimation-parameters :initarg :estimation-parameters :initform NIL :accessor monitor-estimation-parameters)
   (estimation-type :initarg :estimation-type :initform :persist :accessor monitor-estimation-type)))


(defmethod add-monitor-constraint ((type (eql :estimation)) (monitor estimation-mixin) value)
  (assert (consp value) nil "Estimation definition must be a list")
  (declare-estimation-method (car value) monitor value))

(defmethod declare-estimation-method ((type T) (monitor monitor) parameters)
  (declare (ignore parameters))
  (error "~s estimation methods are not defined for ~a monitors in ~s" 
	 type 
	 (car (pattern monitor))
	 (pattern monitor)))

;; (:persist :with-timeout P10S)

(defun parameter-compare (mon type to-check gold-standard)
  (labels ((pc (params)
	     (if (null params) T
		 (let ((key (car params))
		       (val (cadr params))
		       (rest (cddr params)))
		   (assert (member key gold-standard :test #'eql) nil
		     "Invalid parameter key ~S with value ~S given to ~S in ~S"
		     key val type (pattern mon))
		   (assert (or (not (null val))
			       (and (null val) (not (null (cdr params)))))
		       nil
		     "Missing parameter value for key ~S  given to ~S in ~S"
		     key  type (pattern mon))
		   (pc rest)))))
    (pc to-check)))

(defmethod parameter-check ((type (eql :persist)) (monitor monitor) parameters)
  (parameter-compare monitor type parameters '(:with-timeout)))

(defmethod parameter-check ((type (eql :linear-regression)) (monitor monitor) parameters)
  (parameter-compare 
   monitor type parameters
   '(:minimum-points :maximum-error :maximum-difference :minimum-frequency :start :end)))

(defmethod parameter-require ((monitor monitor) (name symbol) parameters)
  (unless (member name parameters)
    (error "a ~s requires a ~s parameter"
	   (type-of monitor) name)))

(defmethod parameter-require ((monitor monitor) (names list) parameters)
  (unless (some (lambda (name) (member name parameters)) names)
    (error "a ~s requires a parameter from this list: ~a"
	   (type-of monitor) names)))

;;; ways to determine min/max values

;;; given a set of constraints ...
;;; ((> (+ (start-of +this-task+) P45s)) (<= (+ (start-of +this-task+) P50S)))
;;; that have been bound to their current values ...
;;; ((> 10000) (<= 200000))
;;; return an interval structure giving the minimum/maximum values 
;;; constraints: list(constraint)
;;; default-minimum: if no minimum provided, use this minimum
;;; default-left: if no minimum provided, use type of interval (:open or :closed)
;;; default-maximum: if no maximum provided, use this maximum
;;; default-right: if no maximum provided, use type of interval (:open or :closed)
;;; returns NIL if max < min. 

(defun close-time-interval (interval)
  (when interval
    (when (eq (interval-left interval) :open)
      (setf (interval-left interval) :closed)
      (when (/= (interval-start interval) +end-of-time+)
	(setf (interval-start interval)
	  (1+ (interval-start interval)))))
    (when (eq (interval-right interval) :open)
      (setf (interval-right interval) :closed)
      (when (/= (interval-end interval) +end-of-time+)
	(setf (interval-end interval)
	  (1- (interval-end interval))))))
  interval)

;; in cases where a form is bound after compile time, we need to check equality
;; if the variable has been bound. This comes up in complex monitors.
(defun %%eql-if-bound (x val/v)
  (if (variable-p val/v) t
      (eql x val/v)))

;; note -- we have to declare that this is not subject to the 'variable bound' precondition.
(defmethod subject-to-variable-bound-precondition-p ((operator (eql '%%eql-if-bound))) nil)


(defun determine-extreme-values (constraints default-minimum default-left default-maximum default-right)
  (let (mins maxes equals)
    (dolist (constraint constraints)
      (cond
       ((member (car constraint) '(> >=))
	(setq mins (cons constraint mins)))
       ((member (car constraint) '(< <=))
	(setq maxes (cons constraint maxes)))
       ((member (car constraint) '(= %%eql-if-bound))
	(setq equals (cons constraint equals)))
       ((eq (car constraint) t)) ;; do nothing
       (t (warn "Constraint ~s not used in determining extrema." constraint))))
    (cond 
     ((> (length equals) 1) ;; maybe they're all equal...warn if not
      (when (> (length (remove-duplicates equals :key #'cadr)) 1)
	(warn "More than one equality constraint provided. Using smallest: ~s" equals))
      (make-interval (cadr (car  (sort equals #'< :key #'cadr)))))
     ((= 1 (length equals))
      (when (or mins maxes)
	(when mins (warn "Ignoring minima constraints; equal constraint provided."))
	(when maxes (warn "Ignoring maxima constraints; equal constraint provided.")))
      (make-interval (cadr (car equals))))
     (t      
      (multiple-value-bind (min left)
	  (determine-mininum-value mins default-minimum default-left)
	(multiple-value-bind (max right)
	    (determine-maximum-value maxes default-maximum default-right)
	  (if (or (> max min)
		  (and (= min max)
		       (not (or (eq left :open)
				(eq right :open)))))
	    (make-interval min max left right)
	    (progn
	      ;; (warn "Minimum is greater than maximum: ~s ~s" mmin mmax)
	      nil))))))))

(defun constraint-order-< (a b)
  (let ((aval (cadr a))
	(bval (cadr b)))
  (cond 
   ((< aval bval) t)
   ((> aval bval) nil)
   (t ;; values are equal -- check lexographic order of operators
      ;; i.e., <= is 'less than' <.
    (let ((aop (car a))
	  (bop (car b)))
      (< (length (symbol-name aop))
	 (length (symbol-name bop))))))))

(defun constraint-order-> (a b)
  (let ((aval (cadr a))
	(bval (cadr b)))
  (cond 
   ((> aval bval) t)
   ((< aval bval) nil)
   (t ;; values are equal -- check lexographic order of operators
      ;; i.e., >= is 'less than' >.
    (let ((aop (car a))
	  (bop (car b)))
      (< (length (symbol-name aop))
	 (length (symbol-name bop))))))))

(defun determine-mininum-value (constraints default-minimum default-left)
  (cond
   ((null constraints)
    (values default-minimum default-left))
   ((= 1 (length constraints))
    (let ((op (car (car constraints)))
	  (val (cadr (car constraints))))
      (values val (if (eq op '>) :open :closed))))
   (t 
    ;; (warn "More than one minima constraint provided. Using largest: ~s" constraints)
    (let* ((constraints (sort constraints 'constraint-order->))
	   (constraint (car constraints))
	   (op (car constraint))
	   (val (cadr constraint)))
      (values val (if (eq op '>) :open :closed))))))
       
(defun determine-maximum-value (constraints default-maximum default-right)
  (cond
   ((null constraints)
    (values default-maximum default-right))
   ((= 1 (length constraints))
    (let ((op (car (car constraints)))
	  (val (cadr (car constraints))))
      (values val (if (eq op '<) :open :closed))))
   (t 
    ;; (warn "More than one maxima constraint provided. Using smallest: ~s" constraints)
    (let* ((constraints (sort constraints 'constraint-order-<))
	   (constraint (car constraints))
	   (op (car constraint))
	   (val (cadr constraint)))
      (values val (if (eq op '<) :open :closed))))))
  

;;; given a set of constraints and bindings 
;;; ((> ?x) (<= (+ (start-of +this-task+)  P34S))), ((x . 100))
;;; convert to a bound version 
;;; ((> 100) (<= 34000)) (say)
;;;
(defun evaluate-constraint (constraint bindings/s)
  (let ((op (substitute-bindings-stack (car constraint) bindings/s))
	(val (convert-duration-specs (substitute-bindings-stack (cadr constraint) bindings/s))))
    (if (variable-p val) `(t)
	`(,op ,(patmatch-eval val)))))

(defun evaluate-constraints (constraints bindings/s)
  (loop for constraint in constraints 
      collecting (evaluate-constraint constraint bindings/s)))

(defun non-comparative-constraints (constraints)
  (loop for constraint in constraints
      unless (and (consp constraint)
		(member (car constraint) '(%%eql-if-bound > < = <= >=)))
      collect constraint))

;;; Monitor-array support

(defmethod set-monitors ((agent agent) monitors)
  ;; agent * list(monitor) -> ()
  (setf (monitors (monitor-array agent)) monitors)
  (loop for monitor in monitors do
        (setf (containing-array monitor) (monitor-array agent)))
  (values))

(defmethod clear ((ma monitor-array))
  (setf (monitors ma) nil))

(defmethod monitors ((a agent))         ; agent -> list(monitor)
  (monitors (monitor-array a)))

(defmethod init-monitor-array ((a agent))
  (setf (monitors (monitor-array a)) nil))


;;; is this event type relevant?
;;;

(defmethod immediately-relevant-p ((monitor monitor) (event-type symbol))
  (and (member event-type (relevant-types monitor)
	       :test #'eq)
       t))

;;; ! these two methods seem like they should be equivalent, but
;;;   they're not.

(defmethod relevant-event-type-p ((monitor monitor) (event-type symbol))
  (or (immediately-relevant-p monitor event-type)
      (and (parent-monitor monitor)
	   (relevant-event-type-p (parent-monitor monitor) event-type))))
       
(defmethod relevant-to-self-or-parent-p ((monitor monitor) (event-type symbol))
  (or (relevant-event-type-p monitor event-type)
      (and (parent-monitor monitor)
	   (relevant-event-type-p (parent-monitor monitor) event-type))))
;;;
;;; wind-down: override/call-next-method this method to add wind down actions
;;; see measurements, which require probes to wind down.

(defmethod wind-down ((monitor monitor))
  (values))

;;; probe monitors

(defclass probe-mixin ()
  ((probes :initarg :probes :initform '() :accessor monitor-probes)))

;;; :probing (<action> <frequency> [:time-limit <tl> :count-limit <cl>])
(defmethod add-monitor-constraint ((type (eql :probing)) (monitor probe-mixin) value)
  (unless (>= (length value) 2)
    (error "Probing must have (<action> <duration>) specifier: ~a" value))
  (let ((expr (expr monitor)))
    (let ((attr (measurement-form-attribute expr))
	  (obj (measurement-form-object expr)))
      (when (variable-p obj)
	(error "Cannot probe unless object is bound in state variable (~s ~s)" attr obj))
      (let* ((sv (make-state-variable attr obj))
	     (probe (apply 'make-probe  sv (task monitor) value)))
	(setf (monitor-probes monitor)
	  (append (monitor-probes monitor)
		  (list probe)))
	(start-sv-logging-policy 
	 (task-memory (task monitor))
	 sv
	 :count-limit +pos-infinity+)
	(start-probe probe)))))

(defmethod wind-down :around ((monitor probe-mixin))
  ;; (format t "~%;;;~%;;;~%;;; Winding down ~a~%;;;~%;;;~%;;;%" monitor)
  (dolist (probe (monitor-probes monitor))
    (stop-sv-logging-policy 
     (task-memory (task monitor))
     (probe-sv probe))
    (stop-probe probe))
  (call-next-method))

;;; this makes it a bit easier to record monitor status for non-complex
;;; monitors.
(defmacro signal-recording-pipe-result (mon &body body)
  (let ((var (gensym)))
    `(let ((,var (progn ,@body)))
       (if (eq ,var empty-pipe)
	 (unless (eq (state (monitor-status ,mon))
		     :partially-satisfied)
	   (set-monitor-status ,mon :unsatisfied nil nil))
	 (let ((head (pipe-head ,var)))
	   (set-monitor-status ,mon :satisfied (binding-set head) (interval head))))
       ,var)))

(defmethod signal-monitor ((monitor non-complex-monitor) (task task) cogevent &optional (added-bindings no-bindings))
  (signal-recording-pipe-result monitor (signal-monitor/1 monitor task cogevent added-bindings)))

;;;
;;; MONITOR TRACING
;;;

(defvar *trace-monitor-patterns* (list)
  "List of monitor patterns to show results for")

(defmacro untrace-monitor (&optional form)
  (if form
    `(setq *trace-monitor-patterns* (remove-if #'(lambda (x)
					     (tree-equal x ',form))))
    `(progn
       (setq *trace-monitor-patterns* (list))
       (funwrap 'signal-monitor 'trace-monitor))))


(defmacro trace-monitor (form &optional (successful-matches-only-p t))
  `(unless (member ',form *trace-monitor-patterns* :test #'tree-equal)
     (push ',form *trace-monitor-patterns*)
     (funwrap 'signal-monitor 'trace-monitor)
     (def-fwrapper signal-monitor-wrap (monitor task cogevent &optional bindings)
       (let ((pipe (call-next-fwrapper)))
	 (when (and (typep cogevent 'cogevent)
		    (or (not ',successful-matches-only-p)
			(some #'(lambda (f)
				  (pat-match f (content cogevent)))
			      *trace-monitor-patterns*)))
	   
	   (format t ";;; Signal-monitor ~s to ~%;;; ~a; ~%;;; first result: ~s~%"
		   (content cogevent) monitor (pipe-head pipe)))
	 pipe))
     (fwrap 'signal-monitor 'trace-monitor 'signal-monitor-wrap)))

;;; app-ob interface

(defmethod cl-user::appob-parent :around ((monitor monitor))
  (or (parent-monitor monitor)
      (call-next-method)))

(defmethod cl-user::appob-children ((monitor monitor))
  (submonitors monitor))

(defun cl-user::monitor-agent (m)
  (cl-user::agent (task m)))

;;; note: it is possible for a monitor to be satisfied on creation,
;;; so before adding it to a task's monitor slot, be sure to check
;;; its 'val' slot isn't t. (see GEN-WAITSTRUCS).
(defun cl-user::make-monitor (expr task &key val type)
  (let ((monitor (compile-monitor expr)))
    ;; more bookkeeping
    (setf (task monitor) task)
    (setf (val monitor) val)
    (setf (monitor-type monitor) (or type (car expr)))
    
    ;; add as a subscriber to its specific types
    (if (member :any (relevant-types monitor))
      (add-general-subscriber monitor (type-router (agent task)))
      (dolist (rtype (relevant-types monitor))
	(add-subscriber monitor rtype (type-router (agent task)))))
    
    ;; see if there is a success at creation time.
    (with-task (parent task)
      (let ((result-pipe (signal-monitor monitor task :creation)))
	(when (not (eq result-pipe empty-pipe))
	  (process-satisfied-monitor task monitor (pipe-head result-pipe) :creation))))
    monitor))

