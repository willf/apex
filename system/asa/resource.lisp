;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/resource.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: resource.lisp,v 1.32 2006/03/16 15:57:48 will Exp $


;;; Resource class and support.

(in-package :user)

(defclass resource (appob assertion-mixin)
  (;; agent that controls resource
   (component-of
    :accessor component-of
    :initarg :component-of)
   (parent-slot
    :allocation :class
    :reader parent-slot
    :initform 'component-of)
   ;; task that invoked resource's current activity
   (task :reader task
         :initarg :task)))

(defmethod containing-object ((x resource)) ; -> Agent
  (component-of x))

;;; --- Add-apex-resource (old)
;;;
;;; Causes a resource (already initialized) to be added as a component
;;; and functional-relation to an agent.  If an alias is supplied, it
;;; will used as an alternative functional relation name for the
;;; resource.  e.g. dominant-hand can be an alias for right-hand.

(defmethod add-apex-resource ((r resource) (h agent) &key alias)
  (setf (slot-value h (type-of r)) r)
  (push r (components h))
  (setf (component-of r) h)
  (push (cons (type-of r) r) (func-relations h))
  (if alias (push (cons alias r) (func-relations h))))

;;; New: this should obsolete the previous eventually
;
(defmethod add-resource ((a agent) (r resource))   ; -> ()
  (let ((rs (resource-set a)))
    (push r (resources rs))
    (setf (component-of r) rs)
    (values)))

(defmethod add-resource-to-agent-if-necessary ((resource-name symbol)
                                               (agent agent))
  (when (not (member resource-name (resource-names agent)))
    (let ((resource-class (ignore-errors (find-class resource-name))))
      ;; !- need to check if the class found is a resource class
      (add-resource agent (make-instance
                              (if resource-class resource-name 'resource)
                            :name resource-name)))))

(defmethod add-resource-to-agent-if-necessary ((resource resource)
                                               (agent agent))
  (if (not (member resource (resources agent)))
      (add-resource agent resource)))

(defmethod agent-of ((r resource))
  (component-of r))


;;; -------------------------------------------------------------------------
;;; Resource activites
;;; -------------------------------------------------------------------------

;;; The ASA controls resources by starting activities (a simengine action) in
;;; which resources are participating entities.  Resource-activities are normal
;;; activities but maintain a pointer to the ASA task that started them and are
;;; mutually exclusive with other activities on the same resource --
;;; i.e. starting a new activity involving a given resource causes any other
;;; activity it is involved in to be stopped.  (Note: stopping an activity is
;;; not the same as completing it)

;;; ! task slot is same as cause slot for purposes of trace 
(defclass resource-activity (activity)
  ((task :accessor task
         :initarg :task
         :initform nil)
   (resources :accessor resources
	     :initarg :resources
	     :initform nil)
   ))


(defmethod update-activity ((ra resource-activity) ignored-object)
  ;;
  ;; ! Second argument is untyped to allow NIL: instances of
  ;; resource-activity do not have primary objects.  This second
  ;; argument, and primary objects, should be obsoleted.
  ;;
  (declare (ignore ignored-object))
  (when (update-action ra)
    (funcall (update-action ra))
    (setf (update-time ra)
      (if (update-interval ra)          ; schedule-completion cancels updates
          (+ (current-time) (activity-interval (update-interval ra) ra))
        nil)))
  ;;  (if (not (complete? ra)) (schedule-activity ra *application*)))
  (values))
  
                            
(defmethod print-object ((ob resource-activity) s)
  (if (task ob)
    (format s "#{~a ~a}"
            (if *hide-object-numbers* (type-of ob) (id ob))
            (specify-task-description (task ob)))
    (format s "#{~a}" (id ob))))

;;; --- Start-resource-activity
;;;
;;; Handles commands from ASA to perform an action.  Essentially just a call
;;; to the simengine function START-ACTIVITY but also replaces an agent's 
;;; internal names for appobs (unique-names, visobfiles,..) with pointers to 
;;; the appob instances.


(defun start-resource-activity (task agent args &optional resources)
  (let ((new-activity 
	 (apply #'start-activity
		(replace-local-names (append args (list :task task)) agent))))
    (setf (resources new-activity) resources)
    (cogevent `(started ,new-activity) agent)
    (setf (action task) new-activity)
    new-activity))


(defmethod start-resource-activity-from-primitive ((task task) timestamp
                                                   &optional resources)
  ;; task * int * List of resource-name -> ()
  ;;
  ;; This is the key function for primitive execution.  ! It's quite
  ;; messy and needs a rewrite.  EVAL should be avoided, the
  ;; funcall/thunk/eval-vas mechanisms refactored or streamlined.
  ;;  
  (record-state-transition task 'ongoing timestamp)
  (labels
      ((eval-vars (exp)
         (replace-vars exp (vars task) (task-globals task) :quote t
				       :unquote-numbers t))
       (initialize-attributes (attrs)   ; alist -> alist
         (mapcar #'(lambda (pair)
                     (cons (car pair) (eval (eval-vars (cdr pair)))))
                     attrs)))

    ;; In the task bindings' values, replace any symbols that
    ;; name a "known" object with that object...
    (mapcar #'(lambda (binding)
                (setf (binding-val binding)
                  (replace-local-names (binding-val binding) (agent task))))
            (vars task))
    (let ((primitive (proc task)))
      ;; Set the attributes of the task from the primitive
      (setf (attributes task) (initialize-attributes
                               (attributes primitive)))
      (let* ((update-interval (if (update-interval primitive)
                                  (duration-read 
                                   (eval-vars (update-interval primitive)))))
             (update-time (if update-interval (+ timestamp update-interval)))
             (duration
              (if (duration primitive)
                  (let ((exp (eval-vars (duration primitive))))
                    (cond ((time-expression? exp) (duration-read exp))
                          (t (let ((exp1 (eval exp)))
                               (cond ((time-expression? exp1)
                                      (duration-read exp1))
                                     (t (error "~a~a~a~a~%"
                                               "duration clause of primitive "
                                               primitive
                                               "is not a valid time expression: "
                                               exp1)))))))))
             (completion-time
              (cond (duration (+ timestamp duration))
                    ((or (update-action primitive)
                         (update-interval primitive))
                     nil)
                    (t timestamp)))
             (activity (make-instance 'resource-activity
                         :task task
                         :start-time timestamp
                         :update-interval update-interval
                         :update-time update-time
                         :completion-time completion-time))
             (returner (if (returnval primitive)
                           (make-attribute-thunk
                            task
                            (eval (eval-vars (returnval primitive))))
                         (make-thunk activity))))
        (if (completion-action primitive)
            (setf (completion-action activity)
              (make-attribute-thunk
               task
               (progn
                 (eval (eval-vars (completion-action primitive)))
                 (transition-task task 'terminated 'success (funcall returner) 'soft))))
          (setf (completion-action activity)
            (make-attribute-thunk
             task
             (transition-task task 'terminated 'success (funcall returner) 'soft))))
        (if (update-action primitive)
            (setf (update-action activity)
              (make-attribute-thunk
               task
               (eval (eval-vars
                      (process-primitive-special-forms
                       (update-action primitive)))))))
        (setf (action task) activity)
        ;; Invoke the start action
        (funcall
         (make-attribute-thunk task
                               (eval (eval-vars
                                      (process-primitive-special-forms
                                       (start-action primitive))))))
        ;; Begin the activity processing
        (if (and (not (complete? activity))
                 (or update-time
                     (and completion-time (not (= completion-time timestamp)))))
            ;;
            ;; Schedule the activity if it is not already complete (this
            ;; could happen via the start action) and it has an update or
            ;; completion time (it would have neither if the primitive had
            ;; no duration or update clause, i.e. a non-durative task)
            ;;
            (progn
	      (schedule-activity activity *application*))
          ;;
          ;; Otherwise, complete the activity (which terminates the task
          ;; -- see completion-action code above)
          
	    (complete-activity activity nil))
	(setf (resources activity) resources)
      activity))))

(defun process-primitive-special-forms (exp)  ; list -> list
  ;; Expand the special "(complete)" form.
  (sublis '(((complete) . (setf (complete? (action +this-task+)) t)))
          exp
          :test #'equal))

;;; --- Stop-resource-activity
;;;
;;; Handles command from ASA to stopm an ongoing resource activity.

(defun stop-resource-activity (activity agent)
  (stop-activity activity)
  (cogevent `(stopped ,activity) agent))


;;; --- Replace-local-names
;;;
;;; Start-activity parameters can refer to data-structures (objects) that 
;;; must be identified and substituted for the reference for the specified
;;; acitvity to be carried out.  This function performs those substitutions.
;;; Reference types include: (1) resource-names (e.g. the symbol left-hand),
;;; (2) visobfiles -- i.e. visual memory representations of "real" objects,
;;; (3) uniquenames (e.g. the symbol my-coffee-cup).

;;; ! need to support more sophisticated name-resolution of resources
;;; specified in a start-activity call, including e.g. designations such as
;;; dominant-hand, anyhand, freehandname.
;;; ! not necessarily desirable to swap out visobfiles in all cases.
;;; ! may want to handle unique-name resolultion for objects that have not
;;; yet been seen.  This entails a long-term memory model.

(defmethod replace-local-names (expr (agent agent))
  (cond ((null expr) nil)
	((consp expr)
	 (mapcar #'(lambda (e) (replace-local-names e agent)) expr))
	((keywordp expr) expr)
	((symbolp expr)
	 (or 
	  (cdr (assoc expr (func-relations agent))) ;; substitute in resource
	  (lookup-unique-name expr agent) ;; map unique name to visob
	  expr)) ;; no substitution
	(t expr)))

(defmethod lookup-unique-name (expr (a agent))
  ;;
  ;; any, Agent -> bool
  ;; This method is useful only for the human class, which specializes it.
  ;;
  (declare (ignore expr))
  nil)

;;; --- Initialize-activity :before 
;;;

;;; !- this method handles the case of the second argument being nil
(defmethod initialize-activity ((act resource-activity) x)
  (declare (ignore x)))

  
(defmethod initialize-activity :before ((act resource-activity) (ob appob))
  (let* ((resource (primary-object act))
	 (old-activity 
	  (find-if 
	   #'(lambda (a) (and (typep a 'resource-activity) 
			      (not (equal a act))))
	   (activities resource))))
    (when old-activity
      ;; (update-pert old-activity (agent-of resource))
      (stop-activity old-activity)
      (cogevent `(clobbered ,old-activity :by ,act) 
		(agent-of resource)))))

;;; --- Complete-activity :after
;;;
;;; generate a cogevent to indicate that the primitive task that started that
;;; activity has completed.

(defmethod complete-activity :after ((act resource-activity) obj)
  (declare (ignore obj))
  (cogevent `(completed ,act)
            (agent (task act))
            :trigger-asa t))
