;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/hierarchy.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: hierarchy.lisp,v 1.8 2006/01/15 03:43:01 dalal Exp $

;;; Support for building and querying the Apex object hierarchy.
;;; See apex/notes/specs/hierarchy.text.

(in-package :user)

;;; --------------------  Children and descendants

(defmethod component-slots ((x appob))
  ;;
  ;; appob -> list(symbol)
  ;; Generic function covering all types.  Classes of interest will have
  ;; an accessor named component-slots
  ;;  
  nil)

(defmethod content-slots ((x appob))
  ;;
  ;; appob -> list(symbol)
  ;; Generic function covering all types.  Classes of interest will have
  ;; an accessor named content-slots
  ;;  
  nil)

(defmethod appob-components ((x appob))  ; appob -> list(appob)
  (collect-slot-values x (component-slots x)))

(defmethod appob-contents ((x appob))  ; appob -> list(appob)
  (collect-slot-values x (content-slots x)))

(defmethod appob-children ((x appob))  ; appob -> list(appob)
  (append (appob-components x) (appob-contents x)))

(defun collect-slot-values (instance slot-names)
  ;;
  ;; clos-instance * list(symbol) -> list
  ;; Given a class instance and a list of slot names, compute a flat
  ;; list of the union of all the slot values.
  ;;  
  (remove nil
          (flatten
           (mapcar
             #'(lambda (slot-name)        ; symbol -> list
                 (slot-value instance slot-name))
             slot-names))))


(defun get-all-descendants (x &optional (remove-duplicates? t))
  ;;
  ;; appob * opt(bool) -> list(appob)
  ;; Generate a flat list of all descendants of a given object.  The
  ;; remove-duplicates? parameter is provided purely as an efficiency
  ;; mechanism when this is used an an auxiliary function.
  ;;
  (if (or (component-slots x) (content-slots x))
      (let* ((children (appob-children x))
             (descendants 
              (append
               children
               (remove nil
                       (flatten
                        (mapcar
                         #'(lambda (child)
                             (get-all-descendants child remove-duplicates?))
                         children))))))
        (if remove-duplicates?
            (remove-duplicates descendants
              :test #'(lambda (a b) (eq (id a) (id b))))
          descendants))))

(defun get-all-descendant-ids (x)
  ;;
  ;; appob -> list(symbol)
  ;; Generate a flat list of the object ids of all descendants of given
  ;; object.
  ;;  
  (remove-duplicates
   (mapcar #'id (get-all-descendants x nil))))


(defun compute-trace-constraint-for-descendants (x)
  ;;
  ;; any -> list
  ;; Generate a trace constraint (filter) for all descendants of given
  ;; object.
  ;;
  (cons 'or
        (mapcar #'(lambda (id) (list 'object-id id))
                (get-all-descendant-ids x))))


;;; --------------------  Parents and ancestors

(defmethod parent-slot ((x appob))
  ;;
  ;; appob -> symbol
  ;; Generic function covering all types.  Classes of interest will have
  ;; an accessor named parent-slot
  ;;  
  nil)

;;; The singleton Application is the root of the tree
(defmethod appob-parent ((app application)) nil)
  
(defmethod appob-parent ((x appob))     ; appob -> list(appob)
  (cond ((and (parent-slot x)
              (slot-value x (parent-slot x)))
         (slot-value x (parent-slot x)))
        ((locale x) (locale x))
        (t *application*)))

(defun ancestors (x)  ; any -> list(any)
  ;; Return the chain of ancestors starting with immediate parent and
  ;; leading to root of tree
  (let ((p (appob-parent x)))
    (if p (cons p (ancestors p)))))

(defun ancestor-ids (x)  ; any -> list(symbol)
  (mapcar #'id (ancestors x)))


;;; Testing

(defmethod verify-hierarchy ((x appob)) ; -> opt(error)
  (let ((children (appob-children x))
        (errors nil))
    (loop for c in children do
          (if (not (eq (appob-parent c) x))
              (push (cons c x) errors))
          (verify-hierarchy c))
    (when errors
      (loop for e in errors do
            (format t "Hierarchy Error: ~a didn't have ~a as parent.~%"
                    (car e) (cdr e)))
      (error "Hierarchy errors -- see printout above"))))
