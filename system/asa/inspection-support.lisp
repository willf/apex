;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/asa/inspection-support.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: inspection-support.lisp,v 1.9 2006/01/15 03:43:00 dalal Exp $


;;; support functions for creating the object tree

(in-package :user)

(defun all-vobs () ; -> List(Any)
  ;;
  ;; Generates a hierarchical list of a simworld's objects.  Note that
  ;; "human objects" are treated differently from nonhuman objects.
  ;;  
  (if *all-agents*
      (append
       (remove-duplicates (mapcar #'locale *all-agents*))
       (mapcar #'human-obs *all-agents*)
       (if (typep *application* 'native-sim-application)
           (decomposition-of-nonhuman-objects))
       ))
  )

(defun decomposition-of-nonhuman-objects () ; -> List(Any)
  (mapcar #'decompose-appob (nonhuman-toplevel-simworld-objects)))

(defmethod decompose-appob ((x appob))  ; Appob -> Appob + List(Any)
  (let ((cs (if (typep x 'physob) (components x))))
    (if cs
        (cons x
              (list (cons 'components (mapcar #'decompose-appob cs))))
      x)))

(defun nonhuman-toplevel-simworld-objects () ; -> List(Appob)
  (loop for item in (toplevel-simworld-objects)
      when (not (eq 'human (type-of item)))
      collect item))

(defun toplevel-simworld-objects ()     ; -> List(Appob)
  (loop for item in (reduce #'union 
                           (mapcar #'(lambda (loc) (if loc (vfield loc)))
                                   (mapcar #'locale *all-agents*)))
      when (null (component-of item))
      collect item))

(defun human-obs (agent)
  (let ((d)
        (alis)
        (alis-term)
        (wlis)
        (wlis-term))
    (dolist (i (reverse (tasks agent)))
      (setf d (specify-task-description i))
      (cond ((member (first d) '(terminate reset resumed) :test (function eq))
             (if (equal (type-of (second d)) 'task)
               (push (second d) alis-term)
               (push i alis-term)))
            ((equal (first d) 'start-activity)
             (push i alis))
            (t
             (push i alis))))
    (dolist (e (reverse (monitors agent)))
     (let ((i (expr e)))
      (cond ((member (first i) '(terminated reset resumed) :test #'eq)
             (if (equal (type-of (second i)) 'task)
               (push (second i) wlis-term)
               (push i wlis-term)))
            ((equal (first i) 'start-activity)
             (push i alis))
            (t (push e wlis)))))
    (remove nil
            (list agent
                  (let ((resources (components agent)))
                    (if resources 
                        (cons 'resources resources)
                      nil))
                  (cons 'agenda (append 
                                 (if alis-term
                                     (list (cons 'terminate alis-term))) 
                                 alis))
                  (if (or wlis wlis-term) 
                      (cons 'monitor-array
                            (append
                             (if wlis-term
                                 (list (cons 'terminated wlis-term)))
                             wlis)))))))


(defun find-vob (x)
  (if *agent*
  (dolist (i (append (vfield (locale *agent*))))
    (if (equal x (id i))
        (return i)))))
