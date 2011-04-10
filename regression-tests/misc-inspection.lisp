;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/misc-inspection.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-inspection.lisp,v 1.5 2006/01/15 03:42:59 dalal Exp $
;;; Description: Regression test for inspection utilities

;;; Construct a diamond shaped class hierarchy.  Slots are named by
;;; letters.  Vowels are suppressed.

(defclass foo (id-mixin)
  ((a
    :initform 'a
    :reader a)
   (b
    :initform 'b
    :reader b)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(a))))

(defclass bar (id-mixin)
  ((c
    :initform 'c
    :reader c)
   (d
    :initform 'd
    :reader d)))

(defclass baz (foo bar)
  ((e
    :initform 'a
    :reader a)
   (f
    :initform 'b
    :reader b)
   (suppressed-slots
    :reader suppressed-slots
    :allocation :class
    :initform '(e))))

(defun set-equal (a b)
  (and (null (set-difference a b))
       (null (set-difference b a))))

(apex.utility.unit-test:with-tests (:name "Suppressed Slots")
  (apex.utility.unit-test:test 
   '(id d c b f) (accumulate-terse-slots (find-class 'baz))
   :test 'set-equal)
  (apex.utility.unit-test:test
   '(id c d) (accumulate-terse-slots (find-class 'bar))
   :test 'set-equal)
  (apex.utility.unit-test:test
   '(id b) (accumulate-terse-slots (find-class 'foo))
   :test 'set-equal))
