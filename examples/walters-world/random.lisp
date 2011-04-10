;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/random.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: random.lisp,v 1.3 2006/01/15 03:42:57 dalal Exp $

;; random.lisp

;; set-random-state
;;
;; Creates a new random state and saves the object to a file
;; specified by filename, or to a file called randomState if
;; no filename is provided.
;;
;; Usage: (set-random-state "my-file")

(defun set-random-state (filename)
  (let*
      ((path (make-pathname 
              :name filename))
       (str (open path
                  :direction :output
                  :if-exists :supersede))
       (rs (make-random-state t)))
    (print rs str)
    (close str)))

;(defun set-random-state ()
;  (setf path (make-pathname :name "randomState"))
;  (setf str (open path
;                  :direction :output
;                  :if-exists :supersede))
;  (setf rs (make-random-state t))
;  (print rs str)
;  (close str))

;;; get-random-state
;;;
;;; Retrieves the random state from file, then saves the random state to
;;; the global variable *random-state*. This value is used by default as
;;; the seed for the random function.
;;;
;;; Usage: (get-random-state)

(defun get-random-state ()
  (with-open-file (stream (make-pathname :directory (application-directory)
                                         :name "randomState")
                   :direction :input
                   :if-does-not-exist :create)
    (setf *random-state* (read stream))))




 