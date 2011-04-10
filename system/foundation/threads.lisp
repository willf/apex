;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/threads.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: threads.lisp,v 1.8 2006/01/15 03:43:01 dalal Exp $

;;; Thread (multi-processing) interface for Apex

(in-package :user)

;;; First, some output related stuff pulled out of the Sherpa system.
;;; This stuff is a candidate for further refactoring and separate
;;; filing.

;;; Destination of trace (and general) output.  Legal values are
;;; currently 'listener and 'sherpa
(defvar *trace-destination* 'listener)

;;; Convenient wrapper to insure trace goes to listener.
(defmacro with-readable-trace (&rest forms)
  `(let ((*trace-destination* 'listener))
     ,@forms))


;;; A Stream object for foreign applications, currently used only by Sherpa.

(defvar *socket-stream* nil)


;;; Evaluate forms with standard output conditionally redirected to
;;; either listener or sherpa.

(defmacro with-output-directed (&body forms)
  `(let ((*standard-output* 
          (if (eq *trace-destination* 'sherpa)
              *socket-stream*
            *standard-output*))
         (*print-pretty* nil))
     ,@forms
     (force-output *standard-output*)))

(defvar *thread-count* 0) ; counter used by several functions

;;; Association list (string, list) mapping names of currently active
;;; threads to their lisp forms.  Useful for debugging (and more useful
;;; than :proc in some cases)
;;;
(defvar *threads* '())                  

;;; Generate a unique thread name

(defparameter *thread-prefix* "apex-thread-")

(defun thread-name ()
  (incf *thread-count*)
  (format nil "~a~a" *thread-prefix* *thread-count*))

(defmethod apex-process? ((p mp:process))
  (string= *thread-prefix* (subseq (mp:process-name p) 0
                                   (length *thread-prefix*))))

(defun reset-threads ()
  (mapc (lambda (p)
          (if (apex-process? p)
              (mp:process-kill p)))
        mp:*all-processes*)
  (setq *threads* nil)
  (setq *thread-count* 0))


;;; Evaluate the given forms in a new thread.
;;;
(defmacro thread (description &rest forms)  ; list * LispForm* -> any
  `(let ((name (thread-name)))
     (push (cons name ,description) *threads*)
     (mp:process-run-function
         (list :name name)
        #'(lambda ()
            (unwind-protect
                (with-output-directed
                    (progn ,@forms))
              (setq *threads*
                (remove-if #'(lambda (pair) (equal name (car pair)))
                           *threads*)))))))

