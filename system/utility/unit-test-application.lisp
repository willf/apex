;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/unit-test-application.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: unit-test-application.lisp,v 1.6 2006/01/15 03:43:03 dalal Exp $

(in-package :cl-user)
(defvar *hide-trace* NIL "Should we hide the trace output?")

(defun ensure-ends-in-lisp (pathnamestring)
  (if (or (string-equal ".lisp"
		    (subseq pathnamestring
			    (max 0 (- (length pathnamestring) 5))
			    (max 0 (length pathnamestring))))
	  (find #\. pathnamestring :test #'char=))
    pathnamestring
    (concatenate 'string pathnamestring ".lisp")))

(defmacro unit-test-application (namestring pathname &key (timeout (* 8 60))
							  (test-agents nil)
							  (hide-trace *hide-trace*))
  "namestring: Name to give the unit test (eg., 'Application: Roshambo')
   pathname: pathname to application (eg. 'Apex:examples;roshambo'
   timeout: how long to wait before giving up (in seconds)
   test-agents: test *all-agents* to see if their initial-task is terminated
   hide-trace: whether to hid the runtime trace."
  `(apex.utility.unit-test:with-tests (:name ,namestring)
     (apex.utility.unit-test:test-no-error
      (without-redefinition-warnings
       (load-application-file (ensure-ends-in-lisp ,pathname))))
     
     (apex.utility.unit-test:test-with-timeout 
      (,timeout) 
      (progn (when ,hide-trace (unshow :runtime)) (startapp)))
     
     (apex.utility.unit-test:test-no-error
      (verify-hierarchy *application*))
     
     ;; (format t "State of application: ~a~%" (state *application*))
     
     (apex.utility.unit-test:test 'done (state *application*))

     (when ,test-agents
       (loop for agent in *all-agents*
	 do
	   (apex.utility.unit-test:test 
	    'terminated
	    (state (initial-task (agenda agent))))))))