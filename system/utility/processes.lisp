;;;-*- Mode: Lisp; Package: inet.util.process -*-
;;;
;;; apex/system/utility/processes.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: processes.lisp,v 1.4 2006/01/15 03:43:03 dalal Exp $
;;; Created:     10 July 1996
;;; Author:      John Wiseman
;; 
;; Description: Interface to MCL process functions.
;;		Based on Alain Roy's portable process interface.
;; 
;; Changes:
;;
;; waf June 2004  - Added GATE functions, allegro initial-form
;; mrh 10/31/2002 - Fixed some calls for CMUCL.
;; mrh 10/09/2002 - Added proc.interrupt, which forces the given
;;                  process the evaluate the given function and
;;                  then continue operations.
;; rjf 09/24/2002 - Moved into the inet.util.process package.  Moved
;;                  package definition to this file.
;; 
;;----------------------------------------------------------------------

#|
Copyright (c) 2002, I/NET, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met: 

- Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

- Neither the name of the I/NET, Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(defpackage :inet.util.process
  (:use :common-lisp)
  (:export 
   #:proc.run-function #:proc.run-with-error-handler #:proc.current 
   #:proc.kill #:proc.name #:proc.wait #:proc.wait-with-timeout 
   #:proc.wait-for-input-available #:proc.get-all-processes
   #:proc.yield #:proc.active? #:proc.start #:proc.stop #:proc.destroy 
   #:proc.make-lock #:proc.get-lock #:proc.giveup-lock #:proc.with-lock 
   #:proc.make-gate #:proc.gate-open-p #:proc.open-gate #:proc.close-gate
   #:proc.make-queue #:proc.enqueue #:proc.dequeue #:proc.with-queue 
   #:proc.queue-owner #:proc.make-process #:proc.preset 
   #:proc.preset-with-error-handler #:proc.initial-form
   #:proc.interrupt #:proc.rename)
  )

(in-package :inet.util.process)

(eval-when (:compile-top-level :load-top-level :execute)
  #+allegro(require :process)
  )

;;------------------------------
;; Processes
;;------------------------------

(defun proc.make-process (name &rest function-args)
  "Creates a new process (but does not start it)."
  #+:mcl (apply #'ccl:make-process name function-args)
  #+cmu (mp:make-process nil :name name)
  #+allegro (apply #'mp:make-process
		   :name name
		   :reset-action T
		   function-args)
  #+:lispworks (apply #'mp:create-process name function-args))

(defun proc.preset (process function &rest function-args)
  "Sets the initial function of PROCESS to FUNCTION.  The process
   will apply FUNCTION to FUNCTION-ARGS when it starts."
  #+:mcl (apply #'ccl:process-preset process function function-args)
  #+cmu (apply #'mp:process-preset process function function-args)
  #+allegro (apply #'mp:process-preset process function function-args)
  #+:lispworks (apply #'mp::process-preset process function function-args))

(defun proc.preset-with-error-handler (process handler function &rest function-args)
  "Sets the initial function of PROCESS to FUNCTION. If the function errors,
the error handler, a function of one argument (the error object) is run."
  (proc.preset process
	       #'(lambda ()
			 (handler-case (apply function function-args)
			   (condition (err) (funcall handler err))))))


(defun proc.run-function (name function &rest function-args)
  "Creates a process, gives it an initial function and starts it."
  #+:mcl (apply #'ccl:process-run-function name function function-args)
  #+cmu (mp:make-process (lambda () (apply function function-args))
			 :name name)
  ; #+cmu (apply #'mp:process-run-function name nil function function-args)
  #+allegro (apply #'mp:process-run-function name function function-args)
  #+:lispworks (apply #'mp:process-run-function name nil function function-args))

(defun proc.run-with-error-handler (name handler function &rest function-args)
  "Creates a process, gives it an initial function and starts it. If the function errors,
the error handler, a function of one argument (the error object) is run."
  (proc.run-function name
		     #'(lambda ()
			 (handler-case (apply function function-args)
			   (condition (err) (funcall handler err))))))

;; This one seems a little bogus.

(defun proc.initial-form (process)
  "Returns a list whose car is PROCESS' initial function and whose
   cdr is the arguments supplied to the function."
  #+:mcl (ccl:process-initial-form process)
  #+:lispworks (list (mp::process-function process)
                     (mp::process-arguments process))
  #+allegro (mp:process-initial-form process))


(defun proc.current ()
  "Returns the current process."
  #+:mcl ccl:*current-process*
  #+cmu (mp:current-process)
  #+allegro sys:*current-process*
  #+:lispworks mp:*current-process*)


(defun proc.kill (process)
  "Kills a process."
  #+:mcl (ccl::process-kill process)
  #+cmu (mp:destroy-process process)
  #+allegro (mp:process-kill process)
  #+:lispworks (mp:process-kill process))


(defun proc.name (process)
  "Given a process, returns the name of the process (that we provided
   to proc.run-function or proc.make-process)."
  #+:mcl (ccl:process-name process)
  #+cmu (mp:process-name process)
  #+allegro (mp:process-name process)
  #+:lispworks (mp:process-name process))

(defun proc.rename (process name)
  "Given a process, sets the name of the process to be the new desired name."
  #+:mcl (warn "proc.rename not implemented under MCL")
  #+cmu  (setf (mp:process-name process) name)
  #+allegro (setf (mp:process-name process) name)
;;  #+:lispworks (setf (mp:process-name process) name))
  #+:lispworks (warn "proc.rename not implemented under Lispworks"))


(defun proc.wait (whostate function &rest args)
  "Blocks the current process until FUNCTION (called with arguments ARGS)
   returns non-NIL."
;  #+:mcl (apply #'process-wait whostate function args)
  ;; Allow proc.waits to be nested... not quite as efficient, I fear.
  #+:mcl (declare (ignore whostate))
  #+:mcl (do ()
             ((apply function args) (values))
           (process-allow-schedule))
  #+cmu (mp:process-wait whostate #'(lambda () (apply function args)))
  #+allegro (apply #'mp:process-wait whostate function args)
  #+:lispworks (apply #'mp:process-wait whostate function args))

(defun proc.wait-with-timeout (whostate seconds function &rest args)
  "Blocks the current process until FUNCTION (called with arguments ARGS)
   returns non-NIL."
;  #+:mcl (apply #'process-wait whostate function args)
  ;; Allow proc.waits to be nested... not quite as efficient, I fear.
  #+:mcl (declare (ignore whostate))
  #+:mcl (do ()
             ((apply function args) (values))
           (process-allow-schedule))
  #+cmu (mp:process-wait-with-timeout whostate seconds
				      #'(lambda () (apply function args)))
  #+allegro (apply #'mp:process-wait-with-timeout whostate seconds function args)
  #+:lispworks (apply #'mp:process-wait-with-timeout whostate seconds function args))

(defun proc.wait-for-input-available (streams whostate &key wait-function timeout)
  #+mcl (error "Not implemented for MCL.")
  #+(or :cmu :lispworks)
  (mp:process-wait-with-timeout
         whostate
	 timeout
	 #'(lambda ()
	     (or (and wait-function (some wait-function streams))
		 (some #'listen streams))))
  #+allegro (declare (ignore timeout))
  #+allegro (mp:wait-for-input-available streams
					 :whostate whostate
					 :wait-function (or wait-function #'stream-listen)))
  

(defun proc.get-all-processes ()
  "Returns a list of all current running processes."
  #+:mcl ccl:*all-processes*
  #+cmu (mp:all-processes)
  #+allegro mp:*all-processes*
  #+:lispworks (mp:list-all-processes))


(defun proc.yield ()
  "Causes the current process to give up the rest of its time quantum."
  #+:mcl (ccl:process-allow-schedule)
  #+cmu (mp:process-yield)
  #+allegro (mp:process-allow-schedule)
  #+:lispworks (mp:process-allow-scheduling))



(defun proc.active? (process)
  #+:mcl (ccl:process-active-p process)
  #+cmu (mp:process-active-p process)
  #+allegro (mp:process-active-p process)
  #+:lispworks (mp::process-active-p process))

(defun proc.start (process)
  #+:mcl (ccl:process-enable process)
  #+cmu (mp:enable-process process)
  #+:allegro (mp:process-enable process)
  #+:lispworks (mp:process-enable process))

(defun proc.stop (process)
  #+:mcl (ccl:process-disable process)
  #+cmu (mp:disable-process process)
  #+allegro (mp:process-disable process)
  #+:lispworks (mp:process-disable process))

(defun proc.destroy (process)
  "Really kills a process.  If a process is killed while it is stopped,
   it won't terminate until it is started."
  #+:mcl (let ((kill-result (proc.kill process)))
          (when (proc.active? process)
            (proc.start process))
          kill-result)
  #+cmu (mp:destroy-process process)
  #+allegro (mp:process-kill process)
  #+:lispworks (mp:process-kill process))

;; CMUCL doesn't support passing arguments to the specified function, so 
;; we don't either.
(defun proc.interrupt (process function)
  "Interrupts the given process, forcing it to evaluate the 
  given function.  The process resumes whatever it was doing when
  the function is finished.  Waiting processes are temporarily
  woken up."
  #+:cmu (mp:process-interrupt process function)
  #+:allegro (mp:process-interrupt process function)
  #+:lispworks (mp:process-interrupt process function)
  #+:mcl (error "Not yet implemented under MCL -- try back later."))


;;------------------------------
;; Locks
;;------------------------------

(defun proc.make-lock ()
  "Creates a lock."
  #+:mcl (ccl:make-lock)
  #+cmu (mp:make-lock)
  #+allegro (mp:make-process-lock)
  #+:lispworks (mp:make-lock))


(defun proc.get-lock (lock)
  "Claims a lock, blocking until the current process can get it."
  #+:mcl (ccl:process-lock lock)
  #+cmu (error "Not implemented for CMUCL.")
  #+allegro (mp:process-lock lock)
  #+:lispworks (mp:process-lock lock))


(defun proc.giveup-lock (lock)
  "Gives up possession of a lock."
  #+:mcl (ccl:process-unlock lock)
  #+cmu (error "Not implemented for CMUCL.")
  #+allegro (mp:process-unlock lock)
  #+:lispworks (mp:process-unlock lock))


(defmacro proc.with-lock (lock &rest body)
  "A simpler way to use locks that proc.get/giveup-lock.  Just wrap this
   around the code that uses the lock."
  #+:mcl `(ccl:with-lock-grabbed (,lock) ,@body)
  #+cmu `(mp:with-lock-held (,lock) ,@body)
  #+allegro`(mp:with-process-lock (,lock) ,@body)
  #+:lispworks `(mp:with-lock (,lock) ,@body))


;; Indent nicely in the MCL editor.

#+:mcl (pushnew '(proc.with-lock . 1) ccl::*fred-special-indent-alist*
               :test #'equal)


;;------------------------------
;; Gates
;;------------------------------

(defun proc.make-gate (&optional (open T))
  (if open
      (make-array 1 :element-type 'bit :initial-element 1)
    (make-array 1 :element-type 'bit :initial-element 0)))


(defun proc.gate-open-p (gate)
  (= (aref gate 0) 1))

(defun proc.open-gate (gate)
  (setf (aref gate 0) 1))

(defun proc.close-gate (gate)
  (setf (aref gate 0) 0))

;;------------------------------
;; Process Queues
;;------------------------------

(defun proc.make-queue (name)
  "Creates a process queue."
  #+:mcl (ccl:make-process-queue name)
  #+cmu (mp:make-lock)
  #+allegro (mp:make-process-lock :name name)
  #+:lispworks (mp:make-lock :name name))

(defun proc.enqueue (queue)
  "Adds the current process to the queue, blocking until the process
   reaches the front of the queue."
  #+:mcl (ccl:process-enqueue queue)
  #+cmu (error "Not implemented for CMUCL.")
  #+allegro (mp:process-lock queue)
  #+:lispworks (mp:process-lock queue))

(defun proc.dequeue (queue)
  "Removes the current process from the queue."
  #+:mcl (ccl:process-dequeue queue)
  #+cmu (error "Not implemented for CMUCL.")
  #+allegro (mp:process-unlock queue)
  #+:lispworks (mp:process-unlock queue)) ;; This might not be right.

(defmacro proc.with-queue (queue &rest body)
  "The easiest way to use queues."
  #+:mcl `(ccl:with-process-enqueued (,queue nil nil NIL) ,@body)
  #+cmu `(mp:with-lock-held (,queue) ,@body)
  #+allegro `(mp:with-process-lock (,queue) ,@body)
  #+:lispworks `(mp:with-lock (,queue) ,@body))

(defun proc.queue-owner (queue)
  "Current owner of the queue, if any."
  #+:mcl (ccl:process-queue-locker queue)
  #+cmu (mp::lock-process queue)
  #+allegro (mp:process-lock-locker queue)
  #+:lispworks (mp:lock-owner queue))


;; Try to correct problem of insufficient voodoo.

#|
(defmacro proc.with-queue (queue &rest body)
  "The easiest way to use queues."
  #+:mcl `(ccl:with-process-enqueued (,queue nil nil NIL)
           ,@body))
|#


;; Indent nicely in the MCL editor.

#+:mcl (pushnew '(proc.with-queue . 1) ccl::*fred-special-indent-alist*
               :test #'equal)
 

;;; -- specials in (find-package :common-lisp)
;;; -- excepting *, **, ***
;;;*print-escape* 
;;;*compile-file-truename* 
;;;*compile-print* 
;;;*read-eval* 
;;;*print-pprint-dispatch* 
;;;*modules* 
;;;*print-pretty* 
;;;*read-base* 
;;;*package* 
;;;*default-pathname-defaults* 
;;;*random-state* 
;;;*debugger-hook* 
;;;*gensym-counter* 
;;;*print-readably* 
;;;*compile-verbose* 
;;;*print-radix* 
;;;*error-output* 
;;;*print-right-margin* 
;;;*print-circle* 
;;;*compile-file-pathname* 
;;;*features* 
;;;*standard-output* 
;;;*trace-output* 
;;;*query-io* 
;;;*load-truename* 
;;;*print-gensym* 
;;;*standard-input* 
;;;*print-miser-width* 
;;;*load-pathname* 
;;;*debug-io* 
;;;*print-length* 
;;;*print-lines* 
;;;*print-base* 
;;;*break-on-signals* 
;;;*read-suppress* 
;;;*print-case* 
;;;*load-verbose* 
;;;*readtable* 
;;;*print-array* 
;;;*macroexpand-hook* 
;;;*print-level* 
;;;*load-print* 
;;;*read-default-float-format* 
;;;*terminal-io* 

(provide :inet.util.process)