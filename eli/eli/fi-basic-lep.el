;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Id: fi-basic-lep.el,v 1.47.6.5.8.1 2003/03/26 16:16:36 layer Exp $
;;
;; The basic lep code that implements connections and sessions

(defvar fi::trace-lep-filter nil)	; for debugging

(defvar session) ;; yuck

(defun fi::show-error-text (format-string &rest args)
  (if (cdr fi:pop-up-temp-window-behavior)
      (apply 'fi:show-some-text nil format-string args)
    (let ((fi:pop-up-temp-window-behavior (cons (car fi:pop-up-temp-window-behavior) 't)))
      (apply 'fi:show-some-text nil format-string args))))

(defvar fi:pop-up-temp-window-behavior '(other . t)
  "*The value of this variable determines the behavior of the popup
temporary buffers used to display information which is the result of
queries of the Lisp environment.  The value is a cons of the style and a
boolean of whether or not the minibuffer should be used for displaying the
result.  The possible values for the CAR of the cons are the symbols
SPLIT, OTHER, and REPLACE.  SPLIT causes the largest window to be split
and the new window to be minimal in size.  OTHER causes the other window to
be used, spliting the screen if there is only one window.  REPLACE causes
the current window to be replaced with the help buffer.  The reason for
specifying a CDR of nil is so that a window is always used--messages
printed in the minibuffer can easily be erased.")

(defun fi:show-some-text (xpackage text &rest args)
  (when args (setq text (apply (function format) text args)))
  (let ((n (string-match "[\n]+\\'" text)))
    (when n (setq text (substring text 0 n))))
  (if (null (cdr fi:pop-up-temp-window-behavior))
      (fi::show-some-text-1 text (or xpackage fi:package))
    (let* ((window (minibuffer-window))
	   ;;(height (1- (window-height window)))
	   (width (window-width window))
	   (text-try
	    ;; cond clause commented 18oct94 smh.
	    ;; Now that package is indicated in the mode line, the real estate
	    ;; can be saved.
	    (cond ;; (fi:package (format "[package: %s] %s" fi:package text))
	     (t text)))
	   (lines/len (fi::frob-string text-try)))
      (if (and (< (car lines/len) 2)
	       (<= (second lines/len) width))
	  (progn
	    (message "%s" text-try)
	    (fi::note-background-reply))
	(fi::show-some-text-1
	 ;; cond clause commented 18oct94 smh.  See above.
	 (cond ;; (fi:package (format "[package: %s]\n%s" fi:package text))
	  (t text))
	 (or xpackage fi:package))))))

(defun fi::frob-string (text)
  (let ((start 0)
	(lines 0)
	(length (length text))
	(max-length 0)
	last
	m)
    (while (and (setq m (string-match "$" text start))
		(< m length))
      (setq last m)
      (let ((len (- m start)))
	(if (> len max-length) (setq max-length len)))
      (setq lines (+ lines 1)
	    start (1+ m)))
    (if (not (eq m last)) (setq lines (1+ lines)))
    (let ((len (- length start)))
      (if (> len max-length) (setq max-length len)))
    (list lines max-length)))

(defvar fi::show-some-text-1-first-time t)

(defun fi::show-some-text-1 (text apackage &optional hook &rest args)
  ;;A note from cer:
  ;;  I think there is some buffer local variable called package
  ;;  Outside of the save-excursion package has the correct value.
  ;;  Inside it has the value of NIL.
  ;;  Consequently fi:package ends up as NIL in *CL-temp* buffers.
  ;;  I renamed package to apackage and that seemed to fix it.
  (let ((buffer (get-buffer-create "*CL-temp*")))
    ;; fill the buffer
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (fi:common-lisp-mode)
      (setq fi:package apackage)
      (insert text)
      (beginning-of-buffer))
    (fi::display-pop-up-window buffer hook args))
  (fi::note-background-reply)
  (when fi::show-some-text-1-first-time
    (message
     "%s"
     (substitute-command-keys
      "Type \\[fi:lisp-delete-pop-up-window] to remove *CL-temp* window."))
    (setq fi::show-some-text-1-first-time nil)))

;;;;;;;;;;;;;;;;;;;;;;TODO
;;; The filter should not abort because of errors. Some how the errors
;;; should be printed.

(defun fi::make-connection (buffer host process)
  (list ':connection
	process
	nil				; sessions
	-1				; session id counter
	host
	buffer))

(defun fi::connection-process (c) (second c))

(defun fi::connection-sessions (c) (third c))
(defun fi::set-connection-sessions (c nv) (setf (third c) nv))

(defun fi::connection-session-id (c) (fourth c))
(defun fi::set-connection-session-id (c nv) (setf (fourth c) nv))

(defun fi::connection-host (c) (fifth c)) ; not used, apparently

(defun fi::connection-buffer (c) (sixth c))

(defvar fi::*connection* nil)

(defun fi::lep-open-connection-p ()
  (and fi::*connection*
       (fi:process-running-p (fi::connection-process fi::*connection*))
       fi::*connection*))

(defun fi::ensure-lep-connection ()
  (or (fi::lep-open-connection-p)
      (fi::try-and-start-lep-connection)
      (error "no connection")))

(defun fi:reset-lep-connection ()
  "Reset the Lisp-editor protocol connection."
  (interactive)
  (set-menubar-dirty-flag)		;smh 31oct94
  (setq fi::*connection* nil))

;;; Start up a connection as soon as we know where to connect to.


(if (boundp 'fi:start-lisp-interface-hook)
    (push 'fi::auto-ensure-lep-connection fi:start-lisp-interface-hook))

(defun fi::auto-ensure-lep-connection ()
  (message "Trying to start connection...")
  (fi::ensure-lep-connection)
  (message "Trying to start connection...done."))

(defun fi::try-and-start-lep-connection ()
  (let ((buffer (process-buffer (progn
				  (fi::sublisp-select)
				  (get-process fi::process-name)))))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (fi::start-connection))
      (fi:error "
An internal error has occurred.  There is no Lisp process and the
emacs-lisp interface cannot be started.
"))))

(defun fi::find-connection-from-process (process)
  fi::*connection*)

(defun fi::find-session (connection id)
  "Search a CONNECTION looking for a session with the ID"
  (let ((sessions (fi::connection-sessions connection)))
    (while (and sessions (not (eq (fi::session-id (car sessions)) id)))
      (setq sessions (cdr sessions)))
    (and sessions (car sessions))))

(defun fi::start-connection ()
  (fi::make-connection-to-lisp fi::lisp-host
			       fi::lisp-port
			       fi::lisp-password
			       fi::lisp-ipc-version))

(defun fi::make-connection-to-lisp (host port passwd ipc-version)
  (cond ((eq ipc-version fi::required-ipc-version)
	 (let* ((proc-name (format " *LEP %s %d %d*" host port passwd))
		;; buffer-name used to be non-nil only when fi::lep-debug
		;; was non-nil, but changes to lep::make-session-for-lisp
		;; depend on there being a buffer associated with the
		;; process.  So, we now do this in all cases.
		(buffer-name proc-name)
		(buffer (when buffer-name
			  (get-buffer-create buffer-name)))
		(process (fi::open-network-stream proc-name nil host port)))
	   (when buffer
	     (bury-buffer buffer)
	     (save-excursion (set-buffer buffer) (erase-buffer))
	     (set-process-buffer process buffer))
	   ;; cac 20dec00
	   (when (and (fboundp 'set-process-coding-system)
		      ;; spr24414
		      (member 'emacs-mule (coding-system-list)))
	     (set-process-coding-system process 'emacs-mule 'emacs-mule))
	   (set-process-filter process 'fi::lep-connection-filter)
	   ;; new stuff to indicate that we want the lisp editor protocol
	   (process-send-string process ":lep\n")
	   (process-send-string process (format "\"%s\"\n" proc-name))
	   (process-send-string process (format "%d \n" passwd))
	   ;; Send the class of the editor to the lisp.
	   ;; This might affect something!
	   ;; For example, gnu 19 has some good features.
	   (process-send-string
	    process
	    (format "\"%s\"\n"
;;;; The following works in xemacs 20.x when this file is compiled
;;;; with emacs 19.x, but we don't want to install this hack since
;;;; there are hundreds of other places a similar hack would have to
;;;; be installed.
		    ;;(remove (aref "\"" 0) (emacs-version))
		    (remove ?\" (emacs-version))))
	   (prog1
	       (setq fi::*connection*
		 (fi::make-connection (current-buffer) host process))
	     (set-menubar-dirty-flag))))
	(t
	 (fi:error
	  "
The Allegro CL ipc version is ``%s'' (from the variable excl::*ipc-version*
in the Lisp environment).  This version of the emacs-lisp interface
requires version ``%s''.  This mismatch would most likely be caused by the
Emacs and Lisp not being from the same distribution.  If the obtained ipc
version is `nil', then it is most likely you are using the emacs-lisp
interface from ACL 4.1 or later with an older Lisp.

See lisp/fi/examples/emacs.el for code to correctly startup different
versions of the emacs-lisp interface.
"
	  ipc-version fi::required-ipc-version))))

(defvar fi::debug-subprocess-filter nil)
(defvar fi::debug-subprocess-filter-output nil)

(defun fi::lep-connection-filter (process string)
  ;; When a complete sexpression comes back from the lisp, read it and then
  ;; handle it
  (when fi::debug-subprocess-filter
    (push string fi::debug-subprocess-filter-output))
  (let ((inhibit-quit t)
	(buffer (or (process-buffer process)
		    (get-buffer-create " LEP temp "))))
    (when fi::trace-lep-filter
      (print string (get-buffer "*scratch*")))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (insert string))
    (let (form error)
      (while
	  (condition-case nil
	      (save-excursion
		(set-buffer buffer)
		(and
		 (null error)
		 (not (eq (point-max) (point-min)))
		 (progn
		   (goto-char (point-min))
		   (forward-sexp)
		   (let ((p (point)))
		     (goto-char (point-min))
		     (condition-case nil
			 (progn (setq form (read (current-buffer))) t)
		       (error (setq error t)))
		     (delete-region (point-min) p))
		   t)))
	    (error nil))
	(if error
	    (error "error reading return value: %s" string)
	  (fi::handle-lep-input process form))))))

(defun fi::handle-lep-input (process form)
  "A reply is (session-id . rest) or (nil . rest)"
  (when fi::trace-lep-filter
    (print (list process form) (get-buffer "*scratch*")))
  (let* ((id (car form))
	 (connection (fi::find-connection-from-process process))
	 (session (fi::find-session connection id)))
    (cond (session
	   (fi::handle-session-reply session (cdr form)))
	  ((car form)
	   (error "something for nonexistent session: %s" form))
	  (t (fi::handle-sessionless-reply (cdr form))))))

(defun fi::handle-session-reply (session form)
  "A session reply is (:error Message) or (nil . results)"
  (when (fi::session-oncep session)
    (fi::delete-session session))
  (if (eq (car form) ':error)
      (if (fi::session-error-function session)
	  (apply (fi::session-error-function session)
		 (second form)
		 (fi::session-error-arguments session))
	(error (second form)))
    (apply (fi::session-function session)
	   (if (fi::session-arguments session)
	     (append (cdr form) (fi::session-arguments session))
	     (cdr form)))))

(defun fi::delete-session (session)
  (let ((connection (fi::session-connection session)))
    (fi::set-connection-sessions
     connection
     (delq session (fi::connection-sessions connection)))))

(defun fi::handle-sessionless-reply (form)
  ;; A session-less reply is either (:error message) or (:request fn . args)
  (cond ((eq (car form) ':error)
	 (error (second form)))
	((eq (car form) ':request)
	 (condition-case error
	     (apply (fi::intern-it (second form)) (cddr form))
	   (error
	    (fi::show-error-text "Request error: %s"
				 (fi::prin1-to-string error)))))
	(t (error "Funny request received: %s" form))))


(defun fi::make-session (id oncep &optional fn args error-fn error-args)
  (list 'session id fn args error-fn error-args oncep))

(defun fi::session-id (s) (second s))

(defun fi::session-function (s) (third s))
(defun fi::set-session-function (s nv) (setf (third s) nv))


(defun fi::session-arguments (s) (fourth s))
(defun fi::set-session-arguments (s nv) (setf (fourth s) nv))

(defun fi::session-error-function (s) (fifth s))
(defun fi::set-session-error-function (s nv) (setf (fifth s) nv))

(defun fi::session-error-arguments (s) (sixth s))
(defun fi::set-session-error-arguments (s nv) (setf (sixth s) nv))

(defun fi::session-oncep (s) (seventh s))

(defun fi::session-connection (s) fi::*connection*)

(defun fi::modify-session-continuation (session continuation-and-arguments
					error-continuation)
  (fi::set-session-function session (car continuation-and-arguments))
  (fi::set-session-arguments session (cdr continuation-and-arguments))
  (fi::set-session-error-function session (car error-continuation))
  (fi::set-session-error-arguments session (cdr error-continuation)))

(defun fi::make-new-session (connection oncep continuation-and-arguments
			     &optional error-continuation)
  (let* ((id (fi::connection-session-id connection))
	 (session (fi::make-session id
				oncep
				(car continuation-and-arguments)
				(cdr continuation-and-arguments)
				(car error-continuation)
				(cdr error-continuation))))
    (fi::set-connection-session-id connection (1- id))
    (fi::add-session connection session)
    session))

(defun fi::remove-session (connection session)
  (fi::set-connection-sessions
   connection
   (delq session (fi::connection-sessions connection))))

(defun fi::add-session (connection session)
  (fi::set-connection-sessions
   connection
   (cons session (fi::connection-sessions connection))))



;;(defun lep::handle-query (session function arguments)
;;  (lep::send-back-reply session (apply function arguments)))


(defun lep::send-request-in-new-session (session-class oncep session-arguments
					 continuation-and-arguments
					 &optional error-continuation-and-arguments
						   ignore-package)
  (unless (fi::lep-open-connection-p)
    (error "There is no connection to Lisp.  See fi:common-lisp documentation."))

  (let* ((connection (fi::ensure-lep-connection))
	 (session (fi::make-new-session connection oncep
					continuation-and-arguments
					error-continuation-and-arguments))
	 (process (fi::connection-process connection)))
    (process-send-string process (fi::prin1-to-string
			  (list* nil
				 'lep::make-session session-class
				 ':session-id (fi::session-id session)
				 ':buffer-readtable (fi::string-to-keyword
						     fi:readtable)
				 (if (or ignore-package
					 (fi::member-plist ':buffer-package
							   session-arguments))
				     session-arguments
				   (list* ':buffer-package
					  (fi::string-to-keyword fi:package)
					  session-arguments)))))
    (process-send-string process "\n")
    session))

(defmacro fi::make-request (type-and-options continuation
			    &optional error-continuation ignore-package)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	t
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(list* 'list
	       (list 'function
		     (list* 'lambda (append (fi::listify (second continuation))
					    (fi::listify (first continuation)))
			    (cddr continuation)))
	       (first continuation))
	(list* 'list (list 'function
			   (list* 'lambda (append (fi::listify (second error-continuation))
						  (fi::listify (first error-continuation)))
				  (cddr error-continuation)))
	       (first error-continuation))
	ignore-package))


(defmacro fi::make-complex-request (type-and-options continuation
				    &optional error-continuation)
  (list 'lep::send-request-in-new-session
	(list 'quote (car type-and-options))
	nil
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(list* 'list (list 'function
			   (list* 'lambda
				  (append (fi::listify (second continuation))
					  (fi::listify (first continuation)))
				  (cddr continuation)))
	       (first continuation))
	(list* 'list (list 'function
			   (list* 'lambda
				  (append (fi::listify (second error-continuation))
					  (fi::listify (first error-continuation)))
				  (cddr error-continuation)))
	       (first error-continuation))))

(defun lep::send-request-in-existing-session (session session-class oncep
					      session-arguments
					      continuation-and-arguments
					      &optional error-continuation-and-arguments)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (process-send-string process
		 (fi::prin1-to-string
		  (list* (fi::session-id session) session-class session-arguments)))
    (process-send-string process "\n")))

(defun lep::kill-session (session)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (fi::remove-session connection session)
    (process-send-string process (fi::prin1-to-string
			  (list nil 'lep::terminate-session
				(fi::session-id session))))
    (process-send-string process "\n")))

(defun lep::send-request-in-session (session session-class session-arguments
				     continuation-and-arguments
				     &optional error-continuation-and-arguments)
  (let* ((connection (fi::ensure-lep-connection))
	 (process (fi::connection-process connection)))
    (fi::modify-session-continuation
     session continuation-and-arguments error-continuation-and-arguments)
    (process-send-string process
		 (fi::prin1-to-string (list* (fi::session-id session)
					     ':request
					     session-class
					     session-arguments)))
    (process-send-string process "\n")))

(defmacro fi::make-request-in-existing-session (session type-and-options
						continuation
						&optional error-continuation)
  (list 'lep::send-request-in-session
	session
	(list 'quote (car type-and-options))
	(cons 'list (fi::quote-every-other-one (cdr type-and-options)))
	(list* 'list (list 'function
			   (list* 'lambda
				  (append (fi::listify (second continuation))
					  (fi::listify (first continuation)))
				  (cddr continuation)))
	       (first continuation))
	(list* 'list
	       (list 'function
		     (list* 'lambda (append (fi::listify (second error-continuation))
					    (fi::listify (first error-continuation)))
			    (cddr error-continuation)))
	       (first error-continuation))))

(defun fi::intern-it (s)
  (if (stringp s) (intern s) s))

(defvar connection) ;; ugly, but necessary
(defvar process) ;; this, too...  crikey, this is grossssss!!!!!

(defun lep::make-session-for-lisp (session-id replyp oncep function &rest args)
  (let ((session (fi::make-session session-id nil))
	(done nil)
	(dead (or (eq 'closed (process-status process))
		  ;; The following test relies on
		  ;; fi::make-connection-to-lisp associating the process
		  ;; with a buffer IN ALL CASES, not just for debugging.
		  (null (process-buffer process)))))
    (fi::add-session connection session)
    (unwind-protect
	(when (not dead)
	  (condition-case error
	      (let* ((result (apply (fi::intern-it function) args)))
		(when replyp
		  (process-send-string process
				       (fi::prin1-to-string
					(list* (fi::session-id session)
					       ':reply
					       result)))
		  (process-send-string process "\n")))
	    (error
	     (if replyp
		 (progn
		   (process-send-string process
					(fi::prin1-to-string
					 (list (fi::session-id session)
					       ':error
					       (fi::prin1-to-string error))))

		   (process-send-string process "\n"))
	       (fi::show-error-text
		"Error %s in %s\nwith args: %s\nstack dump:\n%s"
		(fi::prin1-to-string
		 (if (and (consp error) (cdr error))
		     (cdr error)
		   error))
		function
		args
		(when (fboundp 'backtrace)
		  (with-output-to-string (backtrace)))))))
	  (setq done t))
      (when (and (not dead) (not done))
	(process-send-string process
		     (fi::prin1-to-string (list (fi::session-id session)
						':error
						':aborted))))
      (when (or dead oncep)
	(lep::kill-session session)))))

(defun fi:send-reply (session string)
  (let* ((connection (fi::session-connection session))
	 (process (fi::connection-process connection)))
    (process-send-string process
		 (fi::prin1-to-string
		  (list (fi::session-id session) ':reply string)))
    (process-send-string process "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi::eval-in-lisp-wait-for-connection ()
  (if (not (fi::lep-open-connection-p))
      (let ((i 0) (max 20))
	(while (and (< i max)
		    fi::common-lisp-backdoor-main-process-name
		    (get-process fi::common-lisp-backdoor-main-process-name)
		    (not (fi::lep-open-connection-p))
		    (progn (sleep-for 3) t))
	  (setq i (+ i 1))))))

(defun fi:eval-in-lisp-asynchronous (string &rest args)
  "Apply (Emacs Lisp) format to STRING and ARGS and asychronously evaluate
the result in the Common Lisp to which we are connected."
  (fi::eval-in-lisp-wait-for-connection)
  (let ((string (if args (apply 'format string args) string)))
    (fi::make-request
	;;fi::frob-case-to-lisp removed - 18jan94 smh
	(lep::eval-from-emacs-session :string string)
      ;; Normal continuation
      (() (value)
       ;; ignore the value...
       nil)
      ((string) (error)
       (fi::show-error-text "error evaluating %s: %s" string error)))))

(defvar fi:lisp-evalserver-number-reads 200
  "*The number of times the Lisp eval server tries to read from the
lisp-evalserver process before giving up.  Without this feature Emacs would
hang if Lisp got into an infinite loop while printing.  If the size of the
values returned to Emacs is large, then the value of this variable should
be increased.")

(defun fi:eval-in-lisp (string &rest args)
  "Apply (Emacs Lisp) format to STRING and ARGS and sychronously evaluate
the result in the Common Lisp to which we are connected."
  (fi::eval-in-lisp-wait-for-connection)
  (let ((string (if args (apply 'format string args) string)))
    ;;fi::frob-case-to-lisp removed - 18jan94 smh
    (car (lep::eval-session-in-lisp 'lep::eval-from-emacs-session
				    ':string string))))

(defun lep::eval-session-in-lisp (function &rest arguments)
  (let* ((result-cons (list nil nil nil))
	 (session (lep::send-request-in-new-session
		   function
		   t
		   arguments
		   (list (function fi::immediate-reply-continuation)
			 result-cons)
		   (list (function fi::immediate-reply-error-continuation)
			 result-cons))))
    (fi::wait-for-reply-to-come-back result-cons)))

(defun fi::immediate-reply-continuation (&rest results)
  (let ((r (butlast results))
	(c (car (last results))))
    (fi::stash-in-cons c r nil)))

(defun fi::immediate-reply-error-continuation (error result-cons)
  (fi::stash-in-cons result-cons error t))

(defun fi::stash-in-cons (c r p)
  (setf (third c) p)
  (setf (second c) r)
  (setf (first c) t))

;;; This is complicated because we have to wait for output multiple times.

(defun fi::wait-for-reply-to-come-back (result-cons)
  (when (not (car result-cons))
    (let ((count fi:lisp-evalserver-number-reads))
      (while (and (> (setq count (1- count)) 0)
		  (null (car result-cons)))
	(accept-process-output
	 (fi::connection-process (fi::ensure-lep-connection)))))
    (when (not (car result-cons)) (error "Eval in lisp timed out"))
    (if (third result-cons)
	(error (second result-cons))
      (second result-cons))))

(defun lep::make-request-in-session-and-wait (session function &rest arguments)
  "Send a request to SESSION consisting of FUNCTION and ARGUMENTS and wait
for the reply to come back. It returns a list of values."
  (let* ((result-cons (list nil nil nil))
	 (session (lep::send-request-in-session
		   session
		   function
		   arguments
		   (list (function fi::immediate-reply-continuation)
			 result-cons)
		   (list (function fi::immediate-reply-error-continuation)
			 result-cons))))
    (fi::wait-for-reply-to-come-back result-cons)))
