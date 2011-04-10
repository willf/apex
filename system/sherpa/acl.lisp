;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/sherpa/acl.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: acl.lisp,v 1.4 2006/01/15 03:43:02 dalal Exp $

;;; ACL-specific code for Sherpa interface


(in-package :common-lisp-user)

;;; ACL

;; the socket used to connent to sherpa

(defvar *sherpa-socket* nil)

;; sherpa server running state

(defvar *sherpa-running* nil)

;; acl is multi threaded

(defparameter *multithreaded* t)

;; name of the command handler process name

(defparameter *sherpa-command-handler-name* "Sherpa Command Handler")

;; write a standard line break to a stream

(defun line-break (stream)
  (write-char (code-char 13) stream))

;;; read line from stream

(defun sherpa-read-line (stream)
  (read-line stream))

;; entry point to start the sherpa server

(defun sherpa ()  ; () -> ()
  (stop-sherpa-server)
  (start-sherpa-server))

;; spawns a sherpa server in a new thread

(defun start-sherpa-server ()
  (mp:process-run-function 
   (list :name  "Sherpa Server" 
         :initial-bindings 
         (list (cons '*standard-output* *terminal-io*)))
    #'sherpa-server))

;; tell the sherpa sherver to stop running

(defun stop-sherpa-server ()
  (if *sherpa-running*
      (progn
        (setq *sherpa-running* nil)
        (setq *trace-destination* 'listener)
        (if *sherpa-socket* (close *sherpa-socket*))
;;        (acl-socket:shutdown *sherpa-socket* :direction :output) 
;;        (acl-socket:shutdown *sherpa-socket* :direction :input)
)))

;; the sherpa server

(defun sherpa-server ()
  (setq cl:*print-case* :downcase)
  (format t "started sherpa server...~%")
  (setq *sherpa-running* t)
  (setq *trace-destination* 'sherpa)
  (setq *sherpa-socket* (acl-socket:make-socket :connect
                                           :passive 
                                           :local-port *sherpa-port* 
                                           :reuse-address t
                                           :backlog 1))
  (unwind-protect
      (loop for connection-number from 1 do
            (setq *socket-stream*
              (ignore-errors (acl-socket:accept-connection *sherpa-socket*)))
              (when (not *sherpa-running*) (return))
              (mp:process-run-function
                  (list :name (format nil "~a ~a"
                                      *sherpa-command-handler-name*
                                      connection-number)
                     :initial-bindings (list
                                        (cons '*standard-output*
                                              excl:*initial-terminal-io*)))
                 #'(lambda ()
                     (unwind-protect
                         (sherpa-command-handler)
                       (close *socket-stream*)))))
    (format t "stopped sherpa server.~%")))

;; handle commands as they come in from sherpa stream

(defun sherpa-command-handler ()
  (let ((*print-pretty* nil))
    (loop until (or (eq :stop-command-handler 
                        (dispatch-sherpa-command))
                    (not *sherpa-running*)))))
