;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/load-support.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: load-support.lisp,v 1.11 2006/03/13 17:18:24 will Exp $

;;; Low level support for loading and compiling files, and Apex startup.


(in-package :common-lisp-user)

;;; Function to be called when runtime application (distribution image)
;;; starts.  Was called automatically when image was created with
;;; GENERATE-EXECUTABLE.  Must be specified explicitly when using
;;; GENERATE-APPLICATION.
;;;
(defun main (&rest args)
  ;;
  ;; For debugging
  ;; (format t "args: ~S~%" args)
  ;; (format t "hosts: ~S~%" (translate-logical-pathname "sys:hosts.cl"))
  ;; (when (string= "foo" (second args))
  ;;   (error "foo!"))
  ;;
  (sherpa)
  (startup-terminal)
  
  ;; make our emacs connection
  #-:linux
  (progn
    (format t "~2%*** If you started Apex from Emacs, please enter (emacs)~%")
    (format t "at the prompt to complete the connection. ***~2%"))

  (tpl:start-interactive-top-level
   *terminal-io* 'tpl:top-level-read-eval-print-loop nil))


;;; Compiler related

(defparameter *compile-apex-files-if-possible* t)


(defvar *compiler-available* nil)
(defvar *fasl-extension*
    (pathname-type (compile-file-pathname *default-pathname-defaults*)))

(defun load-apex-files (dir)
  ;;
  ;; string -> ()
  ;; Loads compiled versions (compiling if needed) of all .lisp files in
  ;; given (immediate) subdirectory of apex.
  ;; ! Should extend to:
  ;;    - load compiled files that don't have accompanying source
  ;;    - check if compiler exists before attempting compile, loading
  ;;      source otherwise
  ;;
  (format t "~%~%;;; Loading ~a ...~%" dir)
  (loop 
      for source-file 
      in (directory (format nil "apex:~a;*.lisp" dir))
      doing
        (if (and (or *compiler-available*
		     #+(and :allegro :compiler) t)
                 *compile-apex-files-if-possible*)
            (let ((object-file (make-pathname :host "apex"
                                              :directory `(:absolute ,dir)
                                              :name (pathname-name source-file)
                                              :type *fasl-extension*)))
              (if (or
                   (not (probe-file object-file))
                   (> (file-write-date source-file)
                      (file-write-date object-file)))
                  (compile-file (make-pathname :host "apex"
                                               :directory `(:absolute ,dir)
                                               :name (pathname-name source-file)
                                               :type "lisp")
                                :output-file object-file))
              (load object-file))
          ;; else
          (load source-file))))

;;; Startup

(defun startup-terminal () ; -> ()
  (welcome)
  (if *load-patches* (load-apex-patches))
  (if *load-extensions* (load-apex-extensions))
  (if *load-preferences* (load-apex-preferences))
  (load-apex-info)
  (values))

(defun welcome ()  ; -> ()
  (format t "~%Welcome to Apex!~%")
  (format t "~%Current PrintCase=~a~%" cl:*print-case*)
  (format t
          "~%Version ~a, Copyright (C) 2003 NASA Ames Research Center~%"
          *apex-version-name*)
  (format t "Apex is built on Allegro Common Lisp (R), by Franz, Inc."))

;;; Patches

(defun load-apex-patches ()  ; -> ()
  (load-apex-files "patches"))

;;; Extensions

(defun load-apex-extensions ()  ; -> ()
  (load-apex-files "extensions"))

;;; Preferences

(defparameter *apex-preferences-filename* "apexprefs")
(defparameter *apex-info-filename* "apexinfo")
(defparameter *default-load-history-size* 5)

(defvar *apex-info*
  ;; Stores the contents of the apex info file, which is a plist of
  ;; type (var1 val1 var2 val2 ... varN valN) where each varI is a
  ;; symbol naming a variable assumed to exist, and each valI any type.
  ;; Initialized with defaults here.
    `(load-history-limit ,*default-load-history-size* agreement-signed nil))

(defun load-apex-preferences () ; -> ()
  (let ((file (user-prefs-file)))
    (if (probe-file file)
        (load file)
      (load (default-prefs-file)))))

(defun load-apex-info () ; -> ()
  ;; Reads the plist in the file.
  (let ((file (user-info-file)))
    (when (probe-file file)
      (with-open-file (f file :direction :input)
	(setq *apex-info* (read f)))
      (apex-get-dot-filename)
      t)))

(defun apex-get-dot-filename ()
  (set-dot-application-name 
    (or (getf  *apex-info* 'dot-application-name)
	*dot-application-name*)))

(defun apex-set-dot-filename (pathname)
  (unless (probe-file pathname)
    (error "~a not found." pathname))
  (set-dot-application-name pathname)
  (setf (getf *apex-info* 'dot-application-name) pathname))

(defun user-info-file ()                ; -> pathname
  (user-file *apex-info-filename*))

(defun update-user-info-file ()         ; -> ()
  (with-open-file (file (user-info-file)
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :overwrite)
    (format file ";;; DO NOT EDIT.  This file is generated by Apex.~%~%")
    (format file "~s~%" *apex-info*)))

(defun user-prefs-file ()               ; -> pathname
  (user-file *apex-preferences-filename*))

(defun default-prefs-file ()            ; -> pathname
  (user-file *apex-preferences-filename* t))

(defun user-file (name &optional default?)  ; string, Opt(bool) -> pathname
  (let ((default-file (make-pathname :host "apex" :name name)))
    (if default? default-file
      #+:unix
      (make-pathname :directory `(:absolute ,(sys:getenv "HOME"))
                     :name "" :type name) ; hidden file
      #+:mswindows
      default-file)))


;;; Deprecated form (never did anything meaningful)
;;;
(defun apex-info (&rest ignore) ; Opt(string) -> ()
  (declare (ignore ignore))
  (format t "NOTE: Deprecated APEX-INFO form")
  (if *load-truename*
      (format t " in ~a" *load-truename*))
  (format t " should be removed.~%")
  (values))
          
;;;
;;; *apex-bundle* support for multiple PDL namespaces.
;;;

(defvar *apex-bundle* nil)

(def-fwrapper wrap-honor-apex-bundle (&rest args)
  (declare (ignore args))
  (let ((*apex-bundle* *apex-bundle*))
    (call-next-fwrapper)))
  
(fwrap 'cl:load 'honor-apex-bundle 'wrap-honor-apex-bundle)
(fwrap 'cl:compile-file 'honor-apex-bundle 'wrap-honor-apex-bundle)


(defun in-apex-bundle (bundle)
  (setf *apex-bundle* bundle))

;;; Distribution support

;;; Convenient wrapper for completion of Emacs/Lisp link needed in
;;; distribution (due to bug in ACL).
;;;
(defun emacs ()
  (excl:start-emacs-lisp-interface t))

;;; A convenience function designed for skipping problematic
;;; (e.g. looping) regression tests.  The global referenced is defined
;;; in the file apex/regression-load.lisp; it is unbound in normal
;;; loads.

(defun skip-if-regression-testing (reason) ; string -> () + error
  (declare (special *regression-testing*))
  (if (and (boundp '*regression-testing*) *regression-testing*)
         (error "*** Skipping during regression tests because ~a ***~%"
                reason)
    (values)))

