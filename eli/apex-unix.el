;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Emacs interface for Unix
;;; apex/eli/apex-unix.el
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: apex-unix.el,v 1.2 2006/01/15 03:42:51 dalal Exp $


;;; Apex may be started in Emacs with the command "Meta-x start-apex".
;;; Depending on your keyboard layout the "Meta" key may be one of
;;; Option, Command or Alt.  If none of these combinations work pressing
;;; ESC followed by "x" has the same effect.

;;; To run Apex within Emacs, the code in this file must first be
;;; evaluated.  This is done automatically when starting Apex with the
;;; 'apex' command.  If you wish to start Apex from Emacs directly, then
;;; load this file first.  A convenient way to do this is to add the
;;; following line to your .emacs file:
;;;
;;;    (load (concat (getenv "APEX_HOME") "/eli/apex.el"))
;;;                     
;;; If you don't already have a ".emacs" file, create one in your home
;;; directory.

(defconst *apex-installation-directory* (getenv "APEX_HOME"))

(defconst *emacs-interface-dir*
    (concat *apex-installation-directory* "/eli"))
(setq load-path (cons *emacs-interface-dir* load-path))
(require 'fi-site-init)

(setq fi:common-lisp-directory *apex-installation-directory*)
(setq fi:common-lisp-buffer-name "*apex*")
(setq fi:common-lisp-host "localhost")
(setq fi:common-lisp-image-arguments nil)
(setq fi:common-lisp-image-name
  (concat *apex-installation-directory* "/apexapp"))
(setq fi:common-lisp-image-file
  (concat *apex-installation-directory* "/apexapp.dxl"))

(defun start-apex ()
  (interactive)
  (fi:common-lisp fi:common-lisp-buffer-name
                  fi:common-lisp-directory
                  fi:common-lisp-image-name
                  fi:common-lisp-image-arguments
                  fi:common-lisp-host
                  fi:common-lisp-image-file))
