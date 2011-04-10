;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Emacs interface for Windows
;;; apex/eli/apex-windows.el
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: apex-windows.el,v 1.2 2006/01/15 03:42:52 dalal Exp $


;;; Apex may be started in Emacs with the command "Meta-x start-apex".
;;; Depending on your keyboard layout the "Meta" key may be one of
;;; Option, Command or Alt.  If none of these combinations work pressing
;;; ESC followed by "x" has the same effect.

;;; To enable this feature, you must first complete the following steps:
;;;
;;; 1. Load this file into Emacs.  The easiest way is to do it in your
;;; .emacs file.  If you don't already have a ".emacs" file, create one
;;; in C:/.  Add the following line:
;;;
;;;    (load "c:/apex2-3-4/eli/apex.el")
;;;
;;; Use the correct Apex installation path if yours differs from this
;;; example.
;;;
;;;
;;; 2. If necessary, edit the following line to contain the name of your
;;; Apex installation directory.  It should not end with a slash (/).
;;;
(defconst *apex-installation-directory* "c:/apex2-3-4")

(defconst *emacs-interface-dir*
    (concat *apex-installation-directory* "/eli"))
(setq load-path (cons *emacs-interface-dir* load-path))
(require 'fi-site-init)

(setq fi:common-lisp-directory *apex-installation-directory*)
(setq fi:common-lisp-buffer-name "*apex*")
(setq fi:common-lisp-host "localhost")
(setq fi:common-lisp-image-arguments nil)
(setq fi:common-lisp-image-name
  (concat *apex-installation-directory* "/apexapp.exe"))
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
