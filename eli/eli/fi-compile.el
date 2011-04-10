;; $Id: fi-compile.el,v 2.10.6.3 2001/12/12 01:45:51 layer Exp $

(require 'cl)

;;We're going with emacs 20.x now, and the following doesn't variable
;;having a non-nil value doesn't work compiling eli... 
;;(setq byte-compile-compatibility t) ;; emacs 19

;; doesn't work:
;;(setq byte-compile-emacs19-compatibility t) ;; xemacs 20

(require 'font-lock) ;; makes some compiler warnings go away
(condition-case nil
    (require 'comint) ;; makes some compiler warnings go away
  (error nil))
(defvar mode-motion-hook) ;; makes xemacs compiler warning go away

(push (expand-file-name ".") load-path)

(setq fi::load-el-files-only t)
(load "fi-site-init")
(load "fi-leep0.el")

(setq fi-files
  '("fi-vers" "fi-basic-lep" "fi-changes" "fi-composer" "fi-db"
    "fi-dmode" "fi-emacs18" "fi-emacs19" "fi-filec" "fi-gnu"
    "fi-indent" "fi-keys" "fi-leep0"
    "fi-lep" "fi-lze" "fi-modes" "fi-ring" "fi-rlogin" "fi-shell"
    "fi-stream" "fi-su" "fi-sublisp" "fi-subproc" "fi-telnet" "fi-utils"))

(when (or (eq fi::emacs-type 'xemacs19)
	  (eq fi::emacs-type 'xemacs20))
  (setq fi-files (append fi-files '("fi-xemacs"))))

(setq fi-developer-files '("localfidev"))

(dolist (file fi-files)
  (let ((el (format "%s.el" file))
	(elc (format "%s.elc" file)))
    (unless (file-newer-than-file-p elc el)
      (message "--------------------------------------------------------")
      (byte-compile-file el))))

(dolist (file fi-developer-files)
  (let ((el (format "%s.el" file))
	(elc (format "%s.elc" file)))
    (when (file-exists-p el)
      (unless (file-newer-than-file-p elc el)
	(message "--------------------------------------------------------")
	(byte-compile-file el)))))

;;(message "--------------------------------------------------------")
;;(message "doing readme.htm...")
;;(load "Doc0.elc")
;;(generate-eli-documentation "doc/eli.htm" "readme0.htm")
