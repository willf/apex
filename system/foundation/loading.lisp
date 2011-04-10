;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/loading.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: loading.lisp,v 1.11 2006/02/15 15:21:59 will Exp $

;;; Interface and support for loading Apex applications and files

(in-package :common-lisp-user)

(defvar *update-load-history* t)

(defvar *last-application-file* nil)

(defmethod load-application-file (filespec)     ; string + pathname -> ()
  ;;  Load the top level application file with validity checks.
  ;;
  ;; It's overkill to check for licensing at this level, but this is a
  ;; workaround for the requirement of having the license check done by
  ;; Sherpa for the "typcial" user who won't run Apex in Lisp.  
  ;;
  (let ((file (pathname-to-string filespec)))
    (if (not (probe-file file)) (error "Application file ~a not found" file))
    (check-license-agreement)
    (reset-for-new-application)
    (load file)
    (if *update-load-history*
        (update-load-history
         (app-name *application*)
         (app-pathname *application*)))
    (load-libraries *application*)
    (unless (equal file *last-application-file*) (set-trace-level 'default))
    (load-files *application*)
    ;; Instance registry is cleared after each application run, but some
    ;; objects are created only at load time.  So save them.
    (save-instance-registry)
    (setq *last-application-file* file)
    (values)))


;;; ! This might be a temporary holding place for actions that are
;;; needed before a new application is loaded.

(defun reset-for-new-application ()
  (reset-instance-registry)
  (require-apex-library "default"))

;; Load history

(defun update-load-history (app-name app-file) ; string, pathname -> ()
  (verify-load-history-limit) ; ! hack
  (let ((new-load-history
         (take (getf *apex-info* 'load-history-limit)
               (remove-duplicates (cons (cons app-name app-file)
                                        (getf *apex-info* 'load-history))
                                  :test #'equal :from-end t))))
    (setf (getf *apex-info* 'load-history) new-load-history)
    (update-user-info-file)))

(defun change-load-history-size (n)     ; int -> ()
  (type-check change-load-history-size
              (n nat))
  (verify-load-history-limit)           ; ! hack
  (when (not (= n (getf *apex-info* 'load-history-limit)))
    (let ((new-load-history (take n (getf *apex-info* 'load-history))))
      (setf (getf *apex-info* 'load-history) new-load-history))
    (setf (getf *apex-info* 'load-history-limit) n)
    (update-user-info-file)))

(defun verify-load-history-limit () ; -> () ! Hack.
  ;;
  ;; For an unknown reason, there is sometimes no load history limit
  ;; entry in *apex-info*.  This function prints a warning and sets this
  ;; entry if so.
  ;;
  (when (null (getf *apex-info* 'load-history-limit))
    (setf (getf *apex-info* 'load-history-limit) *default-load-history-size*)
    (format t "Warning: load history limit was nil and reset to default.~%")
    (format t "Please inform apexhelp@eos.arc.nasa.gov that this happened.~%")))
  
(defun clear-load-history ()            ; -> ()
  (setf (getf *apex-info* 'load-history) nil)
  (update-user-info-file))
 
(defun get-recent-application-names () ; -> list(string)
  (mapcar #'car (getf *apex-info* 'load-history)))

(defun get-recent-application-pathnames () ; -> list(pathname)
  (mapcar #'cdr (getf *apex-info* 'load-history)))

(defun get-recent-application-names-and-pathnames () ; -> list(string)
  (zipwith
   #'(lambda (a b)
       (list a (format nil "~a" b))) ; the format converts pathname to string
   (get-recent-application-names)
   (get-recent-application-pathnames)))

(defun reloadapp ()
  (if *application*
      (load-application-file (app-pathname *application*))
    (let ((recent (get-recent-application-pathnames)))
      (if recent 
          (load-application-file (first recent))))))


(defmethod load-libraries ((app application))
  (dolist (lib (libraries app))
    (require-apex-library lib (app-pathname app))))
  
(defmethod load-files ((app application))
  (dolist (f (files app))
    (load f)))
