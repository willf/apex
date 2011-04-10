;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/license.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: license.lisp,v 1.2 2006/01/15 03:43:01 dalal Exp $

;;; License agreement support

(in-package :user)

(defun check-license-agreement (&optional last-chance) ; Opt(bool) -> ()
  (when (not (agreement-signed?))
    (format t "To proceed, you must accept the Apex end user agreement.~%")
    (let ((read? (y-or-n-p "Do you wish to view the agreement? ")))
      (if read?
          (agree-with-license)
        (if last-chance
            (refuse-apex)
          (progn
            (format t "You have one more chance...~%")
            (check-license-agreement t)))))))

(defun agree-with-license () ; -> ()
  (print-agreement)
  (let ((confirm (y-or-n-p "~%Do you accept these terms? ")))
    (if confirm
        (progn
          (sign-agreement)
          (format t "Thank you and enjoy Apex!~2%"))
      (refuse-apex))
    (values)))

(defun agreement-signed? ()             ; -> bool
  (getf *apex-info* 'agreement-signed))

(defun sign-agreement ()                ; -> ()
  (setf (getf *apex-info* 'agreement-signed) t)
  (update-user-info-file))

(defun refuse-apex () ; -> ()
  (format t "~%Sorry, you may not use Apex unless you accept the agreement.~2%")
  (exit))

(defun print-agreement () ; -> ()
  (with-open-file (file "apex:agreement.txt" :direction :input)
    (print-all-lines file))
  (values))

(defun print-all-lines (file) ; stream -> ()
  (let ((line (read-line file nil)))
    (when line
      (format t "~a~%" line)
      (print-all-lines file))))
