;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/debug.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: debug.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $



(in-package :user)


;; the debug system is a simple text debugging messages system.  peper code
;; with debug calls, which are intended to be permenent.  you control which
;; messages are printed when through use of the "level" and "keys"
;; parameters passed to debug.
;;
;; the "level" is the level of detail as follows:
;;
;;    level 0 - stuff you want to see by default
;;    level 1 - stuff you want to see often
;;    level 2 - stuff you want to see less often
;;    etc.
;;
;; the keys are used to group messages.  debug messages in the "ar" project
;; might look like this:
;;
;;    (debug-message 0 'ar "x is ~a~%" x)
;;    (debug-message 1 'ar "sent message: ~a~%" message)
;;    (debug-message 4 'ar "at loop count: ~a~%" i)
;;
;; one or more keys may be passed to debug calls, thus one could use their
;; name in as a key in debug messages:
;;
;;    (debug-message 0 '(trebor ar) "x is ~a~%" x)
;;    (debug-message 0 '(trebor xplane) "y is ~a~%" y)
;;
;; to see these messages, use (debug-set) like this:
;;
;;    (debug-set 0 'trebor)       - see both messages above
;;    (debug-set 0 'ar)           - see ar messages only
;;    (debug-set 0 'x-plane)      - see xplane messages only
;;    (debug-set 0 '(x-plane ar)) - see both xplane and ar messages
;;
;; to save typing, create debug wrapper functions for sections
;; of code.  in the "ar" project you might do this:
;;
;;   (defun debug-ar (level &rest format-params)
;;     (apply #'debug (append
;;                    (list level '(trebor ar))
;;                    format-params)))
;;
;; then tebug messages in the "ar" project would look like this:
;;
;;    (debug-ar 0 "x is ~a~%" x)
;;    (debug-ar 1 "sent message: ~a~%" message)
;;    (debug-ar 4 "at loop count: ~a~%" i)
;;

;; debug detail level, the bigger the number the greater the detail

(defvar *debug-level* 0)

;; debug keys, one or more of the keys here must match those
;; in the debug call for debugging messages to print,
;; "*" matches all keys

(defvar *debug-keys* '*)

;; set the debug level

(defun debug-set (level keys)
  (setf *debug-level* level)
  (setf *debug-keys* keys))

;; a bunch of handy debug settings

(defun debug-off ()            (debug-set -1 nil))   ;; no debug messages
(defun debug-all ()            (debug-set 999 '*))   ;; all debug messags
(defun debug-all-level (level) (debug-set level '*)) ;; all at a given level
(defun debug-all-key (key)     (debug-set 999 key))  ;; all for a given key
(defun debug-default ()        (debug-set 0 '*))     ;; default settings

;; debug, print message if level and keys match global debug settings

(defun debug-message (level keys &rest format-params)
  (if (and (>= *debug-level* level)
           (or (eq *debug-keys* '*)
               (intersection (if (listp keys) keys (list keys))
                             (if (listp *debug-keys*)
                                 keys
                               (list *debug-keys*)))))
      (apply #'format (cons t format-params))))

