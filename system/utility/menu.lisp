;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/menu.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: menu.lisp,v 1.3 2006/01/15 03:43:02 dalal Exp $

;;; Text menu utilities


#| To do
Read entire input line
Add access to detailed documentation
|#

(in-package :common-lisp-user)

(defparameter *cancel-symbol* '/)

(defun toplevel-menu (intro prompt menu)
  ;;
  ;; String * String * Menu -> ()
  ;; Top level function for starting a prompt-driven interface.
  ;;
  (format t "~%~a~%" intro)
  (catch 'exit
    (loop
      (handler-case
          (process-menu-command prompt menu)
        (error (condition)
          (format t "~&;; Error ~a ignored, back to top level."
                  condition)))))
  (values))

(defun process-menu-command (prompt choices)   
  ;;
  ;; string * Menu -> ()
  ;; Take user input from a selection of menu commands and call the
  ;; function associated with the command.
  ;;
  (prompt prompt)
  (if (not (empty-input?))
      (let ((command (read)))
        (if (symbolp command)
            (let ((fn (lookup-command command choices)))
              (if fn
                  (funcall fn)
                (oops command)))
          (oops command))
        (values))))

(defun empty-input? ()  ; -> bool
  (let ((c (read-char)))
    (cond ((member c '(#\Space #\Tab))
           (empty-input?))
          ((eq #\Newline c) t)
          (t (unread-char c)
             nil))))

(defun get-menu-selection (title prompt candidates &key (singleton-menu? t))
  ;;
  ;; string, string, list(A), bool -> A
  ;; Prints a menu of given candidates and asks for input using given prompt.
  ;; Returns either the chosen candidate or NIL if input is cancelled.
  ;; If singleton-menu? is false, no menu is presented if only one menu
  ;; option -- that option is returned.
  ;;
  ;; NOTE: won't handle the case of NIL being a menu item, but who'd do that?
  ;;
  (let ((numcands (length candidates)))
    (if (and (= 1 numcands) (not singleton-menu?))
        (car candidates)
      (labels
          ((print-menu ()               ; -> string
             (terpri)
             (dolist (c candidates)
               (format t "~a) ~a~%" (index c) c))
             (terpri)
             (print-prompt-cancellable prompt "number"))
           (index (cand)                ; symbol -> int
             (1+ (- numcands
                    (length (member cand candidates :test #'equal))))))
        ;; Body
        (if title (format t "~%~a~%" title))
        (print-menu)
        (let ((sel (read)))
          (cond ((and (integerp sel) (> sel 0))
                 (let ((item (nth (1- sel) candidates)))
                   (if item
                       item
                     (progn
                       (oops sel)
                       (get-menu-selection title prompt candidates)))))
                ((eq sel *cancel-symbol*) nil)
                (t (oops sel)
                   (get-menu-selection title prompt candidates))))))))

(defun process-keyboard-input (prompt test action error-msg)
  ;;
  ;; string * (any -> bool) * (any -> ()) * string -> ()
  ;;
  ;; Prompt for user input.  Test it for validity.  If valid, take
  ;; action, else print error message and start over.
  ;;                               
  (print-prompt-cancellable prompt)
  (let ((input (read)))
    (terpri)
    (when (not (eq input *cancel-symbol*))
      (if (funcall test input)
          (progn
            (funcall action input)
            (values))
        (progn
          (format t "~a~%" error-msg)
          (process-keyboard-input prompt test action error-msg))))))

(defun prompt (p)                       ; string -> ()
  (format t "~%~a> " p)
  (values))

(defun oops (sel)                       ; symbol -> ()
  (format t "Invalid selection ~a~%" sel)
  (values))

(defun print-prompt-cancellable (message &optional input-desc)
  ;; string * string -> ()
  (format t "~a [enter ~a~a to cancel]: "
          message
          (if input-desc (format nil "~a, or " input-desc) "")
          *cancel-symbol*)
  (values))


;;; Abstract data type for CommandSpec

(defun lookup-command (command choices) ; Command -> FunctionName
  (dolist (c choices)
    (when (member command (commandspec-command c))
      (return (commandspec-function c)))))

(defun commandspec-command (cs) ; CommandSpec -> Command
  (first cs))

(defun commandspec-description (cs) ; CommandSpec -> Description
  (second cs))

(defun commandspec-function (cs) ; CommandSpec -> FunctionName
  (third cs))


;;; --------- Supporting functions

(defun explain (title choices)  ; string * Menu -> ()
  (labels
      ((format-commandspec (cs) ; list(symbol) -> string
         (cond ((null cs) "")
               (t (format nil "~(~a~)~a~a"
                          (symbol-name (car cs))
                          (if (cdr cs) "/" "")
                          (format-commandspec (cdr cs)))))))
    ;; Body
    (format t "~a~%~%" title)
    (dolist (c choices)
      (format t "  ~a ~15t- ~a~%"
              (format-commandspec (commandspec-command c))
              (commandspec-description c)))))

(defun not-implemented () ; -> ()
  (format t "not implemented~%")
  (values))

;;; The following not yet used.

(defun parse-line (line)     ; string -> List(Any)
  (multiple-value-bind (exp pos)
      (read-from-string line)
    (cond ((= pos (length line)) (list exp))
          (t (cons exp (parse-line (subseq line pos)))))))

(defun read-line-to-list ()             ; -> List(Any)
  (parse-line (read-line)))


;;; --------- Commonly used menu functions

(defun prompt-quit ()
  (throw 'exit nil))

(defun prompt-eval () ; -> ()
  ;; ! Needs some error handling
  (format t "~%~a~%" (eval (read))))


;;; Types
;;;   Menu         = List(CommandSpec)
;;;   CommandSpec  = (List(Command), Description, FunctionName)
;;;   Command      = symbol
;;;   Description  = string
;;;   FunctionName = symbol -- named function has type () -> ()
