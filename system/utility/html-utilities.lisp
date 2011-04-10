;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/html-utilities.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: html-utilities.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $
;;; Created:        September. 2004

;;;
;;; Trivial HTML functions.

(in-package :cl-user)

(defvar *html-escape-map*
    '((#\< . "&lt;")
      (#\> . "&gt;")
      (#\& . "&amp;")))

(defmethod  escape-html-entities ((string string) &optional (map *html-escape-map*))
  (with-output-to-string (str)
    (loop for ch character across string doing
	  (princ (or (cdr (assoc ch map :test #'char=)) ch) str))))

(defmethod  escape-html-entities ((thing t) &optional (map *html-escape-map*))
  (escape-html-entities (princ-to-string thing) map))

