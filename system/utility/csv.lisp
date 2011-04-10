;;;-*- Mode: Lisp; Package:  :apex.utility.csv -*-
;;;
;;; apex/system/utility/csv.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: csv.lisp,v 1.3 2006/01/15 03:43:02 dalal Exp $
;;; Created:        December. 2004

;;;
;;;
;;; Code for reading and writing CSV files (commo-separated value) files.
;;;
;;;
;;; READ-CSV-FILE (pathname &optional (delimiter #\,))
;;; pathname|pathname string + character -> list(list(string))
;;; 
;;; Reads a CSV file and returns a list of list of strings of values.
;;; The empty string ("") is a 'null' value.
;;;
;;; WRITE-CSV-FILE (pathname (data sequence) &optional (delimiter #\,))
;;; pathname|pathname string + sequence(sequence(any)) + character -> (values)
;;; 
;;; Writes a CSV file of a sequence of a sequence of values.
;;;
;;; CSV-STRING ((object t))
;;; any -> string
;;; CSV-STRING ((object NULL)) 
;;; null -> string
;;;
;;; Returns a string form of an object. Specialize if necessary. Don't worry
;;; about CSV escaping, etc.; the code will do this for you. 
;;; Nil values are printed as empty strings.
;;;

(defpackage :apex.utility.csv
  (:use :common-lisp)
  (:export
   #:read-csv-file
   #:write-csv-file
   #:write-csv-records
   #:csv-string))

(in-package :apex.utility.csv)
   
(defmacro csv-while (condition &body body)
  `(do ()
       ((not ,condition))
     ,@body))

(defun newline-char-p (ch)
  (or (char= ch #\Newline)
      (char= ch #\Return)))

;;;
;;; Reading utilities
;;;

(defconstant +csv-quote+ #\")

(defmethod read-csv-field ((stream stream) (delimiter character))
  (let ((state :start)
	(end-of-record nil))
    (values 
     (with-output-to-string (ostr)
       (do ((ch (peek-char nil stream nil :eof)
		(peek-char nil stream nil :eof)))
	   ((or (eq state :done)
		(eq state :error)))
	 ;; (pvs! "in" state ch)
	 (ecase state
	   (:start 
	    (cond
	      ((or (eq ch :eof) (char= ch delimiter))
	       (read-char stream nil)
	       (when (eq ch :eof) (setq end-of-record t))
	       (setq state :done))
	      ((newline-char-p ch)
	       (csv-while 
	       (newline-char-p ch)
	       (read-char stream nil)
	       (setq ch (peek-char nil stream nil :eof)))
	       (setq end-of-record t)
	       (setq state :done))
	      ((char= ch +csv-quote+)
	       (read-char stream nil)
	       (setq state :collect-quoted))
	      (t 
	       (princ ch ostr)
	       (read-char stream nil)
	       (setq state :collect-unquoted))))
	   (:collect-unquoted
	    ;; (pvs! "in" state ch)
	    (cond
	      ((or (eq ch :eof) (char= ch delimiter))
	       (read-char stream nil)
	       (when (eq ch :eof) (setq end-of-record t))
	       (setq state :done))
	      ((newline-char-p ch)
	       (csv-while 
		(newline-char-p ch)
		(read-char stream nil)
		(setq ch (peek-char nil stream nil :eof)))
	       (setq end-of-record t)
	       (setq state :done))
	      (t 
	       (read-char stream nil)
	       (princ ch ostr)
	       (setq state :collect-unquoted))))
	   (:collect-quoted
	    ;; (pvs! "in" state)
	    (cond 
	     ((eq ch :eof)
	      (setq end-of-record t)
	      (setq state :error))
	     ((newline-char-p ch)
	      (csv-while 
	       (newline-char-p ch)
	       (read-char stream nil)
	       (setq ch (peek-char nil stream nil :eof)))
	       (princ #\Newline ostr)
	       (setq state :collect-quoted))
	     ((char= ch +csv-quote+)
	      (read-char stream nil)
	      (setq state :quote-in-quote))
	     (t
	      (read-char stream nil)
	      (princ ch ostr)
	      (setq state :collect-quoted))))
	   (:quote-in-quote
	    ;; (pvs! "in" state)
	    (cond
	     ((char= ch +csv-quote+)
	      (read-char stream nil)
	      (setq state :quote-in-quote-2))
	     (t 
	      (read-char stream nil)
	      (setq state :done))))
	   (:quote-in-quote-2
	    ;; (pvs! "in" state)
	    (cond
	     ((char= ch +csv-quote+)
	      (read-char stream nil)
	      (princ +csv-quote+ ostr)
	      (setq state :collect-quoted))
	     (t 
	      (setq state :error)))))
	 ;; (pvs! "Exiting ECASE" state)
	 ))
     state
     end-of-record)))

(defmethod read-csv-record ((stream stream) (delimiter character))
  (read-csv-record* stream delimiter '()))

(defmethod read-csv-record* ((stream stream) (delimiter character) (accum list))
  (multiple-value-bind (field state end-of-record)
      (read-csv-field stream delimiter)
    (cond
     ((eq state :error)
      (values (nreverse accum) state))
     (end-of-record
      (values (nreverse (cons field accum)) state))
     (t (read-csv-record* stream delimiter (cons field accum))))))

(defmethod read-csv-records ((stream stream) (delimiter character))
  (read-csv-records* stream delimiter '()))

(defmethod read-csv-records* ((stream stream) (delimiter character) (accum list))
  (if (eq (peek-char nil stream nil :eof) :eof)
    (nreverse accum)
    (read-csv-records* 
     stream 
     delimiter 
     (cons (read-csv-record stream delimiter) accum))))


(defun read-csv-file (pathname &optional (delimiter #\,))
  (with-open-file (stream pathname :direction :input)
    (read-csv-records stream delimiter)))



;;; --
;;; writing utilties
;;; --

;;; overwrite this to do special things....

(defmethod csv-string ((object t))
  (princ-to-string object))

(defmethod csv-string ((object null))
  "")

(defmethod write-csv-field ((stream stream) object)
  (let ((str (csv-string object)))
    (if (not (find-if (lambda (ch)
			(or (char= ch #\,)
			    (char= ch #\Newline)
			    (char= ch #\")))
		      str))
      ;; doesn't need quoting..
      (princ str stream)
      ;; needs quotiing ...
      (progn
	(princ #\" stream)
	(loop for ch across str doing
	      (if (char= ch #\")
		(princ "\"\"" stream)
		(princ ch stream)))
	(princ #\" stream)))))

(defmethod write-csv-record ((stream stream) (delimiter character) (record list))
  (loop for fields on record by #'cdr do
	(write-csv-field stream (car fields))
	(when (cdr fields)
	  (princ delimiter stream)))
  (terpri stream))

(defmethod write-csv-record ((stream stream) (delimiter character) (record vector))
  (let ((n (1- (length record))))
    (loop for i from 0 to (1- n) doing
	  (write-csv-field stream (elt record i))
	  (princ delimiter stream))
    (when (not (minusp n))
      (write-csv-field stream (elt record  n)))
    (terpri stream)))

(defmethod write-csv-records ((stream stream) (delimiter character) (records list))
  (dolist (record records)
    (write-csv-record stream delimiter record)))

(defmethod write-csv-records ((stream stream) (delimiter character) (records vector))
  (loop for record across records do
	(write-csv-record stream delimiter record)))

(defun write-csv-file (pathname data &optional (delimiter #\,))
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (write-csv-records stream delimiter data)))


;;; -- end of file.
