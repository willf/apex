;;;-*- Mode: Lisp; Package:  :apex.utility.datetime -*-
;;;
;;; apex/system/utility/datetime.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: datetime.lisp,v 1.27 2006/01/15 03:43:02 dalal Exp $
;;; Created:        June, 2004


(defpackage :apex.utility.datetime
  (:use :common-lisp)
  (:use :inet.util.process)
  (:export
   #:current-milliseconds
   #:current-time
   #:current-year
   #:datetime
   #:make-datetime
   #:copy-datetime
   #:datetime-p
   #:datetime-millisecond
   #:datetime-second
   #:datetime-hour
   #:datetime-date
   #:datetime-month
   #:datetime-year
   #:datetime-tz-offset
   #:current-datetime
   #:datetime->milliseconds
   #:datetime->universal-time
   #:milliseconds->datetime
   #:univeral-time->datetime
   #:datetime=
   #:datetime/=
   #:datetime>
   #:datetime<
   #:datetime>=
   #:datetime<=
   #:datetime-difference
   #:datetime-add-duration
   #:datetime-subtract-duration
   #:8601-date-string
   #:8601-time-string
   #:8601-date-time-string
   #:datetime-princ
   #:datetime-read
   #:duration-princ
   #:duration-read
   #:duration-expression?
   #:with-epoch
   ;;; reading timepoints
   #:timepoint-read
;;; met
   #:reset-met-clock
   #:current-met
   #:with-met-clock
   #:met-princ
   #:ms #:msec #:msecs
   ;;; #:s
   #:sec #:secs #:second #:seconds
   #:m #:min #:mins #:minute #:minutes
   #:h #:hr #:hrs #:hour #:hours
   ;;; #:d 
   #:day #:days
   #:w #:week #:weeks
   #:mo #:mon #:month #:months
   ;;; #:y 
   #:yr #:year #:years
   #:interval
   #:interval-start
   #:interval-end
   #:interval-left
   #:interval-right
   #:make-interval
   #:copy-interval
   #:interval-string
   #:interval-p
   #:contains-p
   #:finishes-p
   #:starts-p
   #:before-p
   #:meets-p
   #:overlaps-p
   #:cotemporal-p
   #:during-p
   #:finished-by-p
   #:started-by-p
   #:after-p
   #:met-by-p
   #:overlapped-by-p
   #:sleep-parsing
   #:time-expression?
   #:quoted-time-expression?
   #:ms2hms
   )
  )

(in-package :apex.utility.datetime)


(defvar *the-epoch-in-lisp* (encode-universal-time 0 0 0 1 1 1970 0)
  "The number of seconds between 1900-01-01T00:00Z and 1970-01-10T00:00Z")

(defmacro with-epoch ((&optional epoch) &body body)
  `(let ((*the-epoch-in-lisp* (or ,epoch (get-universal-time))))
     (declare (special *the-epoch-in-lisp*))
     ,@body))

(defconstant *msecs-per-minute* (* 1000 60))
(defconstant *msecs-per-hour* (* 60 60 1000))
(defconstant *msecs-per-day* (* *msecs-per-hour* 24))
(defconstant *msecs-per-solar-year* (+ (* 365 *msecs-per-day*) (* 5 *msecs-per-hour*) (* 60 48)  46 ))
(defconstant *msecs-per-month* (floor (/ *msecs-per-solar-year* 12)))
(defconstant *msecs-per-week* (* *msecs-per-day* 7))

;;;(defun ms-table ()
;;;  (format t "~%Unit   number of millisconds.")
;;;  (format t "~%------ ----------------------")
;;;  (format t "~%Second 1000")
;;;  (format t "~%Minute ~S" *msecs-per-minute*)
;;;  (format t "~%Hour   ~S" *msecs-per-hour*)
;;;  (format t "~%Day    ~S" *msecs-per-day*)
;;;  (format t "~%Week   ~S" *msecs-per-week*)
;;;  (format t "~%Month  ~S" *msecs-per-month*)
;;;  (format t "~%Year   ~S" *msecs-per-solar-year*)
;;;  (values))

(defun get-current-milliseconds-portion ()
  "Millisecond portion of the current clock time."
  (mod (get-internal-real-time)
       internal-time-units-per-second))

(defun current-milliseconds ()
  "The number of seconds since 1970-01-01T00:00Z"
  (+ ( get-current-milliseconds-portion )
     (* 1000
	(- (get-universal-time)
	   *the-epoch-in-lisp*))))

;;;(defun current-time ()
;;;  (declare (inline current-milliseconds))
;;;  (current-milliseconds))

(defun current-year ()
  "What year is it?"
  (multiple-value-bind (secs min hrs day month year) 
      (get-decoded-time)
    (declare (ignore secs min hrs day month))
    year))

(defstruct (datetime (:print-function princ-datetime))
  (millisecond 0) (second 0) (minute 0) (hour 0) (date 1) (month 1) (year (current-year)) (tz-offset 0))

(defun copy-datetime-into (src dest)
  (setf (datetime-millisecond dest)
    (datetime-millisecond src))
  (setf (datetime-second dest)
    (datetime-second src))
  (setf (datetime-minute dest)
    (datetime-minute src))  
  (setf (datetime-hour dest)
    (datetime-hour src))  
  (setf (datetime-date dest)
    (datetime-date src))
  (setf (datetime-month dest)
    (datetime-month src))
  (setf (datetime-year dest)
    (datetime-year src))
  (setf (datetime-tz-offset dest)
    (datetime-tz-offset src))
  dest)
    
(defun get-locale-tz-offset ()
  "What's the local time zone offset? Use POSIX, not Lisp, number."
  (multiple-value-bind
      (second minute hour date month year day daylight-p zone)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour date month year day))
    (if daylight-p
	(- 1 zone)
      (- zone)))) 
       

(defun current-datetime (&optional (tz-offset 0))
  "What datetime is it right now?"
  (multiple-value-bind
      (second minute hour date month year)
      (decode-universal-time (get-universal-time)
       (- tz-offset))
    (make-datetime :millisecond (get-current-milliseconds-portion)
		    :second second 
		    :minute minute 
		    :hour hour 
		    :date date 
		    :month month 
		    :year year 
		    :tz-offset tz-offset)))


  
(defun datetime->milliseconds (datetime)
  "Convert datetime to milliseconds since 1970-01-01T00:00Z"
  (+ (datetime-millisecond datetime)
      (* 1000
	 (- (datetime->universal-time datetime)
	    *the-epoch-in-lisp*))))

(defun datetime->universal-time (datetime)
  "Convert datetime to seconds since 1900-01-01T00:00Z, also return milliscecond portion."
  (values (encode-universal-time 
	   (datetime-second datetime)
	   (datetime-minute datetime)
	   (datetime-hour datetime)
	   (datetime-date datetime)
	   (datetime-month datetime)
	   (datetime-year datetime)
	   (- (datetime-tz-offset datetime)))
	  (datetime-millisecond datetime)))

(defun %seconds+milliseconds->datetime (seconds milliseconds tz-offset)
  (let ((secs (+ seconds *the-epoch-in-lisp*)))
    (if (< secs 0)
	(error "Cannot convert ~S to datetime (too early)"
	       (+ milliseconds (- seconds *the-epoch-in-lisp*)))
      (multiple-value-bind 
	  (second minute hour date month year)
	  (decode-universal-time secs
				 (- tz-offset))
	(make-datetime :millisecond milliseconds
			:second second 
			:minute minute 
			:hour hour 
			:date date 
			:month month 
			:year year 
			:tz-offset tz-offset)))))

(defun milliseconds->datetime (milliseconds &optional (tz-offset 0))
  "Convert number of miilliseconds since 1970-01-01T00:00Z to datetime."
  (multiple-value-bind (secs ms)
      (floor milliseconds 1000)
    (%seconds+milliseconds->datetime secs ms tz-offset)))

(defun universal-time->datetime (universal-time &optional (tz-offset 0) (milliseconds 0))
  "Convert number of seconds since 1900-01-01T00:00Z to datetime."
  (%seconds+milliseconds->datetime (- universal-time *the-epoch-in-lisp*) milliseconds tz-offset))


;;; ---
;;; datetime comparisons. Note that we *have* to convert to milliseconds in most cases,
;;; because two  datetime representations may have the some millisecond representation; i.e.,
;;; if their timezones differ. And, of course, date arithmetic is hard, so it's better
;;; to convert to a number and use numeric comparisons.
;;;
;;; Inlining datetime->milliseconds *might* speed things up a bit. But if you're going
;;; to do a lot of comparisons, better to convert to milliseconds and use the math
;;; operators.
;;; ---

(defun datetime= (probe target)
  "Is PROBE at the same time as TARGET?"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (datetime-p target)))
  (if (eql (datetime-tz-offset probe)
	   (datetime-tz-offset target))
      (equalp probe target)
    (= (datetime->milliseconds probe)
       (datetime->milliseconds target))))

(defun datetime/= (probe target)
  "Is PROBE at a different time than TARGET?"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (datetime-p target)))
  (if (eql (datetime-tz-offset probe)
	   (datetime-tz-offset target))
      (not (equalp probe target))
    (/= (datetime->milliseconds probe)
	(datetime->milliseconds target))))

(defun datetime< (probe target)
  "Is PROBE at an earlier time than TARGET?"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (datetime-p target)))
  (< (datetime->milliseconds probe)
     (datetime->milliseconds target)))

(defun datetime> (probe target)
  "Is PROBE at an later time than TARGET?"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (datetime-p target)))
  (> (datetime->milliseconds probe)
     (datetime->milliseconds target)))

(defun datetime<= (probe target)
  "Is PROBE at the same, or earlier time than TARGET?"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (datetime-p target)))
  (<= (datetime->milliseconds probe)
      (datetime->milliseconds target)))

(defun datetime>= (probe target)
  "Is PROBE at the same or a later time than TARGET?"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (datetime-p target)))
  (>= (datetime->milliseconds probe)
      (datetime->milliseconds target)))

;;; duration : just a number of milliseconds.

(defun datetime-difference (probe target)
  "The duration between two points in time."
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (datetime-p target)))
  (- (datetime->milliseconds probe)
     (datetime->milliseconds target)))

(defun datetime-add-duration (probe duration)
  "Adds a duration to a datetime, and returns a new datetime (with the same tz-offset)"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (realp duration)))
  (milliseconds->datetime 
   (+ (datetime->milliseconds probe)
      duration)
   (datetime-tz-offset probe)))

(defun datetime-subtract-duration (probe duration)
  "Subtracts a duration to a datetime, and returns a new datetime (with the same tz-offset)"
  (declare (inline datetime->milliseconds))
  (assert (and (datetime-p probe)
	       (realp duration)))
  (milliseconds->datetime 
   (- (datetime->milliseconds probe)
      duration)
   (datetime-tz-offset probe)))


(defun princ-datetime (datetime stream depth)
  (declare (ignore depth))
  (print-unreadable-object (datetime stream :type t :identity t)
    (datetime-princ  datetime stream)))

;;;
;;; datetime formatters are functions of a datetime structure + a stream.
;;; they are keyed in the following table on a character.
;;; Formats look like: "%FT%T%z" (which prints an ISO-8601 formatted
;;; string. The first 'T' is literal; the 2nd T is escaped.
;;;
;;; This is based on a standard C call to print 'broken down time'.
;;; It's not the best, but it's a pretty standard general format.
;;;
;;; Here is a table of formats showing what the formats do, by example:
;;; (Created by calling DISPLAY-DATETME-FORMATS).
;;;
;;; Datetime: 2001-02-03T13:14:15.016-05:00 <-- Central Standard Time
;;; %%: [Prints a literal #\%.]
;;; %a: Sat
;;; %A: Saturday
;;; %b: Feb
;;; %B: Feb
;;; %c: Saturday, February 03, 2001 01:14:15 PM 
;;; %C: 20
;;; %d: 3
;;; %D: 03/02/01
;;; %e:  3
;;; %F: 2001-02-03
;;; %G: [defined, but does not display.]
;;; %g: [defined, but does not display.]
;;; %H: 13
;;; %h: Feb
;;; %I: 1
;;; %j: 34
;;; %k: 13
;;; %l:  1
;;; %M: 14
;;; %m: 2
;;; %n: [Prints a literal #\Newline.]
;;; %P: pm
;;; %p: PM
;;; %R: 13:14
;;; %r: Saturday, February 03, 2001 01:14:15 PM 
;;; %S: 15
;;; %s: 981224055
;;; %T: 13:14:15.016
;;; %t: [Prints a literal #\Tab.]
;;; %U: [defined, but does not display.]
;;; %u: 6
;;; %V: [defined, but does not display.]
;;; %X: 01:14:15  
;;; %x: Saturday, February 03, 2001
;;; %Y: 2001
;;; %y: 1
;;; %Z: [defined, but does not display.]
;;; %z: -05:00


(defvar *datetime-formatters* (make-hash-table :test #'eql))

(defun add-datetime-formatter (ch fun)
  (setf (gethash ch *datetime-formatters*) fun)
  ch)

(defun datetime-formatter (ch)
  (values (gethash ch *datetime-formatters*)))

(defun funcall-datetime-formatter (ch datetime stream)
  (let ((fun (datetime-formatter ch)))
    (if fun
	(funcall fun datetime stream)
      (error "No datetime formatter function for ~S." ch))))

(defmacro simple-datetime-formatter (accessor &optional width pad)
  (let ((base (if width (expt 10 (1- width)))))
    (cond
     ((and width pad)
      `#'(lambda (datetime stream)
	   (format stream "~V,V,d" ,width ,pad (mod (,accessor datetime) ,base))))
     (width
      `#'(lambda (datetime stream)
	   (format stream "~V,d" ,width (mod (,accessor datetime) ,base))))
     (t
      `#'(lambda (datetime stream)
	   (princ (,accessor datetime) stream))))))

(defmethod datetime-princ ((datetime datetime) &optional (stream *standard-output*) (format-string "%FT%T%z"))		
  (datetime-princ1 datetime format-string stream))

(defmethod datetime-princ1 ((datetime datetime) (format-string string) (stream (eql T)))
  (datetime-princ1 datetime format-string *standard-output*))

(defmethod datetime-princ1 ((datetime datetime)  (format-string string) (stream (eql NIL)))
  (with-output-to-string (str)
    (datetime-princ1 datetime format-string str)))

(defun princ-chars (string stream start end)
  (loop for i from start to (1- end)
      doing
	(princ (char string i) stream)))
			   
(defmethod datetime-princ1 ((datetime datetime)  (format-string string) (stream stream))
  (let ((pos (position #\% format-string)))
    (if (null pos)
	(princ format-string stream)
      (progn
	(when (> pos 0)
	  (princ-chars format-string stream 0 pos))
	(datetime-princ-helper datetime format-string stream (1+ pos)))))
  datetime)

(defun peek-string-char (string index eostring-value)
  (if (>= index (length string))
      eostring-value
    (char string index)))

(defun digit->integer (ch)
  (ecase ch
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)))

(defun datetime-princ-format-finder (datetime format-string stream pos)
  (let ((newpos (position #\% format-string :start pos :test #'char=)))
    (if (null newpos)
	(princ-chars format-string stream pos  (length format-string))
      (progn
	(princ-chars format-string stream pos newpos)
	(datetime-princ-helper datetime format-string stream (1+ newpos))))))

(defun datetime-princ-helper (datetime format-string stream pos)
  ;; we know we are *starting* just beyond '%' character.
  (let ((dispatcher (peek-string-char format-string pos :eos)))
    (when (eq dispatcher :eos)
      (error "Premature end to format string ~s at position ~a" format-string pos))
    (progn
      (funcall-datetime-formatter dispatcher datetime stream)
      (datetime-princ-format-finder datetime format-string stream (1+ pos)))))
	  
		  
	      
      
	
;;---
;; Definitions start here
;;---

;;; formatters must be a function of:
;;; datetime stream 
;;;
;;; Except where noted, the following comes from the GNU stftime
;;; function. 
;;;
;;; There is a lot of 'locale' support, which ACL supports,
;;; but not all Lisps do. So, for non-Allegro implementations,
;;; we assume a US-centric view.
;;;
;;;
;;; %a The abbreviated weekday name according to the current locale.
;;;

#+allegro(defun abbreviated-weekday-name (datetime)
       (with-output-to-string (str)
	 (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%a")))

#-allegro(defun abbreviated-weekday-name (datetime)
  (svref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") (datetime-dow datetime)))

(add-datetime-formatter #\a (simple-datetime-formatter abbreviated-weekday-name))

;;; %A The full weekday name according to the current locale.
;;;


#+allegro(defun full-weekday-name (datetime)
       (with-output-to-string (str)
	 (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%A")))

#-allegro(defun full-weekday-name (datetime)
  (svref '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") (datetime-dow datetime)))

(add-datetime-formatter #\A (simple-datetime-formatter full-weekday-name))


;;; %b The abbreviated month name according to the current locale.
;;;
#+allegro(defun abbreviated-month-name (datetime)
       (with-output-to-string (str)
	 (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%b")))

#-allegro(defun abbreviated-month-name (datetime)
	   (svref
	    '#("""Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
		  (datetime-month datetime)))

(add-datetime-formatter #\b (simple-datetime-formatter abbreviated-month-name))

;;; %B The full month name according to the current locale.
;;;
#+allegro(defun full-month-name (datetime)
       (with-output-to-string (str)
	 (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%B")))

#-allegro(defun full-month-name (datetime)
	   (svref
	    '#("""January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
	    (datetime-month datetime)))

(add-datetime-formatter #\B (simple-datetime-formatter abbreviated-month-name))

;;; %c The preferred calendar time representation for the current locale.
;;;
#+allegro(defun locale-c-string (datetime)
	   (with-output-to-string (str)
	     (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%c")))

#-allegro(defun locale-c-string (datetime)
	   (format nil "~a, ~a ~2,'0,a, ~4,'0,a ~2,'0,a:~2,'0,a:~2,'0,a ~a"
		   (full-weekday-name datetime)
		   (full-month-name datetime)
		   (datetime-date datetime)
		   (datetime-year datetime)
		   (mod (datetime-hour datetime) 12)
		   (datetime-minute datetime)
		   (datetime-second datetime)
		   (if (or (=  (datetime-hour datetime) 0)
			   (> (datetime-hour datetime) 11))
		       "PM" "AM")))

(add-datetime-formatter #\c (simple-datetime-formatter locale-c-string))

;;; %C The century of the year. This is equivalent to the greatest
;;;integer not greater than the year divided by 100.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
(defun datetime-century-of-year (datetime)
  (floor (datetime-year datetime) 100))

(add-datetime-formatter #\C (simple-datetime-formatter datetime-century-of-year))
;;;
;;; %d The day of the month as a decimal number (range 01 through 31).

(add-datetime-formatter #\d (simple-datetime-formatter datetime-date))
;;;
;;; %D The date using the format %m/%d/%y.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
(defun datetime-D-string (datetime)
  (format nil "~2,'0,d/~2,'0,d/~2,'0,d"
	  (datetime-date datetime)
	  (datetime-month datetime)
	  (mod (datetime-year datetime) 100)))
(add-datetime-formatter #\D (simple-datetime-formatter datetime-D-string))

;;;
;;; %e The day of the month like with %d, but padded with blank (range 1
;;;through 31).
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
;;;
(defun datetime-e-string (datetime)
  (format nil "~2,' ,d" (datetime-date datetime)))

(add-datetime-formatter #\e (simple-datetime-formatter datetime-e-string))

;;; %F The date using the format %Y-%m-%d. This is the form specified in
;;;the ISOÊ8601 standard and is the preferred form for all uses.
;;;
;;; This format was first standardized by ISOÊC99 and by POSIX.1-2001.
(defun datetime-F-string (datetime)
  (format nil "~4,'0,d-~2,'0,d-~2,'0,d"
	  (datetime-year datetime)
	  (datetime-month datetime)
	  (datetime-date datetime)))

(add-datetime-formatter #\F (simple-datetime-formatter datetime-F-string))

;;;-- WF: The following are NOT supported, only consumed.
;;;
;;; %g The year corresponding to the ISO week number, but without the
;;;century (range 00 through 99). This has the same format and value as
;;;%y, except that if the ISO week number (see %V) belongs to the
;;;previous or next year, that year is used instead.
;;;
;;; This format was first standardized by ISOÊC99 and by POSIX.1-2001.
;;;
;;; %G The year corresponding to the ISO week number. This has the same
;;;format and value as %Y, except that if the ISO week number (see %V)
;;;belongs to the previous or next year, that year is used instead.
;;;
;;; This format was first standardized by ISOÊC99 and by POSIX.1-2001 but
;;; was previously available as a GNU extension.
;;;
(defun datetime-null-string (datetime)
  (declare (ignore datetime))
  "")

(add-datetime-formatter #\g  (simple-datetime-formatter datetime-null-string))
(add-datetime-formatter #\G  (simple-datetime-formatter datetime-null-string))

;;; %h The abbreviated month name according to the current locale. The
;;;action is the same as for %b.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
;;;
(add-datetime-formatter #\h (simple-datetime-formatter abbreviated-month-name))

;;; %H The hour as a decimal number, using a 24-hour clock (range 00
;;;through 23).
;;;
(add-datetime-formatter #\H (simple-datetime-formatter datetime-hour))

;;; %I The hour as a decimal number, using a 12-hour clock (range 01
;;;through 12).
(defun datetime-12-hour (datetime)
  (let ((tt (mod (datetime-hour datetime) 12)))
    (if (= tt 0) 12 tt)))

(add-datetime-formatter #\I (simple-datetime-formatter datetime-12-hour))
;;;
;;; %j The day of the year as a decimal number (range 001 through 366).
;;;
(add-datetime-formatter #\j (simple-datetime-formatter datetime-doy))
;;; %k The hour as a decimal number, using a 24-hour clock like %H, but
;;;padded with blank (range 0 through 23).
;;;
;;; This format is a GNU extension.
;;;
(defun datetime-k-string (datetime)
  (format nil "~2,' ,d" (datetime-hour datetime)))

(add-datetime-formatter #\k (simple-datetime-formatter datetime-k-string))

;;; %l The hour as a decimal number, using a 12-hour clock like %I, but
;;;padded with blank (range 1 through 12).
(defun datetime-l-string (datetime)
  (format nil "~2,' ,d" (datetime-12-hour datetime)))

(add-datetime-formatter #\l (simple-datetime-formatter datetime-l-string))
;;;
;;; This format is a GNU extension.
;;;
;;; %m The month as a decimal number (range 01 through 12).
;;;
(add-datetime-formatter #\m (simple-datetime-formatter datetime-month))
;;; %M The minute as a decimal number (range 00 through 59).
;;;
(add-datetime-formatter #\M (simple-datetime-formatter datetime-minute))
;;; %n A single \n (newline) character.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
;;;
(add-datetime-formatter #\n 
			#'(lambda (datetime stream)
			    (declare (ignore datetime))
			    (terpri stream)))

;;; %p Either AM or PM, according to the given time value; or the
;;;corresponding strings for the current locale. Noon is treated as PM
;;;and midnight as AM. In most locales AM/PM format is not supported, in
;;;such cases "%p" yields an empty string.
#+allegro(defun datetime-capital-meridian-string (datetime)
	   (with-output-to-string (str)
	     (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%p")))
#-allegro(defun datetime-capital-meridian-string (datetime)
	   (if (or (=  (datetime-hour datetime) 0)
		   (> (datetime-hour datetime) 11))
	       "PM" "AM"))
(add-datetime-formatter #\p (simple-datetime-formatter datetime-capital-meridian-string))
;;;
;;; %P Either am or pm, according to the given time value; or the
;;;corresponding strings for the current locale, printed in lowercase
;;;characters. Noon is treated as pm and midnight as am. In most locales
;;;AM/PM format is not supported, in such cases "%P" yields an empty
;;;string.
;;;
;;; This format is a GNU extension.
;;;
(defun datetime-lowercase-meridian-string (datetime)
  (string-downcase (datetime-capital-meridian-string datetime)))

(add-datetime-formatter #\P (simple-datetime-formatter datetime-lowercase-meridian-string))
;;; %r The complete calendar time using the AM/PM format of the current
;;;locale.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99. In
;;; the POSIX locale, this format is equivalent to %I:%M:%S %p.
;;;
#+allegro(defun datetime-r-string (datetime)
	   (with-output-to-string (str)
	     (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%r")))

#-allegro(defun datetime-r-string (datetime)
	   (format nil "~a, ~a ~2,'0,a, ~4,'0,a ~2,'0,a:~2,'0,a:~2,'0,a ~a"
		   (full-weekday-name datetime)
		   (full-month-name datetime)
		   (datetime-date datetime)
		   (datetime-year datetime)
		   (mod (datetime-hour datetime) 12)
		   (datetime-minute datetime)
		   (datetime-second datetime)
		   (if (or (=  (datetime-hour datetime) 0)
			   (> (datetime-hour datetime) 11))
		       "PM" "AM")))

(add-datetime-formatter #\r (simple-datetime-formatter datetime-r-string))

;;; %R The hour and minute in decimal numbers using the format %H:%M.
;;;
;;; This format was first standardized by ISOÊC99 and by POSIX.1-2001 but
;;; was previously available as a GNU extension.
;;;
(defun datetime-capital-r-string (datetime)
  (format nil "~2,'0,d:~2,'0,d" (datetime-hour datetime) (datetime-minute datetime)))

(add-datetime-formatter #\R (simple-datetime-formatter datetime-capital-r-string))

;;; %s The number of seconds since the epoch, i.e., since 1970-01-01
;;;00:00:00 UTC. Leap seconds are not counted unless leap second support
;;;is available.
;;;
;;; This format is a GNU extension.
;;;
(defun datetime-seconds-since-epoch (datetime)
  (floor (datetime->milliseconds datetime) 1000))

(add-datetime-formatter #\s (simple-datetime-formatter datetime-seconds-since-epoch))

;;; %S The seconds as a decimal number (range 00 through 60).
;;;
(add-datetime-formatter #\S (simple-datetime-formatter datetime-second))

;;; %t A single \t (tabulator) character.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
;;;
(add-datetime-formatter #\t
			#'(lambda (datetime stream)
			    (declare (ignore datetime))
			    (princ #\Tab stream)))
;;; %T The time of day using decimal numbers using the format %H:%M:%S.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
;;; wf: I add milliseconds, so there.
(defun datetime-T-string (datetime)
  (if (= (datetime-millisecond datetime) 0)
      (format nil "~2,'0,d:~2,'0,d:~2,'0,d"
	      (datetime-hour datetime)
	      (datetime-minute datetime)
	      (datetime-second datetime))
    (format nil "~2,'0,d:~2,'0,d:~2,'0,d.~3,'0,d"
	      (datetime-hour datetime)
	      (datetime-minute datetime)
	      (datetime-second datetime)
	      (datetime-millisecond datetime) 10)))

(add-datetime-formatter #\T (simple-datetime-formatter datetime-T-string))

;;; %u The day of the week as a decimal number (range 1 through 7),
;;;Monday being 1.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
;;;
(add-datetime-formatter #\u (simple-datetime-formatter datetime-dow))

;;;--- WF: more unsupported formats!
;;; %U The week number of the current year as a decimal number (range 00
;;;through 53), starting with the first Sunday as the first day of the
;;;first week. Days preceding the first Sunday in the year are considered
;;;to be in week 00.
;;;
;;; %V The ISOÊ8601:1988 week number as a decimal number (range 01
;;;through 53). ISO weeks start with Monday and end with Sunday. Week 01
;;;of a year is the first week which has the majority of its days in that
;;;year; this is equivalent to the week containing the year's first
;;;Thursday, and it is also equivalent to the week containing January
;;;4. Week 01 of a year can contain days from the previous year. The week
;;;before week 01 of a year is the last week (52 or 53) of the previous
;;;year even if it contains days from the new year.
;;;
;;; This format was first standardized by POSIX.2-1992 and by ISOÊC99.
(add-datetime-formatter #\U  (simple-datetime-formatter datetime-null-string))
(add-datetime-formatter #\V  (simple-datetime-formatter datetime-null-string))

;;;%x The date using the locale's date format. 
;;;
;;;%Ex Like %x but the locale's alternative data representation is used.
;;;
#+allegro(defun datetime-x-string (datetime)
	   (with-output-to-string (str)
	     (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%x")))

#-allegro(defun datetime-x-string (datetime)
	   (format nil "~a, ~a ~2,'0,a, ~4,'0"
		   (full-weekday-name datetime)
		   (full-month-name datetime)
		   (datetime-date datetime)
		   (datetime-year datetime)))

(add-datetime-formatter #\x (simple-datetime-formatter datetime-x-string))

;;;%X The time using the locale's time format.
;;;
;;;%EX Like %X but the locale's alternative time representation is used.
;;;
+allegro(defun datetime-capital-x-string (datetime)
	   (with-output-to-string (str)
	     (excl:locale-format-time str (datetime->universal-time datetime) t t excl:*locale* "%X")))

#-allegro(defun datetime-capital-x-string (datetime)
	   (format nil "~2,'0,a:~2,'0,a:~2,'0,a"
		   (datetime-hour datetime)
		   (datetime-minute datetime)
		   (datetime-second datetime)))

(add-datetime-formatter #\X (simple-datetime-formatter datetime-capital-X-string))

;;;%y The year without a century as a decimal number (range 0 through
;;;99).
;;;
;;; Leading zeroes are permitted but not required.
;;;
;;; Note that it is questionable to use this format without the %C
;;; format. The strptime function does regard input values in the range
;;; 68 to 99 as the years 1969 to 1999 and the values 0 to 68 as the
;;; years 2000 to 2068. But maybe this heuristic fails for some input
;;; data.
;;;
;;; Therefore it is best to avoid %y completely and use %Y instead. 
;;;
;;;%Ey The offset from %EC in the locale's alternative representation.
;;;
;;;%Oy The offset of the year (from %C) using the locale's alternative
;;;numeric symbols.
;;;
(defun datetime-year-mod-100 (datetime)
  (mod (datetime-year datetime) 100))

(add-datetime-formatter #\y (simple-datetime-formatter datetime-year-mod-100))

;;;%Y The year as a decimal number, using the Gregorian calendar.
;;;
(add-datetime-formatter  #\Y (simple-datetime-formatter datetime-year))
;;;%EY The full alternative year representation.
;;;
;;;%z The offset from GMT in ISOÊ8601/RFC822 format.

(defun datetime-timezone-iso860-string (datetime)
  (let ((tz (datetime-tz-offset datetime)))
    (if (= tz 0) "Z"
      (let ((sign (signum tz))
	    (tz (abs tz)))
	(multiple-value-bind (hrs portion)
	    (floor tz)
	  (let ((minutes (floor (* portion 60))))
	    (format nil "~a~2,'0,d:~2,'0,d"
		    (if (< sign 0) "-" "+")
		    hrs
		    minutes)))))))

(add-datetime-formatter #\z (simple-datetime-formatter datetime-timezone-iso860-string))
;;;
;;;%Z The timezone name.
;;;
;;; Note: Currently, this is not fully implemented. The format is
;;; recognized, input is consumed but no field in tm is set.
;;;
;;; -- WF: It's not even well defined.
(add-datetime-formatter #\Z  (simple-datetime-formatter datetime-null-string))
;;;%% A literal % character.

(add-datetime-formatter #\%
			#'(lambda (datetime stream)
			    (declare (ignore datetime))
			    (princ #\% stream)))


(defun display-datetime-formats (&optional (ct (make-datetime :millisecond 16 :second 15 :minute 14 :hour 13 :date 3 :month 2 :year 2001 :tz-offset -5)))
  (let ((formats (list)))
    (maphash #'(lambda (k v)
		 (push (cons k v) formats))
	     *datetime-formatters*)
    (setq formats (sort formats #'char-lessp :key #'car))
    ; (format t "Formatters (sorted): ~S~%" formats) 
    (format t ";;; Datetime: ~a~%" (datetime-princ1 ct "%FT%T%z" nil))
    (dolist (fmt formats)
      (let* ((ch (car fmt))
	    (fmt-str (format nil "%~a" ch)))
	(format t ";;; ~a: " fmt-str)
	(let ((str (datetime-princ1 ct fmt-str nil)))
	  (cond
	   ((= (length str) 0)
	    (format t "[defined, but does not display.]"))
	   ((= (length str) 1)
	    (if (or (char= (char str 0) #\Newline)
		    (char= (char str 0) #\Tab)
		    (char= (char str 0) #\%))
		(format t "[Prints a literal ~S.]" (char str 0))
	      (princ str)))
	   (t (princ str))))
	(terpri)))))

(defun datetime-dow (datetime) ;; i.e., the day in the week. 1-Monday 
  (multiple-value-bind (isecond iminute ihour idate imonth iyear day)
      (decode-universal-time
       (datetime->universal-time datetime))
      (declare (ignore isecond iminute ihour idate imonth iyear))
      (1+ day)))

(defun leap-yearp (year)
  (or (= (rem year 400) 0)
      (and (= (rem year 4) 0) (not (= (rem year 100) 0)))))

(defun year-day (day month year)
  (let ((days-pr 
	 (assoc (1- month)
		'((0 . 0) (1 . 31) (2 . 59) (3 . 90) (4 . 120) 
		  (5 . 151) (6 . 181) (7 . 212) (8 . 243) 
		  (9 . 273) (10 . 304) (11 . 334))
		:test #'=)))
    (if (not days-pr)
      (error "Invalid month: ~S"  days-pr)
      (if (and (leap-yearp year) (> month 2))
	(+ day (cdr days-pr) 1)
	(+ day (cdr days-pr))))))


(defun days-in-month (month year)
  (let ((days-pr 
	 (assoc month 
		'((1 . 31)  (2 . 28) (3 . 31) (4 . 30)
		  (5 . 31) (6 . 30) (7 . 31) (8 . 31)
		  (9 . 30) (10 . 31) (11 . 30) (12 . 31))
		:test #'=)))
    (if (not days-pr)
      (error "Invalid month: ~S"  days-pr)
      (if (and (leap-yearp year) (= month 2))
	29
	(cdr days-pr)))))

(defun datetime-doy (datetime)
  (let ((year (datetime-year datetime))
	(month (datetime-month datetime))
	(day (datetime-date datetime)))
    (year-day day month year)))




;;; --------------------------------
;;; READ-DATE
;;; --------------------------------

(defun util-intern (str)
  (intern str (find-package :apex.utility.datetime)))

(defmethod datetime-read ((date-string string) &key (format-string "<iso-8601>")  (datetime (make-datetime :tz-offset 0)))
  (with-input-from-string (dstream date-string)
    (with-input-from-string (fstream format-string)
      (let ((datetime (read-formatted-date-stream dstream fstream datetime)))
	(multiple-value-bind (ms err)
	    (ignore-errors (datetime->milliseconds datetime))
	  (declare (ignore ms))
	  (if err 
	      (error "Unable to read date ~S" date-string)
	    datetime))))))
  
(defmethod datetime-read ((dstream stream)  &key (format-string "<iso-8601>")  (date (make-datetime :tz-offset 0)))
  (with-input-from-string (fstream format-string)
    (let ((datetime (read-formatted-date-stream dstream fstream date)))
	(multiple-value-bind (ms err)
	    (ignore-errors (datetime->milliseconds datetime))
	  (declare (ignore ms))
	  (if err 
	      (error "Unable to read date from stream.")
	    datetime)))))

(defun read-tag-and-formats (fstream)
  (let* ((tags (make-string-output-stream))
	 (formats (make-string-output-stream))
	 (currs tags))
    (do  ((fch (peek-char nil fstream nil :feof)
	       (peek-char nil fstream nil :feof)))
	((or (eq fch :feof)(eq fch #\>))
	 (if (eq fch :feof) 
	   (error "Premature end of date string.")
	   (read-char fstream)) ;; consume #\>
	 )
      (let ((ch (read-char fstream)))
	(if (char= ch #\~)
	  (setq currs formats)
	  (princ ch currs))))
    (let ((tagst (get-output-stream-string tags))
	  (formatst (get-output-stream-string formats)))
      (values
       (if (string= tagst "") (error "No tag specified in format string.")
	   (util-intern (nstring-upcase tagst)))
       (if (string= formatst "") NIL
	   formatst)))))
    
    
(defun read-formatted-date-stream (dstream fstream date)
  (do ((dch (peek-char nil dstream nil :deof)
	    (peek-char nil dstream nil :deof))
       (fch (peek-char nil fstream nil :feof)
	    (peek-char nil fstream nil :feof)))
      ((or (eq dch :deof) (eq fch :feof))
       date)
    (cond
     ((char= fch #\<)
      (read-char fstream) ;; consume #\<
      (multiple-value-bind (tag params)
	  (read-tag-and-formats fstream)
	(datetime-reader dstream date tag params)  ;; note EQL specializers below!
	  ))
     ((char= dch fch)
      (read-char fstream) 
      (read-char dstream))
     (T (error "Mismatched date and format strings.")))))

;; -- utility functions

(defun read-int-n-characters (n stream)
  (let ((acc 0))
    (do ((ch (peek-char nil stream nil :eof)
	     (peek-char nil stream nil :eof))
	 (cnt 0 (incf cnt)))
	((or (eq ch :eof)
	     (>= cnt n)
	     (not (digit-char-p ch))) (values acc cnt))
      (let ((dval (digit-char-p (read-char stream))))
	(if dval
	    (setq acc (+ (* acc 10) dval)))))))


(defun read-int (stream)
  (let ((acc 0) 
	(read-one NIL))
    (do ((ch (peek-char nil stream nil :eof)
	     (peek-char nil stream nil :eof))
	 (cnt 0 (incf cnt)))
	((or (eq ch :eof)
	     (not (digit-char-p ch))) (values (if read-one acc :eof) cnt))
      (let ((dval (digit-char-p (read-char stream))))
	(when dval
	  (unless read-one (setq read-one t))
	  (setq acc (+ (* acc 10) dval)))))))

(defun read-string-until (stream test)
  (let ((acc (make-array  0 :adjustable t :element-type 'character :fill-pointer 0))
	(read-one NIL))
    (do ((ch (peek-char nil stream nil :eof)
	     (peek-char nil stream nil :eof)))
	((or (eq ch :eof)
	     (not (funcall test ch)))
	 (if read-one (coerce acc 'string) :eof))
      (unless read-one (setq read-one T))
      (vector-push-extend (read-char stream) acc))))


(defun current-year-month ()
  (multiple-value-bind (secs min hrs day month year) 
      (decode-universal-time (get-universal-time))
    (declare (ignore secs min hrs day))
    (values year month)))

;;;(defun current-year ()
;;;  (multiple-value-bind (secs min hrs day month year) 
;;;      (get-decoded-time)
;;;    (declare (ignore secs min hrs day month))
;;;    year))

(defun current-century ()
  (* 100 (floor (current-year) 100)))

(defun natural-year (n)
  (let* ( (current-year (current-year))
	  (current-century (current-century)) )
    (cond
     ((>= n 100) n)
     ((<  n 0) n)
     ((<=  (- (+ current-century n) current-year) 50)
      (+ current-century n))
     (T
      (+ (- current-century 100) n)))))

(defmethod datetime-reader ((stream stream) (datetime datetime) latch params)
  (declare (ignore params))
  (error "No reader defined for tag ~S" latch))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'year)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 4 stream)))
    (if i (setf (datetime-year datetime) i))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'two-digit-year)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 2 stream)))
    (if i (setf (datetime-year datetime) (natural-year i)))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'month)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 2 stream)))
    (if i (setf (datetime-month datetime) i))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'date)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 2 stream)))
    (if i (setf (datetime-date datetime) i))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'day)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 2 stream)))
    (if i (setf (datetime-date datetime) i))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'hour)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 2 stream)))
    (if i (setf (datetime-hour datetime) i))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'minute)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 2 stream)))
    (if i (setf (datetime-minute datetime) i))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'second)) params)
  (declare (ignore params))
  (let ((i (read-int-n-characters 2 stream)))
    (if i (setf (datetime-second datetime) i))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'optional-second)) params)
  (declare (ignore params))
  (let ((ch (peek-char t stream nil :eof)))
    (if (eq ch :eof) 
	(setf (datetime-second datetime) 0)
      (let ((i (read-int-n-characters 2 stream)))
	(if i (setf (datetime-second datetime) i))))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'optional-millisecond)) params)
  (let ((ch (peek-char t stream nil :eof)))
    (if (or (eq ch :eof)
	    (not (or (char= ch #\.)
		     (char= ch #\,))))
      (setf (datetime-millisecond datetime) 0)
      (progn
	(read-char stream) ;; eat decimal
      (datetime-reader stream datetime 'millisecond params)))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'millisecond)) params)
  (declare (ignore params))
  (multiple-value-bind (int pos)
      (read-int-n-characters 3 stream)
    (declare (ignore pos)) ;; thought i would need this. i was wrong
    (when int
      (setf (datetime-millisecond datetime) int))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'next-digit)) params)
  (declare (ignore params))
  (do ((ch (peek-char nil stream nil :eof)
	   (peek-char nil stream nil :eof)))
      ((or (eq ch :eof)
	   (digit-char-p ch)))
    (read-char stream)))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'exact-char)) param)
  (let ((ch (peek-char nil stream nil :eof)))
    (if (eql ch param)
      (read-char stream)
      (error "Expecting next character to be ~s; got ~s instead"
	     param
	     ch))))


(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'next-digit-or-sign)) params)
  (declare (ignore params))
  (do ((ch (peek-char nil stream nil :eof)
	   (peek-char nil stream nil :eof)))
      ((or (eq ch :eof)
	   (char= ch  #\+)
	   (char= ch #\-)
	   (digit-char-p ch)))
    (read-char stream)))

(defmethod eos-p ((stream stream))
  (eq :eof (peek-char nil stream nil :eof)))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'tz)) params)
  (declare (ignore params))
  (let ((ch (peek-char t stream nil :eof))
	(pos T))
    (if (char-equal ch #\Z)
      (progn
	(setf (datetime-tz-offset datetime) 0)
	(read-char stream))
      (progn
	(if (char-equal ch #\-)
	  (setq pos NIL)
	  (if (char-equal ch #\+)
	    (setq pos T)
	    (error "Time zone must begin with Z or +/-; not ~S" ch)))
	(read-char stream) ;; consume sign
	(let ((hr (read-int-n-characters 2 stream)))
	  (datetime-reader stream datetime 'next-digit nil)
	  (let ((min (read-int-n-characters 2 stream)))
	    (setf (datetime-tz-offset datetime)
	      (funcall (if pos #'+ #'-)
		       (/ (+ (* hr 60) min) 60)))))))))





(defconstant *rfc-tz-names* 
    '(("UT" 0) 
      ("GMT" 0)
      ("EST" -5)
      ("EDT" -4)
      ("CST" -6)
      ("CDT" -5)
      ("MST" -7)
      ("MDT" -6)
      ("PST" -8)
      ("PDT" -7)
      ("A" 1)
      ("B" 2)
      ("C" 3)
      ("D" 4)
      ("E" 5)
      ("F" 6)
      ("G" 7)
      ("H" 8)
      ("I" 9) ;; no J
      ("K" 10)
      ("L" 11) 
      ("M" 12)
      ("N" -1)
      ("O" -2)
      ("P" -3)
      ("Q" -4)
      ("R" -5)
      ("S" -6)
      ("T" -7)
      ("U" -8)
      ("V" -9)
      ("W" -10)
      ("X" -11)
      ("Y" -12)
      ("Z" 0)))


(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'rfc-822-tz)) params)
  (declare (ignore params))
  (let ((ch (peek-char t stream nil :eof))
	(pos T)
	(short NIL))
    (if (char-equal ch #\-)
      (progn (setq pos NIL) (setq short T))
      (if (char-equal ch #\+)
	(progn (setq pos T) (setq short t))))
    (if (not short) ;; didn't start with +/-
      (let* ((month (symbol-name (read stream nil :eof)))
	     (asso (assoc month *rfc-tz-names* :test #'string-equal)))
	(if asso
	    (setf (datetime-tz-offset datetime) (cadr asso))
	  (error "Invalid RFC 866 Time Zone.")))
      (progn
	(read-char stream) ;; consume sign
	(let ((hr (read-int-n-characters 2 stream)))
	  (datetime-reader stream datetime 'next-digit nil)
	  (let ((min (read-int-n-characters 2 stream)))
	    (setf (datetime-tz-offset datetime)
	      (funcall (if pos #'+ #'-)
		       (/ (+ (* hr 60) min) 60)))))))))
	    

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'optional-tz)) params)
  (let ((ch (peek-char t stream nil :eof)))
    (unless (eq ch :eof)
      (datetime-reader stream datetime 'tz params))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'iso-8601)) params)
  (declare (ignore params))
  (datetime-reader stream datetime 'iso-8601-date nil)
  (datetime-reader stream datetime 'iso-8601-time nil)
  )

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'iso-8601-date)) params)
  (declare (ignore params))
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'year nil)
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'month nil)
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'date nil)
  )

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'iso-8601-time)) params)
  (declare (ignore params))
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'hour nil)
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'minute nil)
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'second nil)
  (datetime-reader stream datetime 'optional-millisecond nil)
  (datetime-reader stream datetime 'optional-tz nil))
  
(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'iso-8601-time/stricter)) params)
  (declare (ignore params))
  (datetime-reader stream datetime 'hour nil)
  (datetime-reader stream datetime 'exact-char #\:)
  (datetime-reader stream datetime 'minute nil)
  (datetime-reader stream datetime 'exact-char #\:)
  (datetime-reader stream datetime 'second nil)
  (datetime-reader stream datetime 'optional-millisecond nil)
  (datetime-reader stream datetime 'optional-tz nil))
  
(defmethod read-n-chars ((stream stream) n)
  (with-output-to-string (str)
    (dotimes (i n)
      (princ (read-char stream) str))
    str))
  
(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'short-month)) params)
   (declare (ignore params))
  (let ((months '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	(month (symbol-name (read-from-string (read-n-chars stream 3)))))
    (let ((m (position month months :test #'string-equal)))
      (if m
	(setf (datetime-month datetime) m)
	(error "Unable to read month.")))))

(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'rfc-822)) params)
  (declare (ignore params))
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'date nil)
  (peek-char t stream) ;; consume white space
  (datetime-reader stream datetime 'short-month nil)
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'year nil)
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'hour nil)
  (datetime-reader stream datetime 'next-digit nil)
  (datetime-reader stream datetime 'minute nil)
  (datetime-reader stream datetime 'rfc-822-tz nil)
  )




;;;---
;;; Mission Elapsed Time
;;;
;;; This is the number of milliseconds since the beginning of a mission. This means
;;; simply setting the 'epoch' to a different time. We'll assume just one MET
;;; clock. Is this adequate? The WITH-MET-CLOCK macro does allow for addtionals
;;; clocks under programmatic control.
;;;
;;;---

(defvar *met-epoch* *the-epoch-in-lisp* "The MET epoch")

(defun reset-met-clock (&optional (milliseconds (current-milliseconds)))
  "Reset the global mission clock to the number of seconds since 1970-01-01:00:00Z"
  (setq *met-epoch* (+ (floor milliseconds 1000) *the-epoch-in-lisp*)))

(defmacro with-met-clock ((&optional ms) &body body)
  "Locally define global clock to MET clock. Optionally, locolly define global
   MET clock as well as the number of milliseconds since 1970-01-01:00:00Z"
  (if ms
    (let ((msvar (gensym)))
      `(let* ((,msvar ,ms)
	      (*met-epoch* ,msvar)
	     (*the-epoch-in-lisp* (- (floor ,msvar 1000) *the-epoch-in-lisp*)))
	(declare (special *the-epoch-in-lisp* *met-epoch*))
	,@body))
    `(let ((*the-epoch-in-lisp* *met-epoch*))
       (declare (special *the-epoch-in-lisp* *met-epoch*))
       ,@body)))

(defun current-met ()
  (with-met-clock () (current-milliseconds)))

  
(defmethod datetime-reader ((stream stream) (datetime datetime) (latch (eql 'met)) params)
  (declare (ignore params))
  (datetime-reader stream datetime 'next-digit nil)
  (let ((duration 0)
	(periods (list *msecs-per-hour* *msecs-per-minute*))
	(periodi 0))
    (do* ((amt (read-string-until stream #'(lambda (ch) (or (digit-char-p ch)
							    (char= ch #\.))))
	       (read-string-until stream #'(lambda (ch) (or (digit-char-p ch)
							    (char= ch #\.)))))
	  (period (read-string-until stream #'(lambda (ch) (or (alpha-char-p ch) (char= ch #\:) (char= ch #\/))))
		  (read-string-until stream #'(lambda (ch) (or (alpha-char-p ch) (char= ch #\:) (char= ch #\/))))))
	((or (eql amt :eof)
	     (eql period :eof))
	 (cond
	  ((and (eql amt :eof)
		(eql period :eof))
	   (copy-datetime-into (milliseconds->datetime duration) datetime))
	  ((eql amt :eof)
	   (error "No Amount given for period ~S in MET time specification." period))
	  ((eql period :eof)
	   (let ((amt (read-from-string amt nil :eof)))
	     (if (not (realp amt))
		 (error "Invalid seconds specifier in MET time specification.")
	       (progn
		 (incf duration (* amt 1000))
		 (copy-datetime-into (milliseconds->datetime duration) datetime)))))))
      (let ((amt (read-from-string amt nil :eof))
	    (period (nstring-upcase period)))
	(if (not (realp amt))
	    (error "Invalid amount ~S in MET time specification (must be number)" amt)
	(progn
	  (cond
	   ((string= period "Y")
	    (incf duration (* amt *msecs-per-solar-year*)))
	   ;;((string= period "M")
	   ;;(incf duration (* amt *msecs-per-month*)))
	   ((string= period "W")
	    (incf duration (* amt *msecs-per-week*)))
	   ((or (string= period "D") (string= period "/"))
	    (incf duration (* amt *msecs-per-day*)))
	   ((string= period "H")
	    (incf duration (* amt *msecs-per-hour*)))
	   ((string= period "M")
	    (incf duration (* amt *msecs-per-minute*)))
	   ((string= period "S")
	    (incf duration (round (* amt 1000))))
	   ((string= period ":")
	    (if (>= periodi (length periods))
		(error "Too many colons in time specifier.")
	      (progn
		(incf duration (* amt (elt periods periodi)))
		(incf periodi))))
	   (T
	    (error "Invalid period specifier ~S." period)))
	  (datetime-reader stream datetime 'next-digit nil)))))))


(defun met-princ (met &optional (stream *standard-output*))
  (cond
   ((< met 0)
       (error "Met must be greater than 0."))
   ((= met 0)
    (format stream "+0/00:00:00"))
   (
    (multiple-value-bind (days met)
	(floor met *msecs-per-day*)
      (multiple-value-bind (hours met)
	  (floor met *msecs-per-hour*)
	(multiple-value-bind (minutes met)
	    (floor met *msecs-per-minute*)
	  (multiple-value-bind (seconds ms)
	      (floor met 1000)
	    (format stream "+~S/~2,'0,d:~2,'0,d:~2,'0,d" days hours minutes seconds)
	    (when (> ms 0)
	      (format stream ".~3,'0,d" ms))))))))
       met)

;;;----
;;; Durations
;;;---


(defun duration-princ (duration &optional (stream *standard-output*))
  (when (< duration 0)
      (error "Duration must be greater than 0."))
  (multiple-value-bind (weeks duration)
      (floor duration *msecs-per-week*)
    (multiple-value-bind (days duration)
	(floor duration *msecs-per-day*)
      (multiple-value-bind (hours duration)
	  (floor duration *msecs-per-hour*)
	(multiple-value-bind (minutes duration)
	    (floor duration *msecs-per-minute*)
	  (multiple-value-bind (seconds ms)
	      (floor duration 1000)
	    (if (not (some  
		      #'(lambda (x)
			    (> x 0))
		      (list weeks days hours minutes seconds ms)))
	      (format stream "P0S")
	      (progn 
		(princ "P" stream)
		(when (> weeks 0)
		  (format stream "~AW" weeks))
		(when (> days 0)
		  (format stream "~SD" days))
		(when (> hours 0)
		  (format stream "~SH" hours))
		(when (> minutes 0)
		  (format stream "~SM" minutes))
		(when (or (> seconds 0) (> ms 0))
		  (format stream "~S" seconds)
		  (when (> ms 0)
		    (format stream ".~3,'0,d" ms))
		  (format stream "S")))))))))
  duration)
		  
(defmethod ->list ((self string) &key 
                   (start 0) 
                   (char-bag '(#\Space))
                   (test #'(lambda (ch) (not (member ch char-bag :test 'char=))))
                   (post-process 'identity))
  "Converts SELF into a list,
     starting at START;
     dividing words at boundaries defined by characters in CHAR-BAG,
                 or at boundaries defined by TEST;
     each item is run through POST-PROCESS as it is created. POST-PROCESS can
     be destructive (eg, NSTRING-DOWNCASE)."
  (labels ((->list* (position)
             (let* ((pos (position-if-not test self :start position))
                    (new-pos (if pos (position-if test self :start pos) nil)))
               (cond
                ((and pos new-pos)
                 (cons (funcall post-process (subseq self position pos))
                       (->list* new-pos)))
                (pos (list (funcall post-process (subseq self position pos))))     
                (t (list (funcall post-process (subseq self position))))))))
    
    (let ((pos (position-if test self :start start)))     
      (if pos (->list*  pos) nil))))

(defun duration-expression? (x)         ; any -> bool
  (if (duration-read x t) t nil))

(defmethod duration-read ((x T) &optional nil-on-error)
  (if nil-on-error nil (error "Invalid duration expression: ~a" x)))

(defmethod duration-read ((str string) &optional nil-on-error)
  ;; string * opt(bool) -> opt(int) * opt(string)
  ;;
  ;; Returns two values: first is integral time (nil if expression is
  ;; invalid), second is an error string (if expression is invalid) or
  ;; nil (if expression is valid).  If nil-on-error is NIL (the default)
  ;; and expression is invalid, an error is raised and no values are
  ;; returned.
  ;;
  "P3W4D34M"
    (labels ((derror (format &rest args)
               (if nil-on-error
                   (return-from duration-read
                     (values nil (apply #'format `(nil ,format ,@args))))
                 (apply #'error (cons format args)))))
      (let ((str (string-upcase str)))
        (let ((digits (->list str :test #'(lambda (ch) (or (digit-char-p ch)  (char= ch #\.) ))))
              (periods (->list str :test #'alpha-char-p)))
          (unless (string= (car periods) "P")
            (derror "Invalid period specifier: ~S (does not begin with \"P\")." str))
          (let ((periods (cdr periods)))
            (unless (= (length periods)
                       (length (remove-duplicates periods :test #'string=)))
              (derror "Multiple period specifiers in ~S" str))
            (unless (= (length periods)
                       (length digits))
              (derror "Invalid period specifer: ~S (number of periods and names differ)." str))
            (let ((duration 0))
              (mapc #'(lambda (period digitstr)
                        (let ((multiplier (read-from-string  digitstr nil :eof)))
                          (unless (realp multiplier)
                            (derror "Invalid numeric specifier ~S in ~S (number could not be read)."
                                    digitstr str))
                          (cond
                           ;; ((string= period "Y")
                           ;; (incf duration (* multiplier *msecs-per-solar-year*)))
                           ;; ((string= period "M")
                           ;; (incf duration (* multiplier *msecs-per-month*)))
                           ((string= period "W")
                            (incf duration (* multiplier *msecs-per-week*)))
                           ((string= period "D")
                            (incf duration (* multiplier *msecs-per-day*)))
                           ((string= period "H")
                            (incf duration (* multiplier *msecs-per-hour*)))
                           ((string= period "M")
                            (incf duration (* multiplier *msecs-per-minute*)))
                           ((string= period "S")
                            (incf duration (round (* multiplier 1000))))
                           (T
                            (derror "Invalid duration specifier ~S in ~S" period str)))
                          ;; (format t "~%Leaving one call. Duration incremented to [~S]." duration)
                          ))
                    periods digits)
              (values (round duration) nil))))))) ;; round to convert to integer/values to return 1st only

(defmethod duration-read ((sym symbol) &optional nil-on-error)
  (duration-read (symbol-name sym) nil-on-error))

(defmethod duration-read ((spec NULL) &optional nil-on-error)
  (let ((message "no duration specification given"))
    (if nil-on-error
        (values nil message)
      (error message))))

(defmethod duration-read ((spec list) &optional nil-on-error)
  (duration-read-list spec 0 nil-on-error))

(defun duration-read-list (spec acc &optional nil-on-error)
    (labels ((derror (format &rest args)
               (if nil-on-error
                   (return-from duration-read-list
                     (values nil (apply #'format `(nil ,format ,@args))))
                 (apply #'error (cons format args)))))
      (if (null spec) (values acc nil)
        (let ((amt (car spec))
              (unit (cadr spec))
              (newspec (cddr spec)))
          (unless (numberp amt)
            (derror "Non-numeric time amount given: ~A" amt))
          (unless (not (null unit))
            (derror "No time unit given"))
          (duration-read-list 
           newspec
           (+ acc
              (* amt 
                 (case unit
                   ((ms msec msecs) 1)
                   ((s sec secs second seconds) 1000)
                   ((min mins minute minutes) *msecs-per-minute*)
                   ((h hr hrs hour hours) *msecs-per-hour*)
                   ((d day days) *msecs-per-day*)
                   ((w week weeks) *msecs-per-week*)
                   (otherwise (derror "Unknown time unit ~a" unit)))))
           nil-on-error)))))
   
(defmethod duration-read ((n real) &optional nil-on-error)
  (if (zerop n)
      n
    (let ((message (format nil "No units specified for ~a" n)))
      (if nil-on-error
          (values nil message)
        (error message)))))


;;; -- reading timepoints

;;; 

(defmacro tp-first-non-error (first &rest body)
  (labels ((first-non-error-h (conds)
	     (let  ((f (car conds))
		    (r (cdr conds)))
	       (if (null r)
		 f
		 (let ((new (first-non-error-h r))
		       (vvar (gensym "val"))
		       (evar (gensym "err")))
		   `(multiple-value-bind (,vvar ,evar)
			(ignore-errors  ,f)
		      (if (not ,evar)
			,vvar
			,new)))))))
    (first-non-error-h (cons first body))))
		    
	     
(defmethod timepoint-read ((str string))
  (tp-first-non-error 
   (datetime->milliseconds (datetime-read str :format-string "<iso-8601>" :datetime (current-datetime)))
   (datetime->milliseconds (datetime-read str :format-string "<iso-8601-time/stricter>" :datetime (current-datetime)))
   (+ (current-time) (duration-read str))
   (error "Can't parse timepoint string ~a" str)))

(defmethod timepoint-read ((str symbol))
  (timepoint-read (symbol-name str)))

(defmethod timepoint-read ((r real)) r)

;;---
;;Intervals
;;---

(defstruct (interval 
	    (:print-function princ-interval)
	    (:constructor make-interval-internal (start end left right)))
  (start nil) (end nil) (left :closed) (right :closed))

(defun make-interval (start &optional (end start) (left :closed) (right :closed))
  (assert (realp start) nil
    "Start must be a real: ~S" start)
  (assert (or (null end) (realp end)) nil
    "End must be a real (or null): ~S" end)
  (assert (or (null end) (>= end start)) nil
    "Start must be <= end: ~s ~s" start end)
  (assert (and (member left '(:open :closed))
	       (member right '(:open :closed)))
      nil
    "Interval end specifiers are :OPEN :CLOSED; you provided left: ~S right: ~S"
    left right)
  (when (or (null end) (= end start))
    (assert (and (not (eq left :open))
		 (not (eq right :open)))
	nil
      "Point intervals may not be open: ~S ~S" left right))
  (make-interval-internal start (or end start) left right))

(defmethod cl-user::start-of ((i interval))
  (interval-start i))

(defmethod cl-user::end-of ((i interval))
  (interval-end i))

(defun princ-interval (interval stream depth)
  (declare (ignore depth))
  (print-unreadable-object (interval stream :type t :identity nil)
    (if (eq (interval-left interval) :open)
      (princ #\( stream)
      (princ #\[ stream))
    (princ (interval-start interval) stream)
    (when (/= (interval-start interval) (interval-end interval))
      (princ #\- stream)
      (princ (interval-end interval) stream))
    (if (eq (interval-right interval) :open)
      (princ #\) stream)
      (princ #\] stream))))

(defun interval-string (interval)
  (with-output-to-string (stream)
    (if (eq (interval-left interval) :open)
      (princ #\( stream)
      (princ #\[ stream))
    (princ (cl-user::time-format (interval-start interval) nil) stream)
    (when (/= (interval-start interval) (interval-end interval))
      (princ #\Space stream)
      (duration-princ  (- (interval-end interval) (interval-start interval))  stream))
    (if (eq (interval-right interval) :open)
      (princ #\) stream)
      (princ #\] stream))))
    
(defun contains-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (and (< s1 s2)
	 (<= s2 f2)
	 (< f2 f1))))

(defun finishes-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (and (< s2 s1) (= f1 f2))))

(defun starts-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (and (= s1 s2) (< f1 f2))))

(defun before-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((f1 (interval-end i1))
	(s2 (interval-start i2)))
    (< f1 s2)))

(defun meets-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((f1 (interval-end i1))
	(s2 (interval-start i2)))
    (= f1 s2)))

(defun overlaps-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (< s1 s2 f1 f2)))

(defun cotemporal-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (and (= s1 s2) (= f1 f2))))

(defun during-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (and (< s2 s1)
	 (<= s1 f1)
	 (< f1 f2))))

(defun finished-by-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (and (< s1 s2) (= f1 f2))))

(defun started-by-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (and (= s1 s2) (< f2 f1))))

(defun after-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f2 (interval-end i2)))
    (< f2 s1)))

(defun met-by-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f2 (interval-end i2)))
    (= s1 f2)))

(defun overlapped-by-p (i1 i2)
  (assert (and (interval-p i1) (interval-p i2)))
  (let ((s1 (interval-start i1))
	(f1 (interval-end i1))
	(s2 (interval-start i2))
	(f2 (interval-end i2)))
    (< s2 s1 f2 f1)))


;;;
;;; -- ok, we need this
;;;

(defmethod time-expression? ((i T)  &optional vars-ok?)
  (declare (ignore vars-ok?))
  nil)

(defmethod time-expression? ((i real)  &optional vars-ok?)
  (declare (ignore vars-ok?))
  (if (zerop i) t
      nil))

(defmethod time-expression? ((i string) &optional vars-ok?)
  (declare (ignore vars-ok?))
  (duration-expression? i))

(defmethod time-expression? ((i symbol) &optional vars-ok?)
  (time-expression? (symbol-name i) vars-ok?))

(defconstant *time-expression-units*
  '(ms msec msecs
    s sec secs second seconds
    min mins minute minutes
    h hr hrs hour hours
    d day days
    w week weeks
    ))

(defmethod time-expression? ((expr list)  &optional vars-ok?)
  (flet ((ok-pair (x y) 
	   (and
	    (if vars-ok?
	      (or (realp x) (symbolp x))
	      (realp x))
	    (member y *time-expression-units*))))
    (and (evenp (length expr))
         (loop for exp1 on expr by #'cddr
             for exp2 on (cdr expr) by #'cddr
             unless (ok-pair (car exp1) (car exp2))
             return nil
             finally 
               (return t)))))


(defun quoted-time-expression? (x &optional vars-ok?)
  (and
   (consp x)
   (= 2 (length x))
   (eq 'quote (first x))
   (time-expression? (second x) vars-ok?)))
       
(defmethod ms2hms ((n integer))
  ;;
  ;; int -> int * int * int * int
  ;; Convert milliseconds to hours/mins/secs/fraction
  ;;
  (multiple-value-bind (secs fraction)
      (truncate n 1000)
    (multiple-value-bind (mins seconds)
        (truncate secs 60)
      (multiple-value-bind (hours minutes)
          (truncate mins 60)
        (values hours minutes seconds fraction)))))
