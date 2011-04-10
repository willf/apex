;;;-*- Mode: Lisp; Package: xsdf -*-
;;;

;;; The extremely simple defsystem facility.
;;;
;;; 

(defpackage :xsdf
  (:use :common-lisp)
  (:export
   #:load-file
   #:load-system
   #:compile-system
   #:check-system
   #:clean-system
   #:load-systems
   ))

(in-package :xsdf)

;;; -----------------------------------------------------------------------------
;;; XSDF: The extremely simple defsystem facility
;;; 
;;; (LOAD-SYSTEM <list-of-filespecs> & optional from)
;;; (COMPILE-SYSTEM <list-of-filespecs> &optional from)
;;; (CHECK-SYSTEM <list-of-filespecs> &optional from)
;;; (LOAD-FILE <filespec> &optional from)
;;; --
;;; A *system* is just a list of filenames or pathnames ('filespecs'). 
;;;
;;; To compile a system: (compile-system <system>)
;;; 
;;; Binary (fasl) files will be placed automatically in a directory at
;;; the 'lowest common directory' of the files in the system, named
;;; bin. They will be further divided by Lisp implementation.
;;; 
;;; You can specify the binary directory with the optional FROM parameter.
;;; It should be a directory acceptable to the MAKE-DIRECTORY :DIRECTORY
;;; parameter; eg. (pathname-directory #P"Home:test;bin;") (Note the trailing ';').
;;;
;;; To load a system so compiled: (load-system <system>)
;;;
;;; This will compile the code if necessary, so compile-system is really
;;; not necessary. All you really need is love, and LOAD-SYSTEM.
;;;
;;; If a binary file is out of date, the source file is recompiled, *along with
;;; all files further down in the list*. 
;;;
;;; The function (check-system <system>) will do this checking for you.
;;; It returns T if the system is up-to-date, or NIL otherwise.
;;; If for some reason you want to load a single file, use (load-file
;;; <filespec>).  It has an optional from parameter, too.
;;; 
;;; If you want to define 'modules,' you can create lists of systems
;;; and use (mapcar 'load-system <list-of-systems>) or some
;;; such. Cross module dependencies are not tracked, however.
;;;
;;; If you want to do something else to each file in a system, just call
;;;
;;; (mapcar <fn> <system>)
;;;
;;; For example: (mapcar 'ed *system*) 
;;;
;;; Typical use:
;;;
;;; (setf (logical-pathname-translations "Home")
;;;   `(("**;*.*.*" ,(make-pathname 
;;;		  :directory (append (butlast (pathname-directory *load-truename*))
;;;				    (list :wild-inferiors))
;;;		  :name :wild
;;;		  :type :wild))))
;;;
;;; (defparameter *asystem*
;;;	    '("Home:test;one.lisp"
;;;	      "Home:test;two.lisp"
;;;	      "Home:test;three.lisp"
;;;	      )))
;;;
;;; (load-system *asystem*)
;;;
;;; No topologies were sorted in the creation of this code.
;;; -----------------------------------------------------------------------------

(defvar *fasl-extension*
    (pathname-type (compile-file-pathname *default-pathname-defaults*)))

(defun to-symbol (x)
  (if (symbolp x) x
      (read-from-string 
       (format nil "|~s|" x))))

(defun lowest-common-directory (directories &optional result)
  (let ((ekl (lambda (a &optional (b nil givenp))
	       (if givenp
		 (and (equal a b) a)
		 a))))
    (cond
     ((some 'null directories)
      (reverse result))
     ((reduce ekl (mapcar 'car directories))
      (lowest-common-directory (mapcar 'cdr directories) (cons (caar directories) result)))
     (t (reverse result)))))

#-allegro(defun default-bin-directory (directories)
  (append (butlast (lowest-common-directory directories)) (list "bin")))

#+allegro(defun default-bin-directory (directories)
	   (append (butlast (lowest-common-directory directories)) 
		   `("bin" ;; excl::*common-lisp-version-number*
		     ,@(if (and (find-package :excl)
			      (find-symbol "*COMMON-LISP-VERSION-NUMBER*" (find-package :excl)))
			 (list (substitute #\_ #\.
					   (format nil "acl~a" 
						   (symbol-value 
						    (find-symbol "*COMMON-LISP-VERSION-NUMBER*" (find-package :excl))))
					   :test 'char=))))))


(defun merge-bin-directory (bin-directory original-directory)
  (append bin-directory
	  (subseq 
	   original-directory
	    (length 
	     (lowest-common-directory (list bin-directory original-directory))))))


(defmethod binary-pathname ((p pathname)
			    &optional (from 
				       (append (pathname-directory p)
					       (list "bin"))))
  (merge-pathnames 
   (make-pathname :type *fasl-extension*
		  :directory from)
   p))

(defmethod binary-pathname ((p string)
			     &optional (binary-directory
				       (append (pathname-directory (translate-logical-pathname p))
					       (list "bin"))))
  (binary-pathname (translate-logical-pathname p) binary-directory))

(defmethod newer-file ((p1 pathname) (p2 pathname))
  (cond
   ((and (probe-file p1)
	 (probe-file p2))
    (if (> (file-write-date p1) (file-write-date p2))
      p1 p2))
   ((probe-file p1) p1)
   ((probe-file p2) p2)
   (t nil)))

(defmethod load-file ((p pathname) &optional (from nil))
  (let ((f (newer-file p (if from (binary-pathname p from) (binary-pathname p)))))
    (unless f 
      (warn "~a does not exist; not loading" p)
      (return-from load-file nil))
    (load f :verbose t)
    (setf (get (to-symbol p) 'loaded) t)
    f))

(defmethod load-file ((p string) &optional (from nil))
  (if from
    (load-file (translate-logical-pathname p) from)
    (load-file (translate-logical-pathname p))))

(defmethod compile-and-load-file ((p pathname) from loadp)
  (unless (probe-file p)
    (warn "~a does not exist; not compiling or loading." p)
    (return-from compile-and-load-file nil))
  (let ((bin (binary-pathname p from)))
    (cond 
     ;; a binary exists, but is older, or does not exist
     ((or (not (get (to-symbol p) 'compiled))
	  (not (probe-file bin))
	  (> (file-write-date p) (file-write-date bin)))
      (ensure-directories-exist bin)
      (compile-file p
		    :load-after-compile loadp
		    :output-file bin))
     ;; a binary exists; but is younger
     (t  (when loadp (load-file p from))))
    ;; 
    (when loadp (setf (get (to-symbol p) 'loaded) t))
    (setf (get (to-symbol p) 'compiled) t)
    bin))

(defmethod compile-and-load-file ((p string) from loadp)
  (compile-and-load-file (translate-logical-pathname p) from loadp))

(defmethod ensure-pathname ((p pathname)) p)
(defmethod ensure-pathname ((p string)) (translate-logical-pathname p))
(defmethod ensure-pathname ((p t)) (error "~a must be a string or pathname, but is a ~a"
					  p (type-of p)))
(defun check-system (system  &optional from)
  (let ((pathnames (mapcar 'ensure-pathname system)))
    (let ((from (or from (default-bin-directory 
			     (mapcar 'pathname-directory pathnames))))
	  (kill nil))
      (dolist (pathname pathnames)
	(let ((bin (binary-pathname pathname from))
	      (sym (to-symbol pathname)))
	  (if (or kill
		  (not (probe-file bin))
		  (not (probe-file pathname))
		  (> (file-write-date pathname)
		     (file-write-date bin)))
	    (progn
	      (setf (get sym 'loaded) nil)
	      (setf (get sym 'compiled) nil)
	      (setq kill t))
	    (progn
	      (setf (get sym 'compiled) t)))))
      (not kill))))

(defun load-system (system &optional from)
  (let ((pathnames (mapcar 'ensure-pathname system)))
    (let ((from (or from (default-bin-directory 
			  (mapcar 'pathname-directory pathnames)))))
      (prog1 
	  (not (check-system system from))
	(dolist (pathname pathnames)
	  (unless (get (to-symbol pathname) 'loaded)
	    (compile-and-load-file pathname from t)))))))

(defun compile-system (system  &optional from)
  (let ((pathnames (mapcar 'ensure-pathname system)))
    (let ((from (or from (default-bin-directory 
			  (mapcar 'pathname-directory pathnames)))))
      (prog1 
	  (not (check-system system from))
	(dolist (pathname (mapcar 'ensure-pathname system))
	  (unless (get (to-symbol pathname) 'compiled)
	    (compile-and-load-file pathname from nil)))))))

(defun clean-system (system  &optional from)
  (let ((pathnames (mapcar 'ensure-pathname system)))
    (let ((from (or from (default-bin-directory 
			  (mapcar 'pathname-directory pathnames)))))
  (dolist (pathname pathnames)
    (let ((bin (binary-pathname pathname from))
	  (sym (to-symbol pathname)))
      (when (probe-file bin)
	(delete-file bin))
      (setf (get sym 'loaded) nil)
      (setf (get sym 'compiled) nil))))))



  
  