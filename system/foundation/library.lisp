;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/foundation/library.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: library.lisp,v 1.5 2006/02/15 15:20:29 will Exp $

;;; Library Support code


(in-package :user)

(defvar *loaded-apex-libraries* nil)

(defvar *apex-library-path* 
    (list  :application
	   #.(merge-pathnames (make-pathname :directory '(:relative "apexlib"))
			      (user-homedir-pathname))
	   #p"apex:apexlib;"
           #p"apex:examples;apexlib;"
	   ))


(defun apexlib-loaded-p (library)
  (member library *loaded-apex-libraries* 
	  :test #'string=))

(defun decode-path-element (path-element app-pathname)
  (cond ;;((eq path-element :application)
   ;; *application-base-directory*)
   ((eq path-element :application)
    (or app-pathname (and *application* (application-directory))))
   ((stringp path-element)
    (pathname path-element))
   ((pathnamep path-element)
    path-element)
   ;; Return nil for junk - print error?
   (t nil)))
     
     
(defun find-library-file (library &optional app-pathname)
  (loop for path-element in *apex-library-path*
      for path = (decode-path-element path-element app-pathname)
      for potential-library-file
      = (merge-pathnames
         (make-pathname :name (format nil "~A-apexlib" library)
                        :type "lisp") path)
      for potential-library-dir
      = (merge-pathnames
         (make-pathname :name (format nil "~A-apexlib" library)
                        :directory `(:relative ,library)
                        :type "lisp") path)
      do (debug-message 2 'library "Looking for Library ~S~%" 
                        potential-library-file)
      when (probe-file potential-library-file)
      return potential-library-file
      do (debug-message 2 'library "Looking for Library ~S~%"
                        potential-library-dir)
      when (probe-file potential-library-dir)
      return potential-library-dir))
			      


(defun require-apex-library (library &optional app-pathname)  ; string + pathname -> ()
  (unless (apexlib-loaded-p library)
    (let ((file (find-library-file library app-pathname)))
      (when file
        (load file)
        (push library *loaded-apex-libraries*)))))
  
  
(defun require-apex-file (file)		; string + pathname -> ()
  (let ((source *load-truename*))
  (if source
      (load
       (make-pathname
        :host (pathname-host source)
        :directory (pathname-directory source)
        :name (pathname-name file)
        :type (pathname-type file)))
    (load file))))
