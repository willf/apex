;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/test.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: test.lisp,v 1.2 2006/01/15 03:42:57 dalal Exp $

(in-package :common-lisp-user)


(defapplication "Test"   
    :init (test)
    :libraries ("human"))







(defun test ()  
  (let* 
      ((kitchen (make-instance 'locale 
                  :name 'kitchen))
       
       (cook (make-instance 'human
               :name 'cook 
               :location '(0 0 22)
               :initial-task '(do-domain)
               :pos '(0 0) 
               :locale kitchen
               )) 

  (assemble cook)
  ))
)


(procedure
 (index (do-domain))
 )