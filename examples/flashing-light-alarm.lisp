;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/flashing-light-alarm.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: flashing-light-alarm.lisp,v 1.7 2006/01/15 03:42:52 dalal Exp $


(in-package :cl-user)

(defapplication "Simple Alarm"
    :init (initialize-sim))
;; (10 red true on)
(defun initialize-sim ()
  (let* ((activities
	  '(
	    (10 (color light-1 = green))
	    (10 (flashing light-1 = true))
	    (10 (alarm alarm-1 = off))
	    ;;
	    (20 (color light-1 = green))
	    (20 (flashing light-1 = true))
	    (20 (alarm alarm-1 = on))
	    ;;
	    (30 (color light-1 = red))
	    (30 (flashing light-1 = true))
	    (30 (alarm alarm-1 = on))
	    ;;
	    (40 (color light-1 = red))
	    (40 (flashing light-1 = true))
	    (40 (alarm alarm-1 = on))	    
	    ;;
	    (10 (color light-2 = green))
	    (10 (flashing light-2 = off))
	    (20 (color light-2 = green))
	    (20 (flashing light-2 = off))
	    (30 (color light-2 = green))
	    (30 (flashing light-2 = off))
	    (40 (color light-2 = green))
	    (40 (flashing light-2 = off))	    
	    
	    ))
	 (locale (make-instance 'locale
		   :name "Life Support"))
	 (agent
	  (make-instance 'agent
	    :name "Watchman Nee"
	    :initial-task '(watch-for-alarm)
	    :locale locale
	    )))
    (mapcar 
     #'(lambda (activity)
	 (destructuring-bind (timestamp event)
	     activity
	   (schedule `(,timestamp seconds)
		     (cogevent event agent :trigger-asa t))))
     activities)
    (show alarmed)
    (show color)
    (show alarm)
    (show flashing)
    agent))

(procedure :sequential (watch-for-alarm)
	   (step (alarm-with-light light-1 alarm-1)))

(procedure 
    :sequential (alarm-with-light ?light ?alarm)
    (log (color <?light>)
	 (flashing <?light>)
	 (alarm <?alarm>))
    (step (cogevent (alarmed +self+ = true) +self+)
	  (waitfor 
	   (:in-order 
	    (:and (color <?light> = red)
		  (flashing <?light> = true))
	    (alarm <?alarm> = on)))))

