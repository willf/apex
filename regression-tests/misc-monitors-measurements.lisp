;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/misc-monitors-measurements.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-monitors-measurements.lisp,v 1.10 2006/01/15 03:42:59 dalal Exp $

(unless (fboundp 'signal-test-monitor)
  (load "system/utility/monitor-testing-framework.lisp"))

(procedure (index (test-monitor-noop)))

;;;(let ((counter 0))
;;;  (defun probe-count () 
;;;    (format t ";;; New count is ~a at ~a~%" (incf counter) (current-time)) 
;;;    (when (> counter 50) (break))
;;;    counter)
;;;  (defun reset-count () (setf counter 0)))

(apex.utility.unit-test:with-tests (:name "Measurement")  
  (def-monitor-test 
      "Numeric"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (at P0s 
	;; just start looking -- should see it on the first event
	(waitfor (:measurement o1.1 (alt ac1 = 32000) ))
	(result o1.1 :succeed P10S))
    
    (at P15s 
	;; start looking at 15 seconds; next one in at 20s -- but go against task start
	(waitfor (:measurement o1.2 (alt ac1 = 32000)))
	(result o1.2 :succeed P15S))
    
    (at P0s
	;; look for an event, but constrain the timestamp 
	(waitfor (:measurement 
		  o1.3 
		  (alt ac1 = 32000)
		  :timestamp (> (+ (start-of +this-task+) P10s))))
	(result o1.3 :succeed P20s))

    (at P0s
	;; multiple timestamp constraints
	(waitfor (:measurement
		  o1.4
		  (alt ac1 = 32000)
		  :timestamp
		  (> (+ (start-of +this-task+) P15s))
		  (< (+ (start-of +this-task+) P25s))))
	(result o1.4 :succeed P20s))
    
    (at P0S
	;; bind the value
	(waitfor (:measurement
		  o1.5
		  (alt ac1 = ?val)))
	(result o1.5 :succeed P10s))
    
    (at P0s
	;; a value constraint
	(compile-error 
	 (waitfor (:measurement 
		   o1.6
		   (alt ac1)
		   :value (> 32000)))))
    
    (at P0s
	;; a value constraint with a binding
	(waitfor (:measurement 
		  o1.7
		  (alt ac1 = ?val )
		  :value (> 32000)))
	(result o1.7 :succeed P50s))

    (at P0s 
	;; different aircraft -- happens later 
	(waitfor (:measurement o1.8 (alt ac2 = 32000) ))
	(result o1.8 :succeed P110S))
    
    (at P115s 
	;; start looking at 15 seconds; next one in at 20s -- but task start at 0
	(waitfor (:measurement o1.9 (alt ac2 = 32000)))
	(result o1.9 :succeed P115S))
    
    (at P0s
	;; any aircraft ...
	(waitfor (:measurement o1.10 (alt ?ac = 32000)))
	(result o1.10 :succeed P10S))
    
    (at P0s
	;; any aircraft ...but constraint it
	(waitfor (:measurement 
		  o1.11 (alt ?ac = 32000)
		  :object (eql ac2)))
	(result o1.11 :succeed P110S))  
    
    (at P0s
	;; any aircraft ...but a number of constraints
	(waitfor (:measurement 
		  o1.12 (alt ?ac = ?val)
		  :object (eql ac2)
		  :value (> 32000)
		  :timestamp (> (+ (start-of +this-task+) P50s))))
	(result o1.12 :succeed P150S))
		 
    (at P200s
	;;; look for a something in the past ...
	(waitfor (:measurement 
		  o1.13 (alt ac1 = 32000)
		  :timestamp (> (start-of +this-task+))))
	(result o1.13 :succeed P200S))

    (at P200s
	;;; look for a something in the past ...
	(waitfor (:measurement 
		  o1.14 (alt ?ac = ?val)
		  :object (eql ac2)
		  :value (> 32000)
		  :timestamp (> (+ (start-of +this-task+) P50s))))
	(result o1.14 :succeed P200S))
    
    (at P0s 
	;; use range
	(waitfor (:measurement 
		  o1.15 (alt ?ac = 35100 +/- 100)))
	(result o1.15 :succeed P50S))
    
    
    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 32000)))
    (at P30s (signal-event '(alt ac1 = 32000)))
    (at P40s (signal-event '(alt ac1 = 32000)))
    (at P50s (signal-event '(alt ac1 = 35000)))    
    (at P110s (signal-event '(alt ac2 = 32000)))
    (at P120s (signal-event '(alt ac2 = 32000)))
    (at P130s (signal-event '(alt ac2 = 32000)))
    (at P140s (signal-event '(alt ac2 = 32000)))
    (at P150s (signal-event '(alt ac2 = 35000)))        
    )
  
  
  
  
  (def-monitor-test 
      "Symbolic"
      (keep)
    (task-start P0S)
    (log (state fsa1) :count-limit 1000)
    (at P0s 
	;; just start looking -- should see it on the first event
	(waitfor (:measurement o2.1 (state fsa1 = begin) ))
	(result o2.1 :succeed P10S))
    
    (at P15s 
	;; start looking at 15 seconds; next one in at 60s -- task start at 0s
	(waitfor (:measurement o2.2 (state fsa1 = begin)))
	(result o2.2 :succeed P15S))
    
    (at P0s
	;; look for an event, but constrain the timestamp 
	(waitfor (:measurement 
		  o2.3 
		  (state fsa1 = begin)
		  :timestamp (> (+ (start-of +this-task+) P10s))))
	(result o2.3 :succeed P60s))

    (at P0s
	;; multiple timestamp constraints
	(waitfor (:measurement
		  o2.4
		  (state fsa1 = begin)
		  :timestamp
		  (> (+ (start-of +this-task+) P05s))
		  (< (+ (start-of +this-task+) P25s))))
	(result o2.4 :succeed P10s))
    
    (at P0S
	;; bind the value
	(waitfor (:measurement
		  o2.5
		  (state fsa1 = ?val)))
	(result o2.5 :succeed P10s))
    
    (at P0s
	;; a value constraint
	(compile-error 
	 (waitfor (:measurement 
		   o2.6
		   (state fsa1)
		   :value (eql begin)))))
    
    (at P0s
	;; a value constraint with a binding
	(waitfor (:measurement 
		  o2.7
		  (state fsa1 = ?val )
		  :value (eql end)))
	(result o2.7 :succeed P50s))
    
    
    (at P0s
	;; any state
	(waitfor (:measurement o2.10 (state ?ac = begin)))
	(result o2.10 :succeed P10S))
    
    (at P0s
	;; any state ...but constraint it
	(waitfor (:measurement 
		  o2.11 (state ?ac = begin)
		  :object (eql fsa1)))
	(result o2.11 :succeed P10S))  
    

		 
    (at P200s
	;;; look for a something in the past ...
	(waitfor (:measurement 
		  o2.13 (state fsa1 = begin)
		  :timestamp (> (start-of +this-task+))))
	(result o2.13 :succeed P200S))

    (at P200s
	;;; look for a something in the past ...
	(waitfor (:measurement 
		  o2.14 (state ?ac = ?val)
		  :object (eql fsa1)
		  :value (eql begin)
		  :timestamp (> (+ (start-of +this-task+) P50s))))
	(result o2.14 :succeed P200S))
    
    
    (at P10s (signal-event '(state fsa1 = begin)))
    (at P20s (signal-event '(state fsa1 = state-1)))
    (at P30s (signal-event '(state fsa1 = state-2)))
    (at P40s (signal-event '(state fsa1 = state-1)))
    (at P50s (signal-event '(state fsa1 = end))) 
    (at P60s (signal-event '(state fsa1 = begin)))
    (at P70s (signal-event '(state fsa1 = state-1)))
    (at P80s (signal-event '(state fsa1 = state-2)))
    (at P90s (signal-event '(state fsa1 = state-1)))
    (at P100s (signal-event '(state fsa1 = end)))    
    )
  
;;; -- can't test probing this way, since we don't hook into COGEVENT
;;;  (def-monitor-test 
;;;      "Probes"
;;;      (keep)
;;;    (task-start P0S)
;;;    (eval (reset-count))
;;;    (at P10s (signal-event '(state fsa1 = begin)))
;;;    (at P20s 
;;;	(waitfor
;;;	 (:measurement m3.1 (count mr-computer = 30) :probing (probe-count P5s)))
;;;	(result m3.1 :succeed P1700S))
;;;    )
  
  
  (def-monitor-test 
      "Numeric with operators"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
        (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 33000)))
    (at P30s (signal-event '(alt ac1 = 34000)))
    (at P40s (signal-event '(alt ac1 = 35000)))
    (at P50s (signal-event '(alt ac1 = 36000)))    

    (at P0s 
	;; just start looking -- should see it on the first event
	(waitfor (:measurement o4.1 (alt ac1 = 32000) ))
	(result o4.1 :succeed P10S))
    
    (at p0s 
	(waitfor (:measurement o4.2 (alt ac1 >= 32000)))
	(result o4.2 :succeed P10S))
    
    
    (at p0s 
	(waitfor (:measurement o4.3 (alt ac1 > 32000)))
	(result o4.3 :succeed P20S))
    
    (at p0s 
	(waitfor (:measurement o4.4 (alt ac1 <= 32000)))
	(result o4.4 :succeed P10S))
    
    (at p0s 
	(waitfor (:measurement o4.5 (alt ac1 < 33000)))
	(result o4.5 :succeed P10S))
    
    (at p0s 
	(waitfor (:measurement o4.6 (alt ac1 >= (* 33 3.14159 310))))
	(result o4.6 :succeed P20S))
    
    (at p0s 
	(waitfor (:measurement o4.7 (alt ac1 = (* 33 (expt 10 3)))))
	(result o4.7 :succeed P20S))
    
    (at p0s 
	(waitfor (:measurement o4.8 (alt ac1 = 32900 +/- (* 10 10))))
	(result o4.8 :succeed P20S))
        
    )
  )