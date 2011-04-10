;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/misc-monitors-estimation.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-monitors-estimation.lisp,v 1.5 2006/01/15 03:42:59 dalal Exp $

(unless (fboundp 'signal-test-monitor)
  (load "system/utility/monitor-testing-framework.lisp"))

(apex.utility.unit-test:with-tests (:name "Estmation")  
  (def-monitor-test 
      "Persist/Numeric"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 5)
    (log (alt ac2) :count-limit 5) 
    
    (at P0s 
	;; error in parameter to :persist
	(compile-error (waitfor (:measurement 
		  o1.a
		  (alt ac1 = 32000)
		  :estimation (:persist :with-buggablo 45)
		  ))))
    
    (at P0s 
	;; error in parameter to :estimation
	(compile-error (waitfor (:measurement 
		  o1.b
		  (alt ac1 = 32000)
		  :estimation (:boggablo :with-buggablo 45)
		  ))))    
    
    (at P0s 
	;; persist forever 
	(waitfor (:measurement 
		  o1.1a 
		  (alt ac1 = 32000)
		  :estimation (:persist)
		  ))
	(result o1.1a :succeed P10S))
    
    (at P0s 
	;; persist forever w/variable
	(waitfor (:measurement 
		  o1.1b 
		  (alt ac1 = ?val)
		  :estimation (:persist)
		  ))
	(result o1.1b :succeed P10S))
    
    (at P10s 
	;; persist forever -- ok right away
	(waitfor (:measurement 
		  o1.2a 
		  (alt ac1 = 32000)
		  :estimation (:persist)
		  ))
	(result o1.2a :succeed P10S))
    
    (at P10s 
	;; persist forever -- ok right away w/variable
	(waitfor (:measurement 
		  o1.2b 
		  (alt ac1 = ?val)
		  :estimation (:persist)
		  ))
	(result o1.2b :succeed P10S))
    
    (at P15s 
	;; persist forever -- ok, we find it at 15 seconds,
	;; because it started at 10 seconds.
	(waitfor (:measurement 
		  o1.3a
		  (alt ac1 = 32000)
		  :estimation (:persist)
		  ))
	(result o1.3a :succeed P15S))
    
    (at P15s 
	;; persist forever -- ok, we find it at 15 seconds,
	;; because it started at 10 seconds.
	;;  w/variable
	(waitfor (:measurement 
		  o1.3b
		  (alt ac1 = ?val)
		  :estimation (:persist)
		  ))
	(result o1.3b :succeed P15S))    
    
    (at P15s 
	;; persist forever -- ok, we find it at 15 seconds,
	;; because it started at 10 seconds.
	(waitfor (:measurement 
		  o1.4a
		  (alt ac1 = 32000)
		  :estimation (:persist :with-timeout P10S)
		  ))
	(result o1.4a :succeed P15S))
    
    (at P15s 
	;; persist forever -- ok, we find it at 15 seconds,
	;; because it started at 10 seconds.
	;;  w/variable
	(waitfor (:measurement 
		  o1.4b
		  (alt ac1 = ?val)
		  :estimation (:persist :with-timeout P10S)
		  ))
	(result o1.4b :succeed P15S))
    (at P0s 
	;; this should fail ... because things don't persist
	;; long enough
	(waitfor (:measurement 
		  o1.5a
		  (alt ac1 = 32000)
		  :estimation (:persist :with-timeout P1S)
		  :timestamp 
		  (> (+ (start-of +this-task+) P45s))
		  (< (+ (start-of +this-task+) P50s))		  
		  ))
	(result o1.5a :fail))

    (at P45s 
	;; this should fail ... because things don't persist
	;; long enough
	;; w/variable
	(waitfor (:measurement 
		  o1.5b
		  (alt ac1 = ?val)
		  :estimation (:persist :with-timeout P1S)
		  :timestamp 
		  (> (+ (start-of +this-task+) P45s))
		  (< (+ (start-of +this-task+) P50s))
		  ))
	(result o1.5b :fail))
    (at P15s 
	;; add some constraints ... variable object ...
	;; 	;;  w/variable
	(waitfor (:measurement 
		  o1.6b
		  (alt ?obj = ?val)
		  :estimation (:persist :with-timeout P10S)
		  :object (eql ac2)
		  :value (> 300)
		  :timestamp 
		  (> (+ (start-of +this-task+) P30s))
		  (< (+ (start-of +this-task+) P30H))
		  ))
	(result o1.6b :succeed P110S))    
    
    (at P0s 
	;; persist -- find at a timestamp = measurement
	(waitfor (:measurement 
		  o1.7a
		  (alt ac1 = 32000)
		  :estimation (:persist :with-timeout P5s)
		  :timestamp (= (+ (start-of +this-task+) P20s))
		  ))
	(result o1.7a :succeed P20S))
    
    (at P0s 
	;; persist -- find at a timestamp = measurement
	;; with a variable
	(waitfor (:measurement 
		  o1.7b
		  (alt ac1 = ?var)
		  :estimation (:persist :with-timeout P5s)
		  :timestamp (= (+ (start-of +this-task+) P20s))
		  ))
	(result o1.7b :succeed P20S))
    
    (at P0s 
	;; persist -- find at a timestamp = measurement
	;; with a variable
	(waitfor (:measurement 
		  o1.7c
		  (alt ac1 = ?var)
		  :estimation (:persist :with-timeout P5s)
		  :timestamp (= (+ (start-of +this-task+) P21s))
		  ))
	(result o1.7c :succeed P20S))
    
    (at P0s 
	;; persist -- find at a timestamp = measurement
	;; with a variable -- timestamp >
	(waitfor (:measurement 
		  o1.8b
		  (alt ac1 = ?var)
		  :estimation (:persist :with-timeout P2.5s)
		  :timestamp (> (+ (start-of +this-task+) P25s))
		  ))
	(result o1.8b :succeed P30S))
    
    
    (at P0s 
	;; test +/- version
	;; 
	(waitfor (:measurement 
		  o1.9
		  (alt ac1 = 32100 +/- 2000)
		  :estimation (:persist :with-timeout P2.5s)
		  :timestamp (> (+ (start-of +this-task+) P25s))
		  ))
	(result o1.9 :succeed P30S))

    (at P10s  (signal-event '(alt ac1 = 32000)))
    (at P20s  (signal-event '(alt ac1 = 32000)))
    (at P30s  (signal-event '(alt ac1 = 32000)))
    (at P40s  (signal-event '(alt ac1 = 32000)))
    (at P50s  (signal-event '(alt ac1 = 35000)))    
    (at P110s (signal-event '(alt ac2 = 32000)))
    (at P120s (signal-event '(alt ac2 = 32000)))
    (at P130s (signal-event '(alt ac2 = 32000)))
    (at P140s (signal-event '(alt ac2 = 32000)))
    (at P150s (signal-event '(alt ac2 = 35000))) 
    )
  
  (def-monitor-test 
      "Persist/Symbolic"
      (keep)
    (task-start P0S)
    (log (state switch-1) :count-limit 5)

    (at P0s     (signal-event '(state switch-1 = on)))
    (at P60s    (signal-event '(state switch-1 = off)))    
    (at P90s    (signal-event '(state switch-1 = on)))
    (at P120s   (signal-event '(state switch-1 = off)))        

    (at P10s    
	(waitfor (:measurement 
		  o2.1a
		  (state switch-1 = on)
		  :estimation (:persist)))
	(result o2.1a :succeed P10s))
    
    (at P10s    
	(waitfor (:measurement 
		  o2.1b
		  (state switch-1 = ?val)
		  :estimation (:persist)))
	(result o2.1b :succeed P10s))
				       
    (at P30s
	(waitfor (:measurement 
		  o2.2a
		  (state switch-1 = on)
		  :estimation (:persist :with-timeout P10s)
		  :timestamp (:range (+ (start-of +this-task+) P20S)
				     (+ (start-of +this-task+) P40S))))
	(result o2.2a :fail))
    )
  
  (def-monitor-test 
      "Regress"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 5)
    

    (at P0s 
	;; error in parameter to :linear-regression
	(compile-error (waitfor (:measurement 
				 (alt ac1 = 32000)
		  :estimation (:linear-regression :with-buggablo 45)
		  ))))
    (at P0s 
	;; error in parameter to :linear-regression
	(compile-error (waitfor (:measurement 
		  (alt ac1 = 32000)
		  :estimation (:linear-regression 45)
		  ))))

    ;;; estimate a specific value -- a little weird. range is more usual?
    (at P0s
	(waitfor (:measurement
		  o3.1a
		  (alt ac1 = 32450)
		  :estimation (:linear-regression :minimum-points 3)))
	(result o3.1a :succeed P30s))
    
    ;;; estimate any value ... -- even wierder. 
    (qat P0s
	(waitfor (:measurement
		  o3.1b
		  (alt ac1 = ?val)
		  :estimation (:linear-regression :minimum-points 3)))
	(result o3.1b :succeed P30s))
    
    ;;; esimtate a value at a given time
    (at P0s
	(waitfor (:measurement
		  o3.1c
		  (alt ac1 = ?val)
		  :estimation (:linear-regression :minimum-points 3
						  :start (+ (start-of +this-task+) P60s)
						  :end   (+ (start-of +this-task+) P140s))
		  :timestamp (= (+ (start-of +this-task+) P145S))))
	(result o3.1c :succeed P80s))

    ;;; put a constraint on value
    (at P30s
	(waitfor (:measurement
		  o3.2a
		  (alt ac1 = ?val)
		  :estimation (:linear-regression 
			       :minimum-points 3
			       :start (+ (start-of +this-task+) P10S))
		  :value (> 32400) (< 35000)
		  :timestamp (:range (start-of +this-task+) (+ (start-of +this-task+) P1h))
		  ))
	(result o3.2a :succeed P30s))
    
    (at P30s ;; same as above, more or less, with +/- syntax
	(waitfor (:measurement
		  o3.2b
		  (alt ac1 = 33700 +/- 1300)
		  :estimation (:linear-regression 
			       :minimum-points 3
			       :start (+ (start-of +this-task+) P10S))
		  ;; :value (> 32400) (< 35000)
		  :timestamp (:range (start-of +this-task+) (+ (start-of +this-task+) P1h))
		  ))
	(result o3.2b :succeed P30s))
    
    (at P10s  (signal-event '(alt ac1 = 32100)))
    (at P20s  (signal-event '(alt ac1 = 32200)))
    (at P30s  (signal-event '(alt ac1 = 32300)))
    (at P40s  (signal-event '(alt ac1 = 32400)))
    (at P50s  (signal-event '(alt ac1 = 32500)))
    (at P60s  (signal-event '(alt ac1 = 32500)))    
    (at P70s  (signal-event '(alt ac1 = 32400)))    
    (at P80s  (signal-event '(alt ac1 = 32300)))    
    (at P90s  (signal-event '(alt ac1 = 32200)))    
    (at P100s  (signal-event '(alt ac1 = 32100)))    
    
    )
  )