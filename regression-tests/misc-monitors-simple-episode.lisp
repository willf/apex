;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/misc-monitors-simple-episode.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-monitors-simple-episode.lisp,v 1.6 2006/01/15 03:42:59 dalal Exp $



(unless (fboundp 'signal-test-monitor)
  (load "system/utility/monitor-testing-framework.lisp"))

(primitive (index (test-monitor-noop)))

(apex.utility.unit-test:with-tests (:name "Simple Episode")  
  (def-monitor-test 
      "Holding" 
      (keep)
      (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (at P0s 
	(waitfor 
	 (:episode o1.1a (alt ac1)
		   :quality (:msi P10S)
		   :value (= 32000)
		   :timing (:duration (>= P20S))))
	(result o1.1a :succeed P30s))
    (at P0s 
	(waitfor 
	 (:episode o1.1b (alt ?var)
		   :quality (:msi P10S)
		   :value (= 32000)
		   :timing (:duration (>= P20s))))
	(result o1.1b :succeed P30s))
    
    (at P0s 
	(waitfor 
	 (:episode o1.1c (alt ac1)
		   :quality (:msi P5S)
		   :value (= 32000)
		   :timing (:duration (>= P20S))))
	(result o1.1c :fail))
    
    (at P0s
	(waitfor 
	 (:episode o1.2a  (alt ac1)
		   :quality (:msi P10s)
		   :value (= 32000)
		   :timing 
		   (:start 
		    (= (+ (start-of +this-task+) P10S))
		    :end 
		    (= (+ (start-of +this-task+) P50s)))))
	 (result o1.2a :succeed P50s))
    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 32000)))
    (at P30s (signal-event '(alt ac1 = 32000)))
    (at P40s (signal-event '(alt ac1 = 32000)))
    (at P50s (signal-event '(alt ac1 = 32000)))    
    (at P60s (signal-event '(alt ac1 = 32000)))
    
    )
  (def-monitor-test 
      "Test 2 holding -- varying data" 
      (keep)
      (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (at P0s 
	(waitfor 
	 (:episode o2.1a (alt ac1)
		   :quality (:msi P10s)
		   :value (= 32000)
		   :timing (:duration (>= P20S))))
	(result o2.1a :succeed P50s))
    (at P0s 
	(waitfor 
	 (:episode o2.1b (alt ?var)
		    :quality (:msi P10s)
		   :value (= 32000)
		   :timing (:duration (>= P20s))))
	(result o2.1b :succeed P50s))
    
    (at P0s
	(waitfor 
	 (:episode o2.2a (alt ac1)
		   :quality (:msi P10s)
		   :value (= 32000)
		   :timing 
		   (:start 
		    (= (+ (start-of +this-task+) P10S))
		    :end 
		    (= (+ (start-of +this-task+) P50s)))))
	(result o2.2a :fail))
    
    (at P0s
	(waitfor 
	 (:episode o2.3 (alt ac1)
		   :quality (:msi P10s)
		   :value (:range 31000 32000)
		   :timing 
		   (:start 
		    (= (+ (start-of +this-task+) P10S))
		    :end 
		    (= (+ (start-of +this-task+) P50s)))))
	 (result o2.3 :succeed P50S))
    
    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 31000)))
    (at P30s (signal-event '(alt ac1 = 32000)))
    (at P40s (signal-event '(alt ac1 = 32000)))
    (at P50s (signal-event '(alt ac1 = 32000)))    
    (at P60s (signal-event '(alt ac1 = 32000)))
    
    )
  
  (def-monitor-test 
      "Holding with discrete data"
      (task-start P0S) 
    (keep)
    (log (on-off switch1) :count-limit 1000)
    (log (on-off switch2) :count-limit 1000)    
    (at P10s (signal-event '(on-off switch1 = off)))
    (at P20s (signal-event '(on-off switch1 = off)))
    (at P30s (signal-event '(on-off switch1 = on)))
    (at P40s (signal-event '(on-off switch1 = on)))
    (at P50s (signal-event '(on-off switch1 = on)))    
    (at P60s (signal-event '(on-off switch1 = on)))
    
    (at P10s (signal-event '(on-off switch2 = on)))
    (at P20s (signal-event '(on-off switch2 = off)))
    (at P30s (signal-event '(on-off switch2 = off)))
    (at P40s (signal-event '(on-off switch2 = on)))
    (at P50s (signal-event '(on-off switch2 = off)))    
    (at P60s (signal-event '(on-off switch2 = on)))    

    (at P0s
	(waitfor 
	 (:episode o3.1a (on-off switch1)
		   :quality (:msi P10s)
		   :value (eql on)
		   :timing 
		   (:start 
		    (>= (+ (start-of +this-task+) P30S))
		    :end 
		    (= (+ (start-of +this-task+) P50s)))))
	(result o3.1a :succeed P50s))
    
    (at P0s
	(waitfor 
	 (:episode o3.1b (on-off ?switch)
		   :quality (:msi P10s)
		   :value (eql on)
		   :timing 
		   (:start 
		    (>= (+ (start-of +this-task+) P30S))
		    :end 
		    (= (+ (start-of +this-task+) P50s)))))
	(result o3.1b :succeed P50s))
    )

  (def-monitor-test
      "Atomic episodes in order..."
      (keep)
    (task-start P0S)
    (at P10s (signal-event '(test 1)))
    (at P20s (signal-event '(test 2)))
    (at P30s (signal-event '(test 2)))
    
    (at P0s 
	(waitfor (:in-order io1 (test 1)))
	(result io1 :succeed P10s))
    (at P0s 
	(waitfor (:in-order io2 (test 2)))
	(result io2 :succeed P20s))
    (at P0s 
	(waitfor (:in-order io3 (test 1) (test 2)))
	(result io3 :succeed P20s))
    
    (at P0s 
	(waitfor (:in-order io4 (test 2) (test 2)))
	(result io4 :succeed P30s)) 
    
    (at P0s 
	(waitfor (:in-order io4 (test 1) (test 2) (test 2)))
	(result io4 :succeed P30s))
    
    (at P0s 
	(waitfor (:atomic-episode se1 (test 1))) (result se1 :succeed P10s))
    
    (at P0s 
	(waitfor (:atomic-episode se2 (test 2))) (result se2 :succeed P20s))    
    
    )
  
  (def-monitor-test 
      "Rate changing"
      (keep)
      (task-start P0S)
    (log (alt ac1) :count-limit 1000)

    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 32000)))

    (at P30s (signal-event '(alt ac1 = 32000)))
    (at P40s (signal-event '(alt ac1 = 32000)))
    (at P50s (signal-event '(alt ac1 = 32000)))    
    (at P60s (signal-event '(alt ac1 = 32000)))

    ;; going up !! 
    
    (at P70s (signal-event  '(alt ac1 = 33000)))
    (at P80s (signal-event  '(alt ac1 = 34000)))
    (at P90s (signal-event  '(alt ac1 = 35000)))
    
    ;; hold altitude for a bit ...
    (at P100s (signal-event '(alt ac1 = 36000)))
    (at P110s (signal-event '(alt ac1 = 36000)))
    (at P120s (signal-event '(alt ac1 = 36000)))
    (at P130s (signal-event '(alt ac1 = 36000)))    
    (at P140s (signal-event '(alt ac1 = 36000)))    
    (at P150s (signal-event '(alt ac1 = 36000)))        
    
    ;; dive !!
    
    (at P160s (signal-event '(alt ac1 = 35000)))
    (at P170s (signal-event '(alt ac1 = 34000)))
    (at P180s (signal-event '(alt ac1 = 33000)))
    (at P190s (signal-event '(alt ac1 = 30000)))
    (at P200s (signal-event '(alt ac1 = 27000)))
    (at P210s (signal-event '(alt ac1 = 23000)))
    (at P220s (signal-event '(alt ac1 = 18000)))
    (at P230s (signal-event '(alt ac1 = 12000)))
    (at P240s (signal-event '(alt ac1 = 5000)))
    (at P250s (signal-event '(alt ac1 = 0)))
    (at P260s (signal-event '(alt ac1 = 0)))    
    
  
  (at P0s
	(waitfor 
	 (:episode o4.1a (alt ac1)
		   :quality (:msi P10s)
		   :timing 
		   (:duration (>= P30S))
		   :trend (:step (< 10))))
	(result o4.1a :succeed P40s))
  
  (at P0s
	(waitfor 
	 (:episode o4.2 (alt ac1)
		   :quality (:msi P10s)
		   :timing 
		   (:duration (>= P30S))
		   :trend (:step (> 10))))
	(result o4.2 :succeed P90s))
  
  (at P0s
	(waitfor 
	 (:episode o4.3 (alt ac1)
		   :quality (:msi P10s)
		   :timing 
		   (:duration (>= P30S))
		   :trend (:rate :increasing)))
	(result o4.3 :succeed P70s))
  
  (at P0s
	(waitfor 
	 (:episode o4.4 (alt ac1)
		   :quality (:msi P10s)
		   :timing 
		   (:duration (>= P30S))
		   :trend (:step :decreasing)))
	(result o4.4 :succeed P180s))
  
  (at P0s
	(waitfor 
	 (:episode o4.5 (alt ac1)
		   :quality (:msi P10s)
		   :timing 
		   (:duration (>= P30S))
		   :trend (:step :increasing)))
	(result o4.5 :succeed P90s))
  
  (at P0s
	(waitfor 
	 (:episode o4.6 (alt ac1)
		   :quality (:msi P10s)
		   :timing 
		   (:duration (>= P30S))
		   (:start (>= (+ (start-of +this-task+) P100S))
			   (< (+ (start-of +this-task+) P200S)))
		   :first-value (> 0) 
		   :last-value (< 10000000) (> 0) (evenp)
		   :trend (:rate :decreasing)))
	(result o4.6 :succeed P160s))  
  )
)
