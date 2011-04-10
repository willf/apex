;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/regression-tests/misc-monitors-complex-episodes.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: misc-monitors-complex-episodes.lisp,v 1.7 2006/01/15 03:42:59 dalal Exp $

(unless (fboundp 'signal-test-monitor)
  (load "system/utility/monitor-testing-framework.lisp"))
(procedure (index (test-monitor-noop)))
(apex.utility.unit-test:with-tests (:name "Complex Monitors")  
  
  
  (def-monitor-test 
      "AND"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (log (temp ac1) :count-limit 1000)    
    (at P10s (signal-event '(alt ac1 = 10)))
    (at P20s (signal-event '(alt ac1 = 20)))
    (at P30s (signal-event '(alt ac1 = 30)))
    (at P40s (signal-event '(alt ac1 = 40)))
    (at P50s (signal-event '(alt ac1 = 50)))
    
    (at P0s 
	(waitfor 
	 (:before
	  and.1
	  (:measurement m1 (alt ac1 = 10))
	  (:measurement m2 (alt ac1 = 20))
	  (:measurement m3 (alt ac1 = 30))))
	(result and.1 :succeed P30s))
    
    )
  
  (def-monitor-test 
      "Before"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (log (temp ac1) :count-limit 1000)    
    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 33000)))
    (at P30s (signal-event '(alt ac1 = 33000)))
    (at P40s (signal-event '(alt ac1 = 34000)))
    (at P50s (signal-event '(alt ac1 = 34000)))
    
    (at P10s (signal-event '(temp ac1 = 32)))
    (at P20s (signal-event '(temp ac1 = 33)))
    (at P30s (signal-event '(temp ac1 = 33)))
    (at P40s (signal-event '(temp ac1 = 34)))
    (at P50s (signal-event '(temp ac1 = 34)))    

    (at P0s 
	(waitfor 
	 (:before
	  o1.1
	  (alt ac1 = 32000)
	  (alt ac1 = 33000)))
	(result o1.1 :succeed P20S))
    (at P0s 
	(waitfor 
	 (:before
	  o1.2
	  (:measurement m1 (alt ac1 = ?val))
	  (:measurement m2 (alt ac1 = <?val>))))
	(result o1.2 :succeed P30S))
    
    (at P0s 
	(waitfor 
	 (:in-order o1.3
		    (:measurement m1 (alt ac1 = 32000))
		    (:measurement m2 (alt ac1 = 33000))
		    (:measurement m3 (alt ac1 = 33000))))
	(result o1.3 :succeed P30S))
    (at P0s
	(waitfor
	 (:before o1.4
		  (:episode e1 (alt ac1) :quality (:msi P10S)
			    :value (= 33000)
			    :timing (:duration (>= P10S)))
		  (:episode e2 (alt ac1) :quality (:msi P10S)
			    :value (= 34000)
			    :timing (:duration (>= P10S)))))
	(result o1.4 :succeed P50s))
    (at P0s 
	(waitfor 
	 (:before o1.5 
		  (alt ac1 = 32000)
		  (temp ac1 = 33)))
	(result o1.5 :succeed P20S))
    )
  
  (def-monitor-test 
      "Before"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (log (temp ac1) :count-limit 1000)    
    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 33000)))
    (at P30s (signal-event '(alt ac1 = 33000)))
    (at P40s (signal-event '(alt ac1 = 34000)))
    (at P50s (signal-event '(alt ac1 = 34000)))
    
    (at P10s (signal-event '(temp ac1 = 32)))
    (at P20s (signal-event '(temp ac1 = 33)))
    (at P30s (signal-event '(temp ac1 = 33)))
    (at P40s (signal-event '(temp ac1 = 34)))
    (at P50s (signal-event '(temp ac1 = 34)))    

    (at P0s 
	(waitfor 
	 (:before
	  o1.1
	  (alt ac1 = 32000)
	  (alt ac1 = 33000)))
	(result o1.1 :succeed P20S))
    (at P0s 
	(waitfor 
	 (:before
	  o1.2
	  (:measurement m1 (alt ac1 = ?val))
	  (:measurement m2 (alt ac1 = <?val>))))
	(result o1.2 :succeed P30S))
    
    (at P0s 
	(waitfor 
	 (:in-order o1.3
		    (:measurement m1 (alt ac1 = 32000))
		    (:measurement m2 (alt ac1 = 33000))
		    (:measurement m3 (alt ac1 = 33000))))
	(result o1.3 :succeed P30S))
    (at P0s
	(waitfor
	 (:before o1.4
		  (:episode e1 (alt ac1) :quality (:msi P10S)
			    :value (= 33000)
			    :timing (:duration (>= P10S)))
		  (:episode e2 (alt ac1) :quality (:msi P10S)
			    :value (= 34000)
			    :timing (:duration (>= P10S)))))
	(result o1.4 :succeed P50s))
    (at P0s 
	(waitfor 
	 (:before o1.5 
		  (alt ac1 = 32000)
		  (temp ac1 = 33)))
	(result o1.5 :succeed P20S))
    )
  
  (def-monitor-test 
      "And"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (at P10s (signal-event '(alt ac1 = 1)))
    (at P20s (signal-event '(alt ac1 = 2)))
    (at P30s (signal-event '(alt ac1 = 3)))
    (at P40s (signal-event '(alt ac1 = 4)))    
    
    (at P110s (signal-event '(alt ac1 = 1)))
    (at P120s (signal-event '(alt ac1 = 2)))
    (at P130s (signal-event '(alt ac1 = 3)))
    (at P140s (signal-event '(alt ac1 = 4)))    
    
    (at P210s (signal-event '(alt ac1 = 1)))
    (at P220s (signal-event '(alt ac1 = 2)))
    (at P230s (signal-event '(alt ac1 = 3)))
    (at P240s (signal-event '(alt ac1 = 4)))    
    
    (at P310s (signal-event '(alt ac1 = 1)))
    (at P320s (signal-event '(alt ac1 = 2)))
    (at P330s (signal-event '(alt ac1 = 3)))
    (at P340s (signal-event '(alt ac1 = 4)))    
    
    (at P410s (signal-event '(alt ac1 = 1)))
    (at P420s (signal-event '(alt ac1 = 2)))
    (at P430s (signal-event '(alt ac1 = 3)))    
    (at P440s (signal-event '(alt ac1 = 4)))    
    
    (at P510s (signal-event '(alt ac1 = 1)))
    (at P520s (signal-event '(alt ac1 = 2)))
    (at P530s (signal-event '(alt ac1 = 3)))    
    (at P540s (signal-event '(alt ac1 = 4)))    
    
    (at P610s (signal-event '(alt ac1 = 1)))
    (at P620s (signal-event '(alt ac1 = 2)))
    (at P630s (signal-event '(alt ac1 = 3)))    
    (at P640s (signal-event '(alt ac1 = 4)))        
    
    (at P1000s (signal-event '(alt ac1 = 5)))


    (at P0s 
	(waitfor 
	 (:in-order
	  o2.1
	  (:in-order (alt ac1 = 1)
		(alt ac1 = 2)
		(alt ac1 = 3)
		(alt ac1 = 4) )
	  (alt ac1 = 5))
	  )
	
	(result o2.1 :succeed P1000S))
    )
  
  
    
  
  (def-monitor-test 
      "OR"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (log (temp ac1) :count-limit 1000)    
    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 33000)))
    (at P30s (signal-event '(alt ac1 = 33000)))
    (at P40s (signal-event '(alt ac1 = 34000)))
    (at P50s (signal-event '(alt ac1 = 34000)))
    
    (at P10s (signal-event '(temp ac1 = 32)))
    (at P20s (signal-event '(temp ac1 = 33)))
    (at P30s (signal-event '(temp ac1 = 33)))
    (at P40s (signal-event '(temp ac1 = 34)))
    (at P50s (signal-event '(temp ac1 = 34)))    

    (at P0s 
	(waitfor 
	 (:or
	  o3.1
	  (alt ac1 = 32000)
	  (alt ac1 = 33000)))
	(result o3.1 :succeed P10S))

    (at P0s 
	(waitfor 
	 (:or
	  o3.2
	  (alt ac1 = 33000)
	  (alt ac1 = 32000)))
	(result o3.2 :succeed P10S))
    
    (at P0s 
	(waitfor 
	 (:or o3.3
	      (:measurement m1 (alt ac1 = 132000))
	      (:measurement m2 (alt ac1 = 133000))
	      (:measurement m3 (alt ac1 = 134000))
	      (:measurement m4 (temp ac1 = 34))))
	(result o3.3 :succeed P40S))
    (at P0s
	(waitfor
	 (:or o3.4
		  (:episode e1 (alt ac1) :quality (:msi P10S)
			    :value (= 33000)
			    :timing (:duration (>= P10S)))
		  (:episode e2 (alt ac1) :quality (:msi P10S)
			    :value (= 34000)
			    :timing (:duration (>= P10S)))))
	(result o3.4 :succeed P30s))
    (at P0s 
	(waitfor 
	 (:or o3.5 
		  (alt ac1 = 32000)
		  (temp ac1 = 33)))
	(result o3.5 :succeed P10S))
    )

(def-monitor-test 
      "Allen"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (log (temp ac1) :count-limit 1000)    
    (at P10s (signal-event '(alt ac1 = 32000)))
    (at P20s (signal-event '(alt ac1 = 32000)))
    (at P30s (signal-event '(alt ac1 = 32000)))
    (at P40s (signal-event '(alt ac1 = 32000)))
    (at P50s (signal-event '(alt ac1 = 32000)))
    
    (at P10s (signal-event '(temp ac1 = 50)))
    (at P20s (signal-event '(temp ac1 = 50)))
    (at P30s (signal-event '(temp ac1 = 50)))
    (at P40s (signal-event '(temp ac1 = 50)))
    (at P50s (signal-event '(temp ac1 = 50)))    

    (at P0s 
	(waitfor 
	 (:episode o4.0 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:start (>= P20s) :duration (>= P10S))))

	(result o4.0 :succeed P30S))

    
    ;; CONTAINS
    
    (at P0s 
	(waitfor 
	 (:contains
	  o4.1c
	  (:episode e1 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))
	  (temp ac1 = 50)))

	(result o4.1c :succeed P30S))
    
    (at P0s 
	(waitfor 
	 (:contains
	  o4.1d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.1d :succeed P40S))
    
    
    
    ;; FINISHES
    
     
    (at P0s 
	(waitfor 
	 (:finishes
	  o4.2b
	  (temp ac1 = 50)
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))))
	(result o4.2b :succeed P20S))

    
    (at P0s 
	(waitfor 
	 (:finishes
	  o4.2d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )
	
	(result o4.2d :succeed P30S))
    
    ;; STARTS

    
     
    (at P0s 
	(waitfor 
	 (:starts
	  o4.3b
	  (temp ac1 = 50)
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))))
	(result o4.3b :succeed P20S))

    
    (at P0s 
	(waitfor 
	 (:starts
	  o4.3d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.3d :succeed P30S))

    ;; BEFORE 
    
     (at P0s 
	 (waitfor 
	  (:before
	   4.4a
	   (temp ac1 = 50)
	   (temp ac1 = 50)))
          (result 4.4a :succeed P20s))
     
     (at P0s 
	 (waitfor 
	  (:before
	   4.4b
	   (temp ac1 = 50)
	   (:episode e2 (alt ac1) :quality (:msi P10S)
		     :value (= 32000)
		     :timing (:duration (>= P10S)))))
	 (result 4.4b :succeed P30s))

     (at P0s 
	 (waitfor 
	  (:before
	   4.4c
	   (:episode e1 (alt ac1) :quality (:msi P10S)
		     :value (= 32000)
		     :timing (:duration (>= P10S)))
	   (temp ac1 = 50)))
	 
	 (result 4.4c :succeed P30S))
    
    (at P0s 
	(waitfor 
	 (:before
	  4.4d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )
	
	(result 4.4d :succeed P40S))
    
    ;; MEETS
    
     (at P0s 
	 (waitfor 
	  (:meets
	   o4.5a
	   (temp ac1 = 50)
	   (temp ac1 = 50)))
	  (result o4.5a :succeed P10s))

     
     (at P0s 
	  (waitfor 
	   (:meets
	    o4.5b
	    (temp ac1 = 50)
	    (:episode e2 (alt ac1) :quality (:msi P10S)
		      :value (= 32000)
		      :timing (:duration (>= P10S)))))
          (result o4.5b :succeed P20s))

    (at P0s 
	(waitfor 
	 (:meets
	  o4.5c
	  (:episode e1 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))
	  (temp ac1 = 50)))

	(result o4.5c :succeed P20S))
    
    (at P0s 
	(waitfor 
	 (:meets
	  o4.5d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.5d :succeed P30S))
    
    ;; OVERLAPS

    
    
    (at P0s 
	(waitfor 
	 (:overlaps
	  o4.6d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.6d :succeed P40S))  
    
    ;; COTEMPORAL
    
    (at P0s
	(waitfor 
	 (:cotemporal
	  o4.7a
	  (temp ac1 = 50)
	  (temp ac1 = 50)))
	(result o4.7a :succeed P10S))
	 
    (at P0s 
	(waitfor 
	 (:cotemporal
	  o4.7b
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.7b :succeed P20S))

    ;; DURING
    

     
     (at P0s 
	 (waitfor 
	  (:during
	   o4.8b
	   (temp ac1 = 50)
	   (:episode e2 (alt ac1) :quality (:msi P10S)
		      :value (= 32000)
		      :timing (:duration (>= P10S)))))
          (result o4.8b :succeed P30s))

    
    (at P0s 
	(waitfor 
	 (:during
	  o4.8d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.8d :succeed P40S))

    ;; FINISHED-BY
    

    (at P0s 
	(waitfor 
	 (:finished-by
	  o4.9c
	  (:episode e1 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))
	  (temp ac1 = 50)))

	(result o4.9c :succeed P20S))
    
    (at P0s 
	(waitfor 
	 (:finished-by
	  o4.9d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.9d :succeed P30S))
    
    ;; STARTED-BY


    (at P0s 
	(waitfor 
	 (:started-by
	  o4.10c
	  (:episode e1 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))
	  (temp ac1 = 50)))
	
	(result o4.10c :succeed P20S))
    
    (at P0s 
	(waitfor 
	 (:started-by
	  o4.10d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.10d :succeed P30S))    
    ;; AFTER
    
     (at P0s 
	  (waitfor 
	   (:after
	    o4.11a
	    (temp ac1 = 50)
	    (temp ac1 = 50)))
          (result o4.11a :succeed P20s))
     
     (at P0s 
	  (waitfor 
	   (:after
	    o4.11b
	    (temp ac1 = 50)
	    (:episode e2 (alt ac1) :quality (:msi P10S)
		      :value (= 32000)
		      :timing (:duration (>= P10S)))))
          (result o4.11b :succeed P30s))

     (at P0s 
	 (waitfor 
	  (:after
	   o4.11c
	   (:episode e1 (alt ac1) :quality (:msi P10S)
		     :value (= 32000)
		     :timing (:duration (>= P10S)))
	   (temp ac1 = 50)))

	 (result o4.11c :succeed P30S))
    
     (at P0s 
	 (waitfor 
	  (:after
	   o4.11d
	   (:episode e1 (temp ac1) :quality (:msi P10S)
		     :value (= 50)
		     :timing (:duration (>= P10S)))
	   (:episode e2 (alt ac1) :quality (:msi P10S)
		     :value (= 32000)
		     :timing (:duration (>= P10S))))
	  )
	 
	 (result o4.11d :succeed P40S))
    
     ;; MET-BY
     


    (at P0s 
	(waitfor 
	 (:met-by
	  o4.12c
	  (:episode e1 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))
	  (temp ac1 = 50)))
	
	(result o4.12c :succeed P20S))
    
    (at P0s 
	(waitfor 
	 (:met-by
	  o4.12d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )
	
	(result o4.12d :succeed P30S))
    
    ;; OVERLAPPED-BY
    


    (at P0s 
	(waitfor 
	 (:overlapped-by
	  o4.13d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result o4.13d :succeed P40S))
    ;; IN-ORDER
    
    (at P0s 
	 (waitfor 
	   (:in-order
	    4.14a
	    (temp ac1 = 50)
	    (temp ac1 = 50)))
          (result 4.14a :succeed P20s))
     
     (at P0s 
	  (waitfor 
	   (:in-order
	    4.14b
	    (temp ac1 = 50)
	    (:episode e2 (alt ac1) :quality (:msi P10S)
		      :value (= 32000)
		      :timing (:duration (>= P10S)))))
          (result 4.14b :succeed P30s))

    (at P0s 
	(waitfor 
	 (:in-order
	  4.14c
	  (:episode e1 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S)))
	  (temp ac1 = 50)))

	(result 4.14c :succeed P30S))
    
    (at P0s 
	(waitfor 
	 (:in-order
	  4.14d
	  (:episode e1 (temp ac1) :quality (:msi P10S)
		    :value (= 50)
		    :timing (:duration (>= P10S)))
	  (:episode e2 (alt ac1) :quality (:msi P10S)
		    :value (= 32000)
		    :timing (:duration (>= P10S))))
	 )

	(result 4.14d :succeed P40S))
    )  

(def-monitor-test 
      "Delays"
      (ignore)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (at P10s (signal-event '(alt ac1 = 33)))
    (at P15s (signal-event '(alt ac1 = 33)))    
    (at P20s (signal-event '(alt ac1 = 33)))
    (at P25s (signal-event '(alt ac1 = 33)))    
    (at P30s (signal-event '(alt ac1 = 35)))
    (at P35s (signal-event '(alt ac1 = 35)))
    (at P40s (signal-event '(alt ac1 = 35)))
    (at P45s (signal-event '(alt ac1 = 35)))    
    (at P50s (signal-event '(alt ac1 = 36)))

    (at P0s 
	(waitfor 
	 (:in-order
	  o5.1
	  (:measurement m1 (alt ac1 = 33))
	  (:delay P25s) 
	  (:measurement m2 (alt ac1 = 35))
	  ))
	(result o5.1 :succeed P35S))
    
    (at P0s 
	(waitfor 
	 (:in-order
	  o5.2
	  (:measurement m1 (alt ac1 = 33))
	  (:delay P25s P35s) 
	  (:measurement m2 (alt ac1 = 35))
	  ))
	(result o5.2 :succeed P35S))
    
    (at P0s 
	(waitfor 
	 (:in-order
	  o5.3
	  (:delay P35s P45s) 
	  (:measurement m2 (alt ac1 = 35))
	  ))
	(result o5.3 :succeed P35S))
    
    (at P0s 
	(waitfor 
	 (:in-order
	  o5.4
	  (:measurement m1 (alt ac1 = 33))
	  (:delay P25s P35s) 
	  ))
	(result o5.4 :succeed P35S))
    
    (at P0s 
	(waitfor 
	 (:episode e1 (alt ac1)
		   :quality (:msi P10S)
		   :value (= 33)
		   :timing (:duration (>= P10S))))	 
	(result e1 :succeed P20S))

    (at P0s 
	(waitfor 
	 (:episode e2 (alt ac1)
		   :quality (:msi P10S)
		   :value (= 35)
		   :timing (:duration (>= P10S))))	 
	(result e2 :succeed P40S))
    
    (at P0s
	(waitfor
	 (:in-order o5.5
	  (:episode e1 (alt ac1)
		   :quality (:msi P10S)
		   :value (= 33)
		   :timing (:duration (>= P10S)))
	  (:delay P15s P30s)
	  (:episode e2 (alt ac1)
		    :quality (:msi P10S)
		    :value (= 35)
		    :timing (:duration (>= P10S)))))
	(result o5.5 :succeed P45s))
    
    (at P0s
	(waitfor
	 (:in-order o5.6
	  (:delay P35s P45s)
	  (:episode e2 (alt ac1)
		    :quality (:msi P10S)
		    :value (= 35)
		    :timing (:duration (>= P10S)))))
	(result o5.6 :succeed P45s))
    )
    (def-monitor-test 
      "Hierarchical"
      (keep)
      (task-start P0S)
    (log (speed car) :count-limit 1000)
    (at P15s (signal-event '(speed car = 15)))
    (at P20s (signal-event '(speed car = 20)))
    (at P25s (signal-event '(speed car = 25)))
    (at P30s (signal-event '(speed car = 30)))
    (at P35s (signal-event '(speed car = 35)))
    (at P40s (signal-event '(speed car = 40)))
    (at P45s (signal-event '(speed car = 45)))
    (at P50s (signal-event '(speed car = 50)))

    (at P0s
	(waitfor 
	 (:in-order o6.1
	  (speed car = 15)
	  (speed car = 20)
	  (speed car = 25)
	  (speed car = 30)
	  (speed car = 35)
	  (speed car = 40)
	  (speed car = 45)
	  (speed car = 50)
	  ))
	(result o6.1 :succeed P50s))
    
    (at P0s
	(waitfor 
	 (:in-order o6.2
		    (:in-order
		     (speed car = 15)
		     (speed car = 20)
		     (speed car = 25)
		     (speed car = 30))
		    (:in-order 
		     (speed car = 35)
		     (speed car = 40)
		     (speed car = 45)
		     (speed car = 50)
		     )))
	(result o6.2 :succeed P50s))
    
    (at P0s
	(waitfor 
	 (:in-order o6.3
		    (:and
		     (speed car = 15)
		     (speed car = 20)
		     (speed car = 25)
		     (speed car = 30))
		    (:in-order 
		     (speed car = 35)
		     (speed car = 40)
		     (speed car = 45)
		     (speed car = 50)
		     )))
	(result o6.3 :succeed P50s))
    
    (at P0s
	(waitfor 
	 (:in-order o6.4
		    (:in-order
		     (speed car = 15)
		     (:and
		      (speed car = 20)
		      (speed car = 25)
		      (:in-order 
		       (speed car = 30)))
		     (:in-order 
		      (:and
		       (speed car = 35)
		       (speed car = 40)
		       (speed car = 45)
		       (:or 
			(speed car = 50)))))))

	(result o6.3 :succeed P50s))
    
    )
(def-monitor-test 
      "NOT"
      (keep)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (at P10s (signal-event '(alt ac1 = 1)))
    (at P20s (signal-event '(alt ac1 = 2)))
    (at P30s (signal-event '(alt ac1 = 3)))
    (at P40s (signal-event '(alt ac1 = 4)))    
    
    (at P110s (signal-event '(alt ac1 = 1)))
    (at P120s (signal-event '(alt ac1 = 2)))
    (at P130s (signal-event '(alt ac1 = 3)))
    (at P140s (signal-event '(alt ac1 = 4)))    
    
    (at P210s (signal-event '(alt ac1 = 1)))
    (at P220s (signal-event '(alt ac1 = 2)))
    (at P230s (signal-event '(alt ac1 = 3)))
    (at P240s (signal-event '(alt ac1 = 4)))    
    
    (at P310s (signal-event '(alt ac1 = 1)))
    (at P320s (signal-event '(alt ac1 = 2)))
    (at P330s (signal-event '(alt ac1 = 3)))
    (at P340s (signal-event '(alt ac1 = 4)))    
    
    (at P410s (signal-event '(alt ac1 = 1)))
    (at P420s (signal-event '(alt ac1 = 2)))
    (at P430s (signal-event '(alt ac1 = 3)))    
    (at P440s (signal-event '(alt ac1 = 4)))    
    
    (at P510s (signal-event '(alt ac1 = 1)))
    (at P520s (signal-event '(alt ac1 = 2)))
    (at P530s (signal-event '(alt ac1 = 3)))    
    (at P540s (signal-event '(alt ac1 = 4)))    
    
    (at P610s (signal-event '(alt ac1 = 1)))
    (at P620s (signal-event '(alt ac1 = 2)))
    (at P630s (signal-event '(alt ac1 = 3)))    
    (at P640s (signal-event '(alt ac1 = 4)))        
    
    (at P1000s (signal-event '(alt ac1 = 5)))

    (at P0s 
	(waitfor 
	 (:not
	  o6.1
	  (:in-order 
	   (alt ac1 = 1)
	   (alt ac1 = 2)
	   (alt ac1 = 3)
	   (alt ac1 = 4 ))))
	
	(result o6.1 :succeed P0S))
    
    (at P800s 
	(waitfor 
	 (:not o6.2 (:measurement o6.2a  (alt ac1 = 1)
		       :timestamp (> 0) (< (+ (start-of +this-task+) P1500s)))))
	
	(result o6.2 :failure))
    
    
    (at P500s
	(waitfor
	 (:episode o6.3 (alt ac1) :quality (:msi P10s)
		   :timing 
		   (:start (= (+ (start-of +this-task+) P410s)))
		   (:end   (= (+ (start-of +this-task+) P440s)))
		   ))
	(result o6.3 :succeed P500s))
    
    (at P500s
	(waitfor
	 (:episode o6.4 (alt ac1) :quality (:msi P1s)
		   :timing 
		   (:start (= (+ (start-of +this-task+) P410s)))
		   (:end   (= (+ (start-of +this-task+) P440s)))
		   ))
	(result o6.4 :failure))
    
    (at P500s
	(waitfor
	 (:not  o6.5 (:episode (alt ac1) :quality (:msi P1s)
			 :timing 
			 (:start (= (+ (start-of +this-task+) P410s)))
			 (:end   (= (+ (start-of +this-task+) P440s)))
			 )))
	(result o6.5 :succeed P500s))
    
    )
(def-monitor-test 
      "General Constraints"
      (ignore)
    (task-start P0S)
    (log (alt ac1) :count-limit 1000)
    (at P10s (signal-event '(alt ac1 = 1)))
    (at P20s (signal-event '(alt ac1 = 2)))
    (at P30s (signal-event '(alt ac1 = 3)))
    (at P40s (signal-event '(alt ac1 = 4)))    
    
    (at P110s (signal-event '(alt ac1 = 1)))
    (at P120s (signal-event '(alt ac1 = 2)))
    (at P130s (signal-event '(alt ac1 = 3)))
    (at P140s (signal-event '(alt ac1 = 4)))    
    
;;;    (at P210s (signal-event '(alt ac1 = 1)))
;;;    (at P220s (signal-event '(alt ac1 = 2)))
;;;    (at P230s (signal-event '(alt ac1 = 3)))
;;;    (at P240s (signal-event '(alt ac1 = 4)))    
;;;    
;;;    (at P310s (signal-event '(alt ac1 = 1)))
;;;    (at P320s (signal-event '(alt ac1 = 2)))
;;;    (at P330s (signal-event '(alt ac1 = 3)))
;;;    (at P340s (signal-event '(alt ac1 = 4)))    
;;;    
;;;    (at P410s (signal-event '(alt ac1 = 1)))
;;;    (at P420s (signal-event '(alt ac1 = 2)))
;;;    (at P430s (signal-event '(alt ac1 = 3)))    
;;;    (at P440s (signal-event '(alt ac1 = 4)))    
;;;    
;;;    (at P510s (signal-event '(alt ac1 = 1)))
;;;    (at P520s (signal-event '(alt ac1 = 2)))
;;;    (at P530s (signal-event '(alt ac1 = 3)))    
;;;    (at P540s (signal-event '(alt ac1 = 4)))    
;;;    
;;;    (at P610s (signal-event '(alt ac1 = 1)))
;;;    (at P620s (signal-event '(alt ac1 = 2)))
;;;    (at P630s (signal-event '(alt ac1 = 3)))    
;;;    (at P640s (signal-event '(alt ac1 = 4)))        
;;;    
;;;    (at P1000s (signal-event '(alt ac1 = 5)))

    (qat P0s 
	(waitfor 
	 (:in-order 
	  o7.1
	   (alt ac1 = 1)
	   (alt ac1 = 2)
	   (alt ac1 = 3)
	   (alt ac1 = 4)
	   :constraints (> 10 2)))
	o7.1 :succeed P40S)
    
    (qat P0s 
	(waitfor 
	 (:in-order 
	  o7.2
	   (alt ac1 = 1)
	   (alt ac1 = 2)
	   (alt ac1 = 3)
	   (alt ac1 = 4)
	   :constraints (> (start-of ?o7.2) P10s)))
	o7.2 :succeed P140S)
    
    (qat P0s 
	(waitfor 
	 (:in-order 
	  o7.3
	   (:measurement o7.3a (alt ac1 = 1))
	   (alt ac1 = 2)
	   (alt ac1 = 3)
	   (alt ac1 = 4)
	   :constraints (> (start-of ?o7.3a) P10s)))
	o7.3 :succeed P140S)
    
    (at P0s 
	(waitfor 
	 (:in-order 
	  o7.4
	   (:measurement o7.4a (alt ac1 = 1))
	   (alt ac1 = 2)
	   (alt ac1 = 3)
	   (alt ac1 = 4)
	   :constraints (< (end-of ?o7.4a) P10s)))
	o7.4 :succeed P40S)
    
    (qat P0s 
	(waitfor 
	 (:in-order 
	  o7.5
	   (:measurement o7.5a (alt ac1 = 1))
	   (alt ac1 = 2)
	   (alt ac1 = 3)
	   (alt ac1 = 4)
	   :constraints (< (end-of ?o7.5a) P10s)))
	o7.5 :succeed P40S)
    
    (qat P0s 
	(waitfor 
	 (:in-order 
	  o7.6
	   (:measurement o7.6a (alt ac1 = 1))
	   (alt ac1 = 2)
	   (alt ac1 = 3)
	   (:measurement o7.6b (alt ac1 = 4))
	   :constraints (> (- (start-of ?o7.6b) (end-of ?o7.6a)) P10s)))
	o7.6 :succeed P140S)
    
    ) ;; end of constraints
  )