;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/system/utility/metrics.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: metrics.lisp,v 1.2 2006/01/15 03:43:02 dalal Exp $



(defmacro with-metered-body ((name recorder) &body body)
  (let ((runname (gensym ))
	(realname (gensym )))
    `(let ((,runname (get-internal-run-time))
	   (,realname (get-internal-real-time)))
       (unwind-protect (progn ,@body)
	 (progn
	   (add-meter-info 'run-time ',name ,recorder (- (get-internal-run-time) ,runname))
	   (add-meter-info 'real-time ',name ,recorder (- (get-internal-real-time) ,realname))
	   (add-meter-info 'count ',name ,recorder 1))))))

(defun add-meter-info (kind name recorder value)
  (let ((key `(,kind ,name)))
    (let ((val (gethash key recorder)))
      (if val 
	(incf (gethash key recorder) value)
	(setf (gethash key recorder) value)))))

(defun clear-meter-info (kind name recorder)
  (remhash `(,kind ,name) recorder))

(defun clear-meter-info-all (recorder)
  (clrhash recorder))

(defun meter-info (kind name recorder)
  (values (gethash `(,kind ,name) recorder)))

(defvar *apex-metric* (make-hash-table :test #'equal)
  "Apex Metrics recorder")

(defun apex-asa-counts (&optional (metric *apex-metric*))
  (meter-info 'count 'asamain metric))

(defun apex-monitor-counts (&optional  (metric *apex-metric*))
  (meter-info 'count 'check-cogevent-match metric))
      

(defun meter-info-report (kind name &optional (metric *apex-metric*) (stream *standard-output*))
  (format stream ";;; ~s ~s: ~s~%" kind name (meter-info kind name metric)))

(defun x-per-y-report (x y xval yval &optional (stream *standard-output*))
  (format stream ";;; ~a/~a: " x y)
  (if (or (not (realp xval)) (not (realp yval)) (zerop yval))
    (format stream "n/a~%")
    (format stream "~7,2,f~%" (/ xval yval))))

(defun call-report (fname &optional (metric *apex-metric*) (stream *standard-output*))
  (meter-info-report 'count fname metric stream)
  (meter-info-report 'run-time fname metric stream)
  (meter-info-report 'real-time fname metric stream)
  (x-per-y-report  'calls "second (runtime)"
		   (meter-info 'count fname metric)
		   (if (meter-info 'run-time fname metric) 
		     (/ (meter-info 'run-time fname metric) 1000)
		    0)
		   stream)
  (x-per-y-report 'ms "call (run-time)"
		  (meter-info 'run-time fname metric)
		  (meter-info 'count fname metric)
		  stream)
  (x-per-y-report  'calls "second (real-time)"
		   (meter-info 'count fname metric)
		   (if (meter-info 'real-time fname metric) 
		     (/ (meter-info 'real-time fname metric) 1000)
		     0)
		   stream)
  (x-per-y-report 'ms "call (real-time)"
		  (meter-info 'real-time fname metric)
		  (meter-info 'count fname metric)
		  stream)
  )
  

(defun metric-report (&optional (metric *apex-metric*) (stream *standard-output*))
  (format stream ";;; Apex metric report ")
  (datetime-princ (current-datetime) stream)
  (terpri stream)
  (dolist (call '(startapp asamain check-cogevent-match))
    (format stream ";;;--~%")
    (call-report call metric stream)
    )
  (values))

(def-fwrapper asamain-wrapper (agent)
  (with-metered-body (asamain *apex-metric*)
    (call-next-fwrapper)))

(def-fwrapper check-cogevent-match-wrapper (cogevent monitor)
  (with-metered-body (check-cogevent-match *apex-metric*)
    (call-next-fwrapper)))

(def-fwrapper startapp-wrapper ()
  (with-metered-body (startapp *apex-metric*)
    (call-next-fwrapper)))

(defun start-apex-metrics ()
  (clear-meter-info-all *apex-metric*)
  (funwrap  'asamain 'metrics)
  (fwrap    'asamain 'metrics 'asamain-wrapper)
  (funwrap  'check-cogevent-match 'metrics)
  (fwrap  'check-cogevent-match 'metrics 'check-cogevent-match-wrapper)
  (funwrap  'startapp 'metrics)
  (fwrap  'startapp 'metrics 'startapp-wrapper)  
  )

(defun stop-apex-metrics ()
  (funwrap  'asamain 'metrics)
  (funwrap  'check-cogevent-match 'metrics)
  (funwrap  'startapp 'metrics)
  (metric-report *apex-metric*))

(defun run-with-metric (pathname &key (and-report t) (hide-trace t))
  (load-application-file pathname)
  (start-apex-metrics)
  (when hide-trace (unshow :runtime))
  (startapp)
  (when and-report (stop-apex-metrics)))




  

