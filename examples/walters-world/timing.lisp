;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/timing.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: timing.lisp,v 1.5 2006/01/15 03:42:57 dalal Exp $

(in-package :common-lisp-user)

(defvar *procedureTimingList*)
(setf *procedureTimingList* nil)
;;;list of (procedureID (startTime endTime) (startTime endTime) ...)

(defvar *ongoingTaskStartTimes*)
(setf *ongoingTaskStartTimes* nil)
;;;list of (taskID procedureID startTime)

(defun startTaskTiming (taskID procedureID startTime)
  (if (equal (assoc taskID *ongoingTaskStartTimes* :test #'equal) nil)
      (setf *ongoingTaskStartTimes* (append *ongoingTaskStartTimes* 
                     (list (list taskID procedureID startTime))))
    nil))


(defun addToTimingList (procedureID startTime endTime)
  (let ((procedureRecord (assoc procedureID *procedureTimingList* 
                                :test #'equal)))
    (if (null procedureRecord)
        (setf *procedureTimingList* 
          (append *procedureTimingList* (list (list procedureID 
                                                (list startTime endTime)))))
      (setf *procedureTimingList*
        (append (remove procedureRecord *procedureTimingList*) 
                (list (append procedureRecord (list 
                                               (list startTime endTime)))))))))

(defun endTaskTiming (taskID endTime)
  (let ((taskStart (assoc taskID *ongoingTaskStartTimes* :test #'equal)))
    (if (null taskStart)
       nil
      (and (addToTimingList (second taskStart) (third taskStart) endTime)
           (setf *ongoingTaskStartTimes* 
             (remove taskStart *ongoingTaskStartTimes*))))))

(defun computeAverage (occurances sum timeList)
  (if (null timeList)
      (/ sum occurances)
    (computeAverage (1+ occurances) (+ sum (- (second (first timeList));;
                                             (first (first timeList))))
                    (rest timeList))))

(defun sum (list)
  (if (null list) 0 (+ (first list) (sum (rest list)))))

(defun subt (pair)
  (- (second pair) (first pair)))

(defun singleVar (num avg)
  (let ((dif (- num avg)))
    (* dif dif)))

(defun variance (times count avg)
  (let ((diffList (mapcar #'(lambda (x) (singleVar x avg)) times)))
    (/ (sum diffList) count)))

(defun avgAndSD (listOfPairs)
  (let* ((times (mapcar #'subt listOfPairs))
         (count (length times))
         (avg (/ (sum times) count)))
    (list avg (sqrt (variance times count avg)) count)))


(defun guessDuration(procedureID)
  (let ((procedureRecord (assoc procedureID *procedureTimingList* 
                                :test #'equal)))
    (if (null procedureRecord)
        (guessGeneralizedDuration procedureID)
      (append (avgAndSD (rest procedureRecord)) '(exact-match))
      ;;(computeAverage 0 0 (rest procedureRecord))
)))

(defun isProcRecord (procName procedureRecord)
  (equal procName (first (first procedureRecord))))

(defun guessGeneralizedDuration (procID)
  (let ((records 
         (remove-if-not #'(lambda (x) (isProcRecord (first procID) x)) 
                        *procedureTimingList*)))
    (if (null records)
        (list -1 -1 0 'no-match)
      (loop for r in records
          collect (first (rest r)) into timingList
             ;;finally (format t "~%~%timingList: ~a~%" timingList)))))
          finally (return 
                    (append (avgAndSD timingList) '(generalized-match)))))))

(defun scaleMostRecentDuration (procID scale)
  (let* ((procedureRecord (assoc procID *procedureTimingList* :test #'equal))
         (timeToChange (nth (1- (length procedureRecord)) procedureRecord)))
    (if (null procedureRecord)
        nil
      (setf (nth (1- (length procedureRecord)) procedureRecord) 
        (list (first timeToChange) (+ (* (- (second timeToChange) 
                                            (first timeToChange)) scale) 
                                      (first timeToChange))))
      (setf *procedureTimingList* 
        (append (remove procedureRecord *procedureTimingList*) 
                (append (remove timeToChange procedureRecord) 
                        (list (first timeToChange))))))))
 