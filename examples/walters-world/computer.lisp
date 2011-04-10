;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/computer.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: computer.lisp,v 1.2 2006/01/15 03:42:56 dalal Exp $

(in-package :user)

;;; --- Computer

(defclass computer (agent physob)
  ((power 
    :accessor power 
    :initarg 
    :power 
    :initform 'off)
   (user 
    :accessor user 
    :initarg 
    :user 
    :initform nil)
   (email 
    :accessor email 
    :initform nil)
   (number-for-game 
    :accessor number-for-game)          ;number Walter will have to guess
   (tries 
    :accessor tries)                    ;number of tries Walter has used up
   (shape 
    :accessor shape :
    initform '(computer))))

(in-apex-bundle :computer)

(procedure
 (index (be computer))
 (step s1 (computer-turned-on-by ?user) 
       (waitfor (turned-on-by ?user)))
 (step s2 (check-email) 
       (waitfor (want-to-check-email)))
 (step s3 (play-game) 
       (waitfor (want-to-play-game))))

(primitive
 (index (computer-turned-on-by ?user))
 (duration (100 ms)) 
 (on-start (setf (user +self+) ?user))
 (on-completion (inform `(computer-on) 
                        :router *walter-router* )))

(primitive
 (index (check-email))
 (duration (800 ms)) 
 (locals (email-list '(upcoming-fencing-tournament 
		       friend-coming-to-visit 
		       money-problems-getting-you-down?) ))
 (on-start (if (= (random 2) 0)
	       (setf (email +self+) (append (email +self+) 
                                            (list (first email-list)))))
	   (if (equal (weather-outside (find-instance `window-1)) 'windy)
	       (setf (email +self+) (append (email +self+) 
                                            (list (second email-list)))))
	   (if (equal (power (find-instance `lamp-1)) 'on)
	       (setf (email +self+) (append (email +self+) 
                                            (list (third email-list)))))
	   (if (null (email +self+))
	       (progn (inform `(no-new-email) 
                              :router *walter-router*)
		      (complete-the-activity (action +this-task+)))
	     (inform `(new-email) 
                     :router *walter-router*)))
 (update (100 ms)
	 (if (null (email +self+))
	     (inform `(no-more-email) 
                     :router *walter-router*)
	   (progn (inform (list (first (email +self+))) 
                          :router *walter-router*)
		  (setf (email +self+) (rest (email +self+)))))))

(procedure
 (index (play-game))
 (step s1 (set-up-game))
 (step s2 (get-human-move) (repeating ))
 (step st (terminate) 
       (waitfor (game-finished))))

(procedure
 (index (get-human-move))
 (step s1 (process-human-move ?x) 
       (waitfor (human-move ?x)))
 (step s2 (terminate) 
       (waitfor ?s1)))

(primitive
 (index (set-up-game))
 (duration (100 ms)) 
 (on-start
  (setf (number-for-game +self+) (random 10))
  (setf (tries +self+) 0))
 (on-completion
  (inform `(ready-for-move) 
          :router *walter-router*)))

(primitive
 (index (process-human-move ?x))
 (duration (2 ms))
 (on-completion
  (incf (tries +self+))
  (if (= (number-for-game +self+) ?x)
      (progn (inform `(human-wins) 
                     :router *walter-router*)
	     (inform `(game-finished)
                     :router *walter-router*))
    (if (>= (tries +self+) 3)
	(progn (inform `(human-loses) 
                       :router *walter-router*)
	       (inform `(game-finished)
                       :router *walter-router*))
      (inform `(ready-for-move) 
              :router *walter-router*)))))
