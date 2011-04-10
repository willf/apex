;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/goms/atm/cmn.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: cmn.lisp,v 1.4 2006/01/15 03:42:53 dalal Exp $

;;; Example application: Withdrawal of money from at ATM using the CMN
;;; approach.

;;; The CMN model is based on classic GOMS (Card, Moran and Newell,
;;; 1983).  It involves hierarchically decomposing a goal into
;;; operator-level goals, each representing a "primitive" action for
;;; manipulating an interface and each requiring a characteristic time
;;; interval to carry out.  The sum duration of all operator-level goals
;;; equals the predicted duration for the high-level task.

(in-package :user)

(defapplication "ATM-CMN World"
    :libraries ("atm")
    :files ("common.lisp")
    :init (initialize-atm-common))

;;; Top level goal, which is decomposed in the following procedures in
;;; a straightforward sequential way.  All agent activities are those of
;;; the right hand, and are modelled using the "dummy" activity because
;;; the only thing of interest is their duration.

(procedure :sequential
 (index (get ?amt from atm))
 (initiate ATM session)
 (withdraw ?amt from atm)
 (end ATM session)
 (end-trial))

(procedure :sequential
  (index (initiate ATM session))
  (insert card)
  (enter password))

(procedure :sequential
  (index (insert card))
  (use resource right-hand for (1021 ms)))

(procedure :sequential
  (index (enter password)) 
  ((enter 4) using right-hand for (626 ms))
  ((enter 9) using right-hand for (365 ms))
  ((enter 0) using right-hand for (420 ms))  
  ((enter 1) using right-hand for (469 ms))  
  ((enter ok) using right-hand for (548 ms)))

(procedure :sequential
  (index (withdraw ?amt from ATM))
  (choose withdraw)
  (choose account)
  (enter amount)
  (retrieve money))

(procedure :sequential
  (index (choose withdraw))
  (use resource right-hand for (667 ms)))

(procedure :sequential
  (index (choose account))
  (use resource right-hand for (372 ms)))

(procedure :sequential
 (index (enter amount))
 ((enter 8) using right-hand for (684 ms))
 ((enter 0*) using right-hand for (339 ms))
 ((enter correct) using right-hand for (960 ms)))

(procedure :sequential
  (index (retrieve money))
  (use resource right-hand for (908 ms)))

(procedure :sequential
  (index (end ATM session))
  (enter no)
  (retrieve card)
  (retrieve receipt))

(procedure :sequential
  (index (enter no))
  (use resource right-hand for (935 ms)))

(procedure :sequential
  (index (retrieve card))
  (use resource right-hand for (856 ms)))

(procedure :sequential
  (index (retrieve receipt))
  (use resource right-hand for (1048 ms)))
