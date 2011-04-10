;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/hello.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: hello.lisp,v 1.15 2006/01/15 03:42:52 dalal Exp $

(in-package :user)

;;; This is a very simple and introductory Apex application that
;;; simulates answering of a ringing telephone.

(defapplication "Hello World"
    :init (hello-world))

;;; Jill is an agent, who knows how to answer the phone when it rings.
;;; These are her procedures.

(in-apex-bundle :jill)

(procedure :sequential (handle phone)
  (step (answer ?phone the phone)
        (waitfor (ringing ?phone))
        (on-start
         (inform '(answering the ?phone)))
        (on-end
         (inform '(answered the ?phone))))
  (step (end-application)))

(procedure :sequential (answer ?phone the phone)
  (profile right-hand)
  (step (pickup-phone ?phone))
  (step (say-hello)
        (on-start
         (inform '(saying hello into the ?phone)))
        (on-end
         (inform '(said hello into the ?phone)))))

(procedure :sequential (pickup-phone ?phone)
  (profile right-hand)
  (step (pick up ?phone with right-hand)))

(procedure :sequential (say-hello)
  (profile voice)
  (step (speak "Hello?")))

(primitive (pick up ?phone with ?hand)
 (profile ?hand)
 (duration p2s))

(primitive (speak ?utterance)
 (profile voice)
 (duration (500 ms))
 (on-completion
  (print ?utterance)
  (inform `(said ,?utterance) :author +self+)))


;;; The phone is an agent, which knows how ring after some time.

(in-apex-bundle :phone)

(primitive (ring sometime)
  (on-start
   (schedule 'p1.5s
     (inform `(ringing ,+self+) :author +self+))))


;;; Initialization function for this application.

(defun hello-world ()
  (let* ((locale (make-instance 'locale :name "Room"))
         (jill (make-instance 'agent :name "Jill"
                              :use-bundles '(:jill)
                              :initial-task '(handle phone)
                              :locale locale))
         (phone (make-instance 'agent :name "Phone"
                               :use-bundles '(:phone)
                               :initial-task '(ring sometime)
                               :locale locale)))

    ;; Trace settings
    (show state)
    (show saying)
    (show said)
    (show ringing)
    (show answering)
    (show answered)))
