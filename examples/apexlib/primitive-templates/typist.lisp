;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/apexlib/primitive-templates/typist.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: typist.lisp,v 1.7 2006/01/15 03:42:53 dalal Exp $

;; Modified 3aug02 by Mike Matessa and Bonnie John We changed the
;;    hold-resource to be inside a hold-resource-step That step uses the
;;    resource that it holds. This solved a problem that we had with the
;;    CAD model, where contention for the left-hand-block caused the
;;    template to lose ownership of all resources, which released the
;;    typing-cognition-block, which allowed activities from later
;;    templates to sneak in.  Our solution removed the contention, and
;;    hence made the CAD model run, but we still need to think about
;;    whether the asa should remove resources as it is doing or if it
;;    should do it some other way that does not cause the problem if
;;    other unanticipated contentions arise.
;; The hold-resource-steps are at the top of this file.  It is likely
;;    that all templates should use them but we will decide about that
;;    after the Cognitive Science 2002 Tutorial.
;;
;; modified 5/10/02 (eyk)


;;; -- wf. 12 January 2005.
;;;     Version 2.4.x of Apex incorrectly converted these sequential
;;;     procedures to put in a 'waitfor' on the 'hold-resource' step
;;;     of these procedures. That is to say, it was not defined what it
;;;     would mean to have a PROFILE step in :sequential procedures
;;;     and Apex didn't signal an error. Rewriting these to have the
;;;     a 'waitfor' that can't complete seems to solve the problems with
;;;     the applications, but it might not be what is wanted.
;;; 

;;;(procedure :sequential
;;;  (index (hold-resource-step right-hand-block))
;;;  (profile right-hand-block)
;;;  (hold-resource right-hand-block :ancestor 2))
;;;
;;;(procedure :sequential
;;;  (index (hold-resource-step left-hand-block))
;;;  (profile left-hand-block)
;;;  (hold-resource left-hand-block :ancestor 2))
;;;
;;;(procedure :sequential
;;;  (index (hold-resource-step type-string-block))
;;;  (profile type-string-block)
;;;  (hold-resource type-string-block :ancestor 2))
;;;
;;;(procedure :sequential
;;;  (index (hold-resource-step typing-cognition-block))
;;;  (profile typing-cognition-block)
;;;  (hold-resource typing-cognition-block :ancestor 2))


(procedure ;; :sequential
  (index (hold-resource-step right-hand-block))
  (profile right-hand-block)
  (step s1 (hold-resource right-hand-block :ancestor 2)) ;;  (waitfor ?ever))
  (step term (terminate) (waitfor ?s1)))

(procedure ;; :sequential
  (index (hold-resource-step left-hand-block))
  (profile left-hand-block)
  (step s1 (hold-resource left-hand-block :ancestor 2)) ;;   (waitfor ?ever))
  (step term (terminate) (waitfor ?s1)))
  
(procedure ;; :sequential
  (index (hold-resource-step type-string-block))
  (profile type-string-block)
  (step s1 (hold-resource type-string-block :ancestor 2)) ;; (waitfor ?ever))
  (step term (terminate) (waitfor ?s1)))

(procedure ;;:sequential
  (index (hold-resource-step typing-cognition-block))
  (profile typing-cognition-block)
  (step s1 (hold-resource typing-cognition-block :ancestor 2)) ;;  (waitfor ?ever))
  (step term (terminate) (waitfor ?s1)))



;;;;;;;;;;; GLOBAL CONSTANTS ;;;;;;;;
;; COGNITION PRIMITIVES
(setf *get-chunk* (list 'get-chunk))
(setf *initiate-keystroke* (list 'initiate-keystroke))

;; LEFT-HAND PRIMITIVES
(setf *type-key-left*  (list 'type-key-left))


;; RIGHT-HAND PRIMITIVES
(setf *type-key-right*  (list 'type-key-right))

;; WORLD PRIMITIVE
(setf *world* (list 'world))

;;;;;;;;;;; GLOBAL VARIABLES ;;;;;;;;
(setf *LAST-CHAR-HAND-GENSYM* nil)     ;; Unique symbol of previous type-string template created
                                        ;; (if it exists).  This is to keep track of a world event that
                                        ;; is generated at the end of every type-string template created.
                                        ;; If the current type-string template has a same-hand constraint
                                        ;; it needs to wait for the last motor activity of the previous
                                        ;; type-string template to begin the first initiate-keystroke
                                        ;; activity of the current template being generated.
(setf *LAST-CHAR-HAND* nil)            ;; Holds the hand that typed the last character of the
                                        ;; previous template.


;;;;;;;;;; TYPIST CODE ;;;;;;;;;;;;;;
;; TYPE-STRING
;; args: ?str
;; description: constructs one pdl template for a given string.
;;              the pdl template is given a unique identifier.
;;              after the pdl template is constructed, the pdl template is called.
;; return value: nothing
;(procedure :sequential
;   (index (type-string ?str))
;   (construct-typing-pdl ?str => ?pdl-name)
;   (TYPE-STRING-CREATED ?pdl-name)
;)

(procedure
   (index (type-string ?str))
   (step s1 (?this-step)
  	   (select ?this-step
  		   (depends-on '*R-hand-on*
  		               '((mouse home-to-keyboard)
  		       	         (keyboard no-op))))
  	   (rank 1))
   (step s2 (type-string-R-hand-on-keyboard ?str)
	   (rank 2))
  (step t (terminate) (waitfor ?s1 ?s2))
)


(procedure :sequential
    (index (type-string-R-hand-on-keyboard ?str))
    (construct-typing-pdl ?str => ?pdl-name)
    (TYPE-STRING-CREATED ?pdl-name)
)


(procedure :sequential
    (index (home-to-keyboard-before-typing))
    (hold-resource-step right-hand-block)
    (initiate-home-to keyboard)
    (right-hand-home-to keyboard)
    (WORLD 0 *R-hand-on* keyboard))




;; CONSTRUCT-TYPING-PDL
;; args: ?str
;; description: generates a new pdl template to type the string ?str.
;;              a unique id is given to the template.
;;              sets the hand of the last chararacter typed in ?str and unique id.
;; return value: the unique id for the template
;;;(procedure :special
;;;    (index (construct-typing-pdl ?str))
;;;    (let
;;;      ((func-name (gensym)))  ;;create the new procedure name
;;;      (eval (build-pdl ?str func-name))
;;;      (add-newest-proc)
;;;      (setf *LAST-CHAR-HAND-GENSYM* (read-from-string (string func-name)))
;;;      (setf *LAST-CHAR-HAND* (choose-hand (car (last (string-to-list ?str)))))
;;;      func-name
;;;    )
;;;)

(primitive
    (index (construct-typing-pdl ?str))
    (return (let
		((func-name (gensym)))  ;;create the new procedure name
	      (eval (build-pdl ?str func-name))
	      (add-newest-proc)
	      (setf *LAST-CHAR-HAND-GENSYM* (read-from-string (string func-name)))
	      (setf *LAST-CHAR-HAND* (choose-hand (car (last (string-to-list ?str)))))
	      func-name
	      )
	    ))

;; ADD-NEWEST-PROC (mfreed)
;; args: none
;; description: Call this after (eval (build-pdl ?str func-name)) in the procedure type-string.
;; return value: don't know
;; notes: This function is a hack, to be obsoleted after revising proc management mechanisms.

(defun add-newest-proc ()
  (add-procedure (first *proclib*) *agent*))

;; BUILD-PDL
;; args: str func-name
;; description: returns a list representing the PDL procedure to type str
;; return value: a list representing the PDL template to type the string str
;; author: eyk
(defun build-pdl (str func-name)
   (let* ((chunks (chop-up str)))
     (cons 'procedure
           (cons (create-type-string-index-clause func-name)
                 (append (build-iter chunks 1 nil chunks)
                         (append (list (create-step-clause 'w
                                                           (append *world*
                                                                   (list 0)
 
(list 'LAST-CHAR-HAND-GENSYM)
 
(list (read-from-string (string func-name))))
 
(create-waitfor-clause
                                                            (list 
(create-motor-step-index-var
 
(length chunks)
                                                                   (+ 
(length (string-to-list
 
(car (last chunks)))) 1)))
                                                            )
                                                           ))
                                 (list (create-terminate-clause))
                                 )
                         )
                 )
           )
     )
   )


;; BUILD-ITER
;; args: lis
;;       ind
;;       wait-for
;;       orig-chunks
;; description: iterates through each chunk and calls build-pdl-chunk to create the pdl for each chunk
;; return value: the initiate-keystroke and type-key pdl for all the chunks of the type-string template
(defun build-iter (lis ind wait-for orig-chunks)
   (if (null lis)
     nil
     (append (build-pdl-chunk (car lis)
                              ind
                              wait-for
                              orig-chunks)
             (build-iter (cdr lis)
                         (+ ind 1)
                         (create-waitfor-clause
                          (list (create-cog-step-index-var
                                 ind
                                 (+ (length (string-to-list (car lis))) 1)
                                 )
                                )
                          )
                         orig-chunks)
             )
     )
   )


;; BUILD-PDL-CHUNK
;; args: str
;;       ind
;;       wait-for
;;       orig-str-chunks-lis
;; description: creates the pdl for one particular chunk
;; return value: the initiate-keystroke and type-key pdl for ONE chunk of the type-string template
(defun build-pdl-chunk (str ind wait-for orig-str-chunks-lis)
   (cons
    (create-step-clause (create-COG-STEP-INDEX ind 1)
                        (cons 'get-chunk
                              (list (read-from-string str)))
                        wait-for)
    (cons
     ;; hold virtual resource - typing-cognition-block
     (create-step-clause (create-hold-virtual-resource-step-index ind 1 0)
                         (list 'hold-resource-step 'typing-cognition-block)
                         (create-waitfor-clause (list 
(create-cog-step-index-var ind 1)))
                         )
     (create-initiate-keystroke-type-string-body (string-to-list str) 
ind orig-str-chunks-lis)
     )
    )
   )


;; create-INITIATE-KEYSTROKE-TYPE-STRING-BODY
;; args: orig-str-lis
;; description: takes a list of keys representing the string and returns the a list representing
;;              the initiate-keystroke-type-string pdl for the entire string
;; return value: a list representing the PDL code for initiate-keystroke and type-key steps
;;              for typing a chunk
(defun create-initiate-keystroke-type-string-body (orig-str-lis
                                                    step-index1
                                                    orig-str-chunks-lis)
   (do*
     (
      ;; var1 init1 update

      ;; variables with no update on each iteration
      (str-total-numchar (length orig-str-lis))
      (str-left-numchar (create-left-numchar orig-str-lis))
      (str-right-numchar (create-right-numchar orig-str-lis))
      (step-index2 2 (+ step-index2 1))


      (prev-hand nil (choose-hand cur-key))

      ;; variables with updates on each iteration
      (str-list orig-str-lis (rest str-list))
      (cur-key (first orig-str-lis) (first str-list))
      (cur-hand (choose-hand cur-key) (choose-hand cur-key))

      (str-left-numchar-cnt (+ 0 (inc-left-numchar-cnt cur-hand))
                            (+ str-left-numchar-cnt 
(inc-left-numchar-cnt cur-hand)))

      (str-right-numchar-cnt (+ 0 (inc-right-numchar-cnt cur-hand))
                             (+ str-right-numchar-cnt 
(inc-right-numchar-cnt cur-hand)))

      (result-pdl-list (append (create-type-key-pdl str-total-numchar
                                                    str-left-numchar
                                                    str-right-numchar
                                                    step-index1
                                                    step-index2
                                                    cur-hand
                                                    str-left-numchar-cnt
                                                    str-right-numchar-cnt
                                                    str-list
                                                    orig-str-chunks-lis)

                               (append (create-initiate-keystroke-pdl str-list
                                                                      cur-hand
                                                                      prev-hand
 
step-index1
 
step-index2
 
orig-str-chunks-lis)
                                       nil))

                       (append (create-type-key-pdl str-total-numchar
                                                    str-left-numchar
                                                    str-right-numchar
                                                    step-index1
                                                    step-index2
                                                    cur-hand
                                                    str-left-numchar-cnt
                                                    str-right-numchar-cnt
                                                    str-list
                                                    orig-str-chunks-lis)
                               (append (create-initiate-keystroke-pdl str-list
                                                                      cur-hand
                                                                      prev-hand
 
step-index1
 
step-index2
                                                       
orig-str-chunks-lis)
                                       result-pdl-list)))

      )

     ;; test action1 .. action-n
     ((equal 1 (length str-list))
      (reverse result-pdl-list)
      )
     ) ;;end do*
   )

;; create-INITIATE-KEYSTROKE-PDL
;; args: str-list cur-hand prev-hand step-index
;; description: takes 4 arguments.  str-list represents the list of keys that yet to have
;;              PDL generated.  cur-hand is the hand needed to type the current key.
;;              prev-hand is the hand needed to type the previous key (nil if first key).
;;              step index is the current step.
;; return value: a list representing each a single initiate-keystroke step
;; author: eyk
(defun create-initiate-keystroke-pdl (str-list
                                       cur-hand
                                       prev-hand
                                       step-index1
                                       step-index2
                                       orig-str-chunks-lis)
   (cond ((and (= step-index1 1)
               (= step-index2 2)
               (equal cur-hand *LAST-CHAR-HAND*)) ;; same-hand constraint across chunks BETWEEN templates for first initiate-keystroke cognitive operator
          (cond ((equal 1 (length str-list))
                 (list
                  (create-step-clause (create-cog-step-index 
step-index1 step-index2)
                                      (reverse (cons (first str-list) 
*initiate-keystroke*))
                                      (append (create-waitfor-clause
                                               (cons 
(create-cog-step-index-var step-index1
 
(- step-index2 1))
                                                     nil))
                                              (list (list
                                                     'variable
                                                     'LAST-CHAR-HAND-GENSYM
                                                     *LAST-CHAR-HAND-GENSYM*))
                                              )
                                      )
                  ;; release virtual resource - typing-cognition-block
                  (create-step-clause 
(create-release-virtual-resource-step-index step-index1 step-index2 0)
                                      (list 'release-resource 
'typing-cognition-block ':ancestor 1)
                                      (create-waitfor-clause
                                       (list 
(create-cog-step-index-var step-index1 step-index2)))
                                      )
                  )
                 )
                (t
                 (list
                 (create-step-clause (create-cog-step-index 
step-index1 step-index2)
                                     (reverse (cons (first str-list) 
*initiate-keystroke*))
                                     (append (create-waitfor-clause
                                              (cons 
(create-cog-step-index-var step-index1
 
(- step-index2 1)) nil))
                                             (list (list
                                                    'variable
                                                    'LAST-CHAR-HAND-GENSYM
                                                    *LAST-CHAR-HAND-GENSYM*))
                                             )
                                     )
                 )
                 )
                )
          )
         ((and (> step-index1 1)
               (= step-index2 2)
               (equal cur-hand (choose-hand
                                (nth (- (length
                                         (string-to-list
                                          (nth (- step-index1 2) 
orig-str-chunks-lis))) 1)
                                     (string-to-list (nth (- 
step-index1 2) orig-str-chunks-lis)))
                                ))
               )
          ;; same-hand constraint across chunks WITHIN templates
          ;; for first initiate-keystroke cognitive operator
          (list
           (create-step-clause (create-cog-step-index step-index1 step-index2)
                               (reverse (cons (first str-list) 
*initiate-keystroke*))
                               (create-waitfor-clause (cons 
(create-cog-step-index-var
 
step-index1 (- step-index2 1))
                                                            (cons 
(create-motor-step-index-var
                                                                   (- 
step-index1 1)
                                                                   (+ (length
 
(string-to-list
                                                                        (nth
 
(- step-index1 2)
 
orig-str-chunks-lis))) 1))
                                                                  nil))))
           )
          )

         ((equal 1 (length str-list))  ;; last character
          (cond ((equal cur-hand prev-hand)
                 (list
                  (create-step-clause (create-cog-step-index 
step-index1 step-index2)
                                      (reverse (cons (first str-list) 
*initiate-keystroke*))
                                      (create-waitfor-clause (cons 
(create-cog-step-index-var
 
step-index1 (- step-index2 1))
 
(cons (create-motor-step-index-var
 
step-index1
 
(- step-index2 1)) nil))))
                  ;; release virtual resource - typing-cognition-block
                  (create-step-clause 
(create-release-virtual-resource-step-index step-index1 step-index2 0)
                                      (list 'release-resource 
'typing-cognition-block ':ancestor 1)
                                      (create-waitfor-clause
                                       (list 
(create-cog-step-index-var step-index1 step-index2)))
                                      )
                  )
                 )
                (t
                 (list
                  (create-step-clause (create-cog-step-index 
step-index1 step-index2)
                                      (reverse (cons (first str-list) 
*initiate-keystroke*))
                                      (create-waitfor-clause (cons 
(create-cog-step-index-var
 
step-index1 (- step-index2 1))
                                                                   nil)))
                  ;; release virtual resource - typing-cognition-block
                  (create-step-clause 
(create-release-virtual-resource-step-index step-index1 step-index2 
0)
                                      (list 'release-resource 
'typing-cognition-block ':ancestor 1)
                                      (create-waitfor-clause
                                       (list 
(create-cog-step-index-var step-index1 step-index2)))
                                      )
                  )
                 )
                )
          )
         (t ;; not last character
          (cond ((equal cur-hand prev-hand)
                 (list
                 (create-step-clause (create-cog-step-index 
step-index1 step-index2)
                                     (reverse (cons (first str-list) 
*initiate-keystroke*))
                                     (create-waitfor-clause (cons 
(create-cog-step-index-var
 
step-index1 (- step-index2 1))
                                                                  (cons
                                               
(create-motor-step-index-var
 
step-index1 (- step-index2 1))
                                                                   nil))))
                 )
                 )
                (t
                 (list
                  (create-step-clause (create-cog-step-index 
step-index1 step-index2)
                                      (reverse (cons (first str-list) 
*initiate-keystroke*))
                                      (create-waitfor-clause (cons 
(create-cog-step-index-var
                                                                    step-index1
                                                                    (- 
step-index2 1))
                                                                   nil)))
                  )
                 )
                )
          )
         )
   )

;; create-TYPE-KEY-PDL
;; args: str-total-numchar
;;       str-left-numchar
;;       str-right-numchar
;;       step-index
;;       cur-hand
;;       str-left-numchar-cnt
;;       str-right-numchar-cnt
;;       str-list

;; description: takes 8 arguments.  str-total-numchar is the total
;;              number of characters in the string.  str-left-numchar is
;;              the total number of characters typed with the left-hand.
;;              str-right-numchar is the total number of characters
;;              typed with the right-hand.  step-index is the number for
;;              the current step to be generated.  cur-hand is the hand
;;              (left or right) used to type the current character.
;;              str-left-numchar-cnt is a counter to determine how many
;;              characters have been typed with the left hand.
;;              str-right-numchar-cnt is a coutner to determine how many
;;              characters have been typed with the right hand.
;;              str-list is the list representation of the string to be
;;              typed.
;; notes: this is a helper function called by
;; create-initiate-keystroke-type-string-body author: eyk

(defun create-type-key-pdl (str-total-numchar
                             str-left-numchar
                             str-right-numchar
                             step-index1 step-index2
                             cur-hand
                             str-left-numchar-cnt
                             str-right-numchar-cnt
                             str-list
                             orig-str-chunks-lis)
  (let* ((vr-index 0))
    (cond ((equal str-total-numchar (length str-list)) ;first motor operator
           (cond ((> step-index1 1)  ;; the current chunk is 2..n chunk
                  (cond ((equal 'right cur-hand)
                         (cond ((equal str-right-numchar str-right-numchar-cnt)
                                (list
                                 (create-step-clause
                                  (create-motor-step-index step-index1
                                                           step-index2)
                                  (reverse (cons (first str-list)
                                                 *type-key-right*))
                                  (create-waitfor-clause (cons
                                                          (create-cog-step-index-var
                                                           step-index1
                                                           step-index2)
                                                          (cons 
                                                           (create-motor-step-index-var
                                                            (- step-index1 1)
                                                            (+ (length
                                                                (string-to-list
                                                                 (nth
                                                                  (- step-index1 2)
                                                                  orig-str-chunks-lis)))
                                                               1))
                                                           nil))))
                                 (create-step-clause
                                  (create-hold-virtual-resource-step-index step-index1 step-index2 (+ vr-index 1))
                                  (list 'hold-resource-step 'right-hand-block)
                                  (create-waitfor-clause
                                   (list (create-cog-step-index-var 
                                          step-index1 step-index2))) ; bonnie told me to!
                                  )
                                 (create-step-clause
                                  (create-hold-virtual-resource-step-index step-index1 step-index2 (+ vr-index 2))
                                  (list 'hold-resource-step 'type-string-block)
                                  (create-waitfor-clause
                                   (list (create-cog-step-index-var step-index1 step-index2)))
                                 )
                                 (create-step-clause
                                  (create-release-virtual-resource-step-index step-index1 step-index2 
                                                                              (+ vr-index 3))
                                  (list 'release-resource 
                                        'right-hand-block ':ancestor 1)
                                  (create-waitfor-clause
                                   (list (create-motor-step-index-var 
                                          step-index1 step-index2)))
                                  )
                                 
                                 )
                                
                                )
                              (t
                               (list

                                (create-step-clause
                                 (create-motor-step-index step-index1 
step-index2)
                                 (reverse (cons (first str-list) 
*type-key-right*))
                                 (create-waitfor-clause (cons
 
(create-cog-step-index-var
                                                          step-index1
                                                          step-index2)
                                                         (cons 
(create-motor-step-index-var
                                                                (- 
step-index1 1)
                                                                (+ (length
 
(string-to-list
                                                                     (nth
 
(- step-index1 2)
 
orig-str-chunks-lis)))
                                                                   1))
                                                               nil))))

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 1))
                                 (list 'hold-resource-step 'right-hand-block)
                                 (create-waitfor-clause
                                  (list (create-motor-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 2))
                                 (list 'hold-resource-step 'type-string-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )
                                )
                               )))
                       ((equal 'left cur-hand)
                        (cond ((equal str-left-numchar str-left-numchar-cnt)
                               (list
                                (create-step-clause
                                 (create-motor-step-index step-index1 
step-index2)
                                 (reverse (cons (first str-list) 
*type-key-left*))
                                 (create-waitfor-clause (cons
 
(create-cog-step-index-var
                                                          step-index1
                                                          step-index2)
                                                         (cons 
(create-motor-step-index-var
                                                                (- 
step-index1 1)
                                                                (+ (length
 
(string-to-list
                                                                     (nth
 
(- step-index1 2)
 
orig-str-chunks-lis)))
                                                                   1))
                                                               nil))))
                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 1))
                                 (list 'hold-resource-step 'left-hand-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 2))
                                 (list 'hold-resource-step 'type-string-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-release-virtual-resource-step-index step-index1 step-index2 
(+ vr-index 3))
                                 (list 'release-resource 
'left-hand-block ':ancestor 1)
                                 (create-waitfor-clause
                                  (list (create-motor-step-index-var 
step-index1 step-index2)))
                                 )
                                )
                               )
                              (t
                               (list
                                (create-step-clause
                                 (create-motor-step-index step-index1 
step-index2)
                                 (reverse (cons (first str-list) 
*type-key-left*))
                                 (create-waitfor-clause (cons
 
(create-cog-step-index-var
                                                          step-index1
                                                          step-index2)
                                                         (cons 
(create-motor-step-index-var
                                                                (- 
step-index1 1)
                                                                (+ (length
 
(string-to-list
 
(nth (- step-index1 2)
 
orig-str-chunks-lis)))
                                                                   1))
                                                               nil))))
                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 1))
                                 (list 'hold-resource-step 'left-hand-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 2))
                                 (list 'hold-resource-step 'type-string-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )
                                )
                               )
                              ))
                       )
                 )
                (t  ;; the current chunk is the first chunk
                 (cond ((equal 'right cur-hand)
                        (cond ((equal str-right-numchar str-right-numchar-cnt)

                               (list

                                (create-step-clause
                                 (create-motor-step-index step-index1 
step-index2)
                                 (reverse (cons (first str-list) 
*type-key-right*))
                                 (create-waitfor-clause
                                  (cons (create-cog-step-index-var 
step-index1 step-index2)
                                        nil)))

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 1))
                                 (list 'hold-resource-step 'right-hand-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 2))
                                 (list 'hold-resource-step 'type-string-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-release-virtual-resource-step-index step-index1 step-index2 
(+ vr-index 3))
                                 (list 'release-resource 
'right-hand-block ':ancestor 1)
                                 (create-waitfor-clause
                                  (list (create-motor-step-index-var 
step-index1 step-index2)))
                                 )


                                )



                               )
                              (t
                               (list
                                (create-step-clause
                                 (create-motor-step-index step-index1 
step-index2)
                                 (reverse (cons (first str-list) 
*type-key-right*))
                                 (create-waitfor-clause
                                  (cons (create-cog-step-index-var 
step-index1 step-index2)
                                        nil)))
                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 1))
                                 (list 'hold-resource-step 'right-hand-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 2))
                                 (list 'hold-resource-step 'type-string-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )
                                )

                               )))

                       ((equal 'left cur-hand)
                        (cond ((equal str-left-numchar str-left-numchar-cnt)
                               (list

                               (create-step-clause
                                (create-motor-step-index step-index1 
step-index2)
                                (reverse (cons (first str-list) 
*type-key-left*))
                                (create-waitfor-clause
                                 (cons (create-cog-step-index-var 
step-index1 step-index2)
                                       nil)))
                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 1))
                                 (list 'hold-resource-step 'left-hand-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 2))
                                 (list 'hold-resource-step 'type-string-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                                (create-step-clause
 
(create-release-virtual-resource-step-index step-index1 step-index2 
(+ vr-index 3))
                                 (list 'release-resource 
'left-hand-block ':ancestor 1)
                                 (create-waitfor-clause
                                  (list (create-motor-step-index-var 
step-index1 step-index2)))
                                 )
                               )
                               )
                              (t
                               (list
                               (create-step-clause
                                (create-motor-step-index step-index1 
step-index2)
                                (reverse (cons (first str-list) 
*type-key-left*))
                                (create-waitfor-clause
                                 (cons (create-cog-step-index-var 
step-index1 step-index2)
                                       nil)))
                               (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 1))
                                 (list 'hold-resource-step 'left-hand-block)
                                 (create-waitfor-clause
                                  (list (create-cog-step-index-var 
step-index1 step-index2)))
                                 )

                               (create-step-clause
 
(create-hold-virtual-resource-step-index step-index1 step-index2 (+ 
vr-index 2))
                                (list 'hold-resource-step 'type-string-block)
                                (create-waitfor-clause
                                 (list (create-cog-step-index-var 
step-index1 step-index2)))
                                )
                               )
                               )))
                       )
                 )
                )
          )
         ((equal 'right cur-hand)  ;right motor operator
          (cond ((equal 1 str-right-numchar-cnt)  ;;type-key-right-first
                 (list
                  (create-step-clause
                   (create-motor-step-index step-index1 step-index2)
                   (reverse (cons (first str-list)  *type-key-right*))
                   (create-waitfor-clause
                    (cons (create-cog-step-index-var step-index1 step-index2)
                          (cons (create-motor-step-index-var 
step-index1 (- step-index2 1)) nil))))
                  (create-step-clause
                   (create-hold-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 1))
                   (list 'hold-resource-step 'right-hand-block)
                   (create-waitfor-clause
                    (list (create-cog-step-index-var step-index1 step-index2)))
                   )

                  )
                 )
                ((equal str-total-numchar (+ str-right-numchar-cnt 
str-left-numchar-cnt))
                 (list
                  (create-step-clause
                   (create-motor-step-index step-index1 step-index2)
                   (reverse (cons (first str-list) *type-key-right*))
                   (create-waitfor-clause (cons 
(create-cog-step-index-var step-index1 step-index2)
                                                (cons 
(create-motor-step-index-var step-index1
 
(- step-index2 1))
                                                      nil))))

                  (create-step-clause
                   (create-release-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 1))
                   (list 'release-resource 'right-hand-block ':ancestor 1)
                   (create-waitfor-clause
                    (list (create-motor-step-index-var step-index1 
step-index2)))
                   )

                  (create-step-clause
                   (create-release-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 2))
                   (list 'release-resource 'type-string-block ':ancestor 1)
                   (create-waitfor-clause
                    (list (create-cog-step-index-var step-index1 step-index2)))
                   )


                   )
                  )

                ((equal str-right-numchar str-right-numchar-cnt)
                 (list
                  (create-step-clause
                   (create-motor-step-index step-index1 step-index2)
                   (reverse (cons (first str-list)  *type-key-right*))
                   (create-waitfor-clause (cons 
(create-cog-step-index-var step-index1 step-index2)
                                                (cons 
(create-motor-step-index-var step-index1
 
(- step-index2 1))
                                                      nil))))
                  (create-step-clause
                   (create-release-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 1))
                   (list 'release-resource 'right-hand-block ':ancestor 1)
                   (create-waitfor-clause
                    (list (create-motor-step-index-var step-index1 
step-index2)))

                   )
                  )
                 )
                (t
                 (list
                  (create-step-clause
                   (create-motor-step-index step-index1 step-index2)
                   (reverse (cons (first str-list)  *type-key-right*))
                   (create-waitfor-clause (cons 
(create-cog-step-index-var step-index1 step-index2)
                                                (cons 
(create-motor-step-index-var step-index1
 
(- step-index2 1))
                                                      nil))))
                  )
                 )
                )
          )
         ((equal 'left cur-hand)
          (cond ((equal 1 str-left-numchar-cnt)  ;;type-key-right-first
                 (list
                 (create-step-clause
                  (create-motor-step-index step-index1 step-index2)
                  (reverse (cons (first str-list)  *type-key-left*))
                  (create-waitfor-clause (cons 
(create-cog-step-index-var step-index1 step-index2)
                                               (cons 
(create-motor-step-index-var step-index1
 
(- step-index2 1))
                                                     nil))))
                  (create-step-clause
                   (create-hold-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 1))
                   (list 'hold-resource-step 'left-hand-block)
                   (create-waitfor-clause
                    (list (create-cog-step-index-var step-index1 step-index2)))
                   )
                 )
                 )
                ((equal str-total-numchar (+ str-right-numchar-cnt 
str-left-numchar-cnt))
                 (list
                 (create-step-clause
                  (create-motor-step-index step-index1 step-index2)
                  (reverse (cons (first str-list)  *type-key-left*))
                  (create-waitfor-clause (cons 
(create-cog-step-index-var step-index1 step-index2)
                                               (cons 
(create-motor-step-index-var step-index1
 
(- step-index2 1))
                                                     nil))))
                  (create-step-clause
                   (create-release-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 1))
                   (list 'release-resource 'left-hand-block ':ancestor 1)
                   (create-waitfor-clause
                    (list (create-motor-step-index-var step-index1 
step-index2)))
                   )

                  (create-step-clause
                   (create-release-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 2))
                   (list 'release-resource 'type-string-block ':ancestor 1)
                   (create-waitfor-clause
                    (list (create-cog-step-index-var step-index1 step-index2)))
                   )
                 )
                 )
                ((equal str-left-numchar str-left-numchar-cnt)
                 (list
                 (create-step-clause
                  (create-motor-step-index step-index1 step-index2)
                  (reverse (cons (first str-list)  *type-key-left*))
                  (create-waitfor-clause (cons 
(create-cog-step-index-var step-index1 step-index2)
                                               (cons 
(create-motor-step-index-var step-index1
 
(- step-index2 1))
                                                     nil))))
                  (create-step-clause
                   (create-release-virtual-resource-step-index 
step-index1 step-index2 (+ vr-index 1))
                   (list 'release-resource 'left-hand-block ':ancestor 1)
                   (create-waitfor-clause
                    (list (create-motor-step-index-var step-index1 
step-index2)))

                   )
                 )
                 )
                (t
(list
                 (create-step-clause
                  (create-motor-step-index step-index1 step-index2)
                  (reverse (cons (first str-list)  *type-key-left*))
                  (create-waitfor-clause (cons 
(create-cog-step-index-var step-index1 step-index2)
                                               (cons 
(create-motor-step-index-var step-index1
 
(- step-index2 1))
                                                     nil))))
                 )
                 )
                )
          )
         (nil
          (print "error! key not found!"))  ;;error! key not found!
         
   )
   )

) ;; kmd added this; it might be wrong


;;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;;;


;; CHOOSE-HAND
;; args: key
;; description: Accepts a key in the form <key>-key and returns the hand that types it
;; return value: Hand that typed the key
;; notes: This is the knowledge of which hand types which key
;;        that a touch-typist has "at her fingertips"
;;        This means we are not actually using the keyboard at all
;;        that may not be stupid for a touch typist (bej)
(defun choose-hand (key)
   (cond ((equal key nil) nil)
         ((member key '(esc-key
                        f1-key
                        f2-key
                        f3-key
                        f4-key
                        f5-key

                        tilda-key
                        excl-key
                        at-key
                        hash-key
                        quest-key
                        perc-key
                        carrot-key

                        bquote-key
                        1-key
                        2-key
                        3-key
                        4-key
                        5-key
                        6-key

                        tab-key
                        q-key
                        w-key
                        e-key
                        r-key
                        t-key

                        caps-key
                        a-key
                        s-key
                        d-key
                        f-key
                        g-key

                        shft-key
                        z-key
                        x-key
                        c-key
                        v-key
                        b-key

                        ctrl-key
                        win-key
                        alt-key))
          'left)
         ((member key '(f6-key
                        f7-key
                        f8-key
                        f9-key
                        f10-key
                        f11-key
                        f12-key

                        and-key
                        aster-key
                        left-paren-key
                        right-paren-key
                        dash-key
                        equal-key
                        plus-key

                        7-key
                        8-key
                        9-key
                        0-key
                        hyphen-key
                        equal-key
                        bslash-key

                        y-key
                        u-key
                        i-key
                        o-key
                        p-key
                        [-key
                        ]-key

                        h-key
                        j-key
                        k-key
                        l-key
                        semi-key
                        quote-key
                        enter-key

                        n-key
                        m-key
                        comma-key
                        period-key
                        slash-key
                        shift-key

                        space-key

                        alt-key
                        win-key
                        select-key
                        ctrl-key

                        colon-key
                        double-quote-key
                        left-bracket-key
                        right-bracket-key
                        less-key
                        greater-key
                        question-key

                        backslash-key))
          'right)
         (t "not a key we know")
         )
)

;; CHOP-UP (mmatessa)
;; args: str
;; description: parses str into separate strings in a list, each string represents a "chunk"
;; return value: a list representing the string parsed into chunks
(defun chop-up (str)
    (let* ((pos (or (position #\space str)
                    (position #\return str)))
           (end (if pos
                   (position-if-not #'(lambda (x) (or (equal x #\space)
                                                      (equal x #\return)))
                                    str :start pos))))
      (cond ((or (null pos)
                 (null end))
             (list str))
            (t
             (cons (subseq str 0 end)
                   (chop-up (subseq str end (length str))))))))



;; STR2LIS
;; args: x
;; description: accepts a string and returns a list representing that string
;;              e.g. (str2lis "elaine") => (E L A I N E)
;; author: Mike Matessa
(defun str2lis (x)
   (let ((ans nil)
         (sub nil))
     (dotimes (i (length x))
       (setf sub (subseq x i (1+ i)))
       (push sub ans))
     (reverse ans)))

;; STRING-TO-LIST
;; args: x
;; description: This converts a string to a list of characters with "-key" appended to each
;;              character.
;;              e.g. (string-to-list "hello") => (H-KEY L-KEY A-KEY I-KEY N-KEY E-KEY)
;; author: eyk
(defun string-to-list (x)
   (append-all (mapcar 'char2key (str2lis x)))
)

(defun append-all (lis)
   (if (null lis)
     nil
     (append (car lis)
             (append-all (cdr lis)))))

;; CHAR2KEY
;; args: x
;; description: This adds "-key" to each charcter in the list
;; author:
(defun char2key (x)
   (if (or (and (char>= (char x 0) #\A)
                (char<= (char x 0) #\Z))

           (char= (char x 0) #\~)
           (char= (char x 0) #\!)
           (char= (char x 0) #\@)
           (char= (char x 0) #\#)
           (char= (char x 0) #\$)
           (char= (char x 0) #\%)
           (char= (char x 0) #\^)
           (char= (char x 0) #\&)
           (char= (char x 0) #\*)
           (char= (char x 0) #\()
           (char= (char x 0) #\))
           (char= (char x 0) #\_)
           (char= (char x 0) #\+)
           (char= (char x 0) #\{)
           (char= (char x 0) #\})
           ;;(char= (char x 0) #\|)  ;;errors occur on "|"
           (char= (char x 0) #\:)
           (char= (char x 0) #\")
           (char= (char x 0) #\<)
           (char= (char x 0) #\>)
           (char= (char x 0) #\?)
           (char= (char x 0) #\\))

     (cons 'shft-key (list (to-dash-key x)))
     (list (to-dash-key x))
     )
   )

;; TO-DASH-KEY
;; args: x
;; description: accepts a character and returns the equivalent id for the key
(defun to-dash-key (x)
   (cond ((equal x " ")
          'space-key)
         ((equal x "
")
          'enter-key)
         ((equal x "`")
          'bquote-key)
         ((equal x ",")
          'comma-key)
         ((equal x ".")
          'period-key)
         ((equal x "=")
          'equal-key)
         ((equal x "-")
          'hyphen-key)

         ((equal x "/")
          'slash-key)
         ((equal x ";")
          'semi-key)
         ((equal x "'")
          'quote-key)

         ;;shifted characters
         ((equal x "~")
          'tilda-key)
         ((equal x "!")
          'excl-key)
         ((equal x "@")
          'at-key)
         ((equal x "#")
          'hash-key)
         ((equal x "$")
          'quest-key)
         ((equal x "%")
          'perc-key)
         ((equal x "^")
          'carrot-key)
         ((equal x "&")
          'and-key)
         ((equal x "*")
          'aster-key)
         ((equal x "(")
          'left-paren-key)
         ((equal x ")")
          'right-paren-key)
         ((equal x "_")
          'dash-key)
         ((equal x "+")
          'plus-key)

         ((equal x "}")
          'right-bracket-key)
         ((equal x "{")
          'left-bracket-key)

         ((equal x ":")
          'colon-key)

         ((equal x "<")
          'less-key)
         ((equal x ">")
          'greater-key)
         ((equal x "?")
          'question-key)

         ((equal x "\"")
          'double-quote-key)

         ((equal x "\\")
          'backslash-key)

         ((string< x "0")
          'char-key)
         (t
          (read-from-string (format nil "~a-key" x)))))



;; INC-LEFT-NUMCHAR-CNT
;; args: cur-hand
;; description: accepts one argument cur-hand (left or right) to determine whether to increment
;; returns: updated value
;; notes: this is a helper function called by create-initiate-keystroke-type-string-body
;; author: eyk
(defun inc-left-numchar-cnt (cur-hand)
     (if (equal cur-hand 'left) 1 0)

)

;; INC-RIGHT-NUMCHAR-CNT
;; args: cur-hand
;; description: accepts one argument cur-hand (left or right) to determine whether to increment
;; returns: updated value
;; notes: this is a helper function called by create-initiate-keystroke-type-string-body
;; author: eyk
(defun inc-right-numchar-cnt (cur-hand)
     (if (equal cur-hand 'right) 1 0)
)


;; INTEGER-TO-STRING
;; args: x
;; description: converts a digit x to a string
;; return value: "x"
(defun integer-to-string (x)
   (prin1-to-string x))

;; create-LEFT-NUMCHAR args: lis description: returns the number of
;; characters typed with the left hand in a string the argument lis is
;; in the form (h-key e-key l-key l-key o-key) for the string "hello"
;; author: eyk

(defun create-left-numchar (lis)
   (cond ((equal nil lis)
          0)
         ((equal 'left (choose-hand (first lis)))
          (+ 1 (create-left-numchar (rest lis))))
         (t
          (+ 0 (create-left-numchar (rest lis)))))
)

;; create-RIGHT-NUMCHAR
;; args: lis
;; description: returns the number of characters typed with the right
;;              hand in a string, the argument lis is in the form (h-key
;;              e-key l-key l-key o-key) for the string "hello"
;; author: eyk
(defun create-right-numchar (lis)
   (cond ((equal nil lis)
          0)
         ((equal 'right (choose-hand (first lis)))
          (+ 1 (create-right-numchar (rest lis))))
         (t
          (+ 0 (create-right-numchar (rest lis)))))
)


;;;;;;;;;;;;;;;;; functions to return step clauses for a given step, primitive, and waitfor condition ;;;;;;;;;;;;


;; create-TYPE-STRING-INDEX-CLAUSE args: string description: returns the
;; index clause for a given string to type return value: index clause
;; e.g. (create-type-string "elaine") => (index (type-string-elaine))
;; author: eyk
(defun create-type-string-index-clause (func-name)
   (list 'index (list 'TYPE-STRING-CREATED func-name)))

;; create-STEP-CLAUSE args: step-index atom, primitive list, waitfor
;; list description: returns the step clause for a given step-index
;; string, primitive list, and waitfor list return value: step clause
;; e.g. (create-step-clause 'c2 '(initiate-keystroke) '(waitfor ?c1)) =>
;; (step c2 (initiate-keystroke) (waitfor ?c1)) author: eyk notes: NEED
;; TO CHECK FOR NIL WAITFOR LISTS (taken care of)
(defun create-step-clause (step-index primitive-list waitfor-list)
   (cond ((equal waitfor-list nil)  ;; no waitfors
          (append (list 'step step-index) (list primitive-list)))
         (t
          (append (append (list 'step step-index) (list 
primitive-list)) (list waitfor-list)))))


;; create-TERMINATE-CLAUSE
;; args: x
;; description: returns the termination clause for a given waitfor step index x
;; return value: termination clause
;; author: eyk
(defun create-terminate-clause ()
   (list 'step 't '(terminate) '(waitfor ?w))
)




;;;;;;;;;;;;;;;;;;; functions to return waitfor clauses for step indexes ;;;;;;;;;;;;;;;;;

;; create-WAITFOR-CLAUSE args: list of step indexes description: returns
;; the waitfor clause for a list of waitfor for conditions (conjunctive)
;; return value: waitfor clause e.g (create-waitfor-clause '(?c3 ?m2))
;; => (waitfor ?c3 ?m2)
(defun create-waitfor-clause (lis)
   (cons 'waitfor lis)
)

;;;;;;;;;;;;;;;;;;; functions to return a step index (motor, cog, world)
;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;


;; create-MOTOR-STEP-INDEX-VAR
;; args: step index
;; description: returns the string value of a motor step for a given index
;; return value:  motor step e.g. (create-motor-step-index '5) => ?m5)
(defun create-motor-step-index-var (num-char1 num-char2)
   (read-from-string (concatenate 'string "?m" (integer-to-string 
num-char1) "-" (integer-to-string num-char2))))

;; create-COG-STEP-INDEX-VAR
;; args: step index
;; description: returns the string value of a cognitive step for a given index
;; return value:  cognitive step e.g. (create-cog-step-index-str '5) => ?c5)
(defun create-cog-step-index-var (num-char1 num-char2)
   (read-from-string (concatenate 'string "?c" (integer-to-string 
num-char1) "-" (integer-to-string num-char2))))

;; create-WORLD-STEP-INDEX-VAR
;; args: step index
;; description: returns the string value of a world step for a given index
;; return value:  world step e.g. (create-world-step-index-str '5) => ?w5)
(defun create-world-step-index-var (num-char1 num-char2)
   (read-from-string (concatenate 'string "?w" (integer-to-string 
num-char1) "-" (integer-to-string num-char2))))

;; create-MOTOR-STEP-INDEX
;; args: step index
;; description: returns the string value of a motor step for a given index
;; return value:  motor step e.g. (create-motor-step-index '5) => m5)
(defun create-motor-step-index (num-char1 num-char2)
   (read-from-string (concatenate 'string "m" (integer-to-string 
num-char1) "-" (integer-to-string num-char2))))

;; create-COG-STEP-INDEX
;; args: step index
;; description: returns the string value of a cognitive step for a given index
;; return value:  cognitive step e.g. (create-cog-step-index '5) => c5)
(defun create-cog-step-index (num-char1 num-char2)
   (read-from-string (concatenate 'string "c" (integer-to-string 
num-char1) "-" (integer-to-string num-char2))))

;; create-WORLD-STEP-INDEX
;; args: step index
;; description: returns the string value of a world step for a given index
;; return value:  world step e.g. (create-world-step-index-str '5) => w5)
(defun create-world-step-index (num-char1 num-char2)
   (read-from-string
    (concatenate 'string
                 "w"
                 (integer-to-string num-char1)
                 "-"
                 (integer-to-string num-char2))
    )
   )

;; create-HOLD-VIRTUAL-RESOURCE-STEP-INDEX
;; args: step index
;; description: returns the string value of a cognitive step for a given index
;; return value:  cognitive step e.g. (create-cog-step-index '5) => c5)
(defun create-hold-virtual-resource-step-index (num-char1 num-char2 num-char3)
   (read-from-string (concatenate 'string "hvr" (integer-to-string 
num-char1) "-" (integer-to-string num-char2) "-" (integer-to-string 
num-char3))))

;; create-RELEASE-VIRTUAL-RESOURCE-STEP-INDEX
;; args: step index
;; description: returns the string value of a cognitive step for a given index
;; return value:  cognitive step e.g. (create-cog-step-index '5) => c5)

(defun create-release-virtual-resource-step-index (num-char1 num-char2
                                                   num-char3)
  (read-from-string (concatenate 'string "rvr"
                                 (integer-to-string num-char1) "-" (integer-to-string num-char2) "-"
                                 (integer-to-string num-char3))))
