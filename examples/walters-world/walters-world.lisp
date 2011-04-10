;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; apex/examples/walters-world/walters-world.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: walters-world.lisp,v 1.6 2006/01/15 03:42:57 dalal Exp $



(in-package :user)

(defapplication "Walter's World"   
    :init (initialize-world)
    :libraries ("human")
    :files ("tv.lisp" "plant.lisp" "phone.lisp"
            "dog.lisp" "cd-player.lisp" "computer.lisp"
            "cooking.lisp" "food.lisp" "egg.lisp" 
            "human1.lisp" "kitchen-pdl.lisp" "kitchen-definitions.lisp"
            "living-room-pdl.lisp" "living-room-definitions.lisp"
            "front-porch-pdl.lisp" "front-porch-definitions.lisp"
            "hand-custom.lisp" "timing.lisp" "random.lisp" ))

;;; ----- (i) Initializing a Apartmentworld simulation trial

;;; INITIALIZE is called at the beginning of a simulation trial to create
;;; the objects and activities to be simulated.  In apartmentworld, these
;;; include objects such as a kitchen, cook and egg, and activities these
;;; objects are performing such as seeing (the cook) and temp-equilibria-
;;; tion (all objects). The kitchen object is of a special type called a 
;;; locale that provides a 2-dimensional frame of reference for the 
;;; objects "in" it.  Most objects are intiialized with a pos(ition) 
;;; value that indicates where in the locale it lies.  Position units
;;;are in centimeters.

;;A router for communication between the cook and other agents 

(defparameter *walter-router* (make-instance 'ps-router))


(defun initialize-world ()
  (initialize-physob-database) ;;orginal syntax
  (reset *walter-router*) ;;original syntax
  
  (get-random-state)
  
  (let* 
      ((kitchen (make-instance 'locale 
                  :name 'kitchen))

       (living-room (make-instance 'locale
                      :name 'living-room))
       
       (front-porch (make-instance 'locale 
                      :name 'front-porch))
       
       (walter (make-instance 'human
               :name 'Walter 
               :location '(0 0 22)
               :initial-task '(do-household-tasks)
               :pos '(0 0) 
               :locale kitchen
               ))
       
       (tv (make-instance 'television
             :name "TV"
             :use-bundles '(:tv) 
             :initial-task '(be tv)
             :locale kitchen))
    
       (phone (make-instance 'telephone
                :name 'phone 
                :locale kitchen))
        
       (flower (make-instance 'flower 
                 :locale kitchen))
       
       (plant (make-instance 'plant 
                :name "Plant" 
                :locale kitchen 
                :use-bundles '(:plant) 
                :initial-task '(be plant)
                :flower flower))
                
       (vase (make-instance 'vase 
               :locale kitchen))
       
       (cd (make-instance 'cd-player 
             :name "cd-player"
             :use-bundles '(:CD)
             :initial-task '(be CD) 
             :locale living-room))

       (dog (make-instance 'dog 
              :name "Goldie" 
              :use-bundles '(:dog)
              :initial-task '(be dog) 
              :locale living-room))

       (dog-dish (make-instance 'food-dish 
                   :locale living-room))
       
       (computer (make-instance 'computer 
                   :name "computer" 
                   :locale living-room
                   :use-bundles '(:computer) 
                   :initial-task '(be computer)))
      
       (lamp (make-instance 'lamp 
               :locale living-room))
      
       (window (make-instance 'window 
                 :locale living-room))
       
       (porch-swing (make-instance 'porch-swing 
                      :locale front-porch))

       (counter (make-instance 'kitchen-counter
                  :dimensions '(60 62) 
                  :refpos '(-70 20)
                  :name 'counter 
                  :locale kitchen))
       
       (stovetop (make-instance 'stovetop 
                   :dimensions '(78 62)
                   :refpos '(-10 20)
                   :name 'stovetop 
                   :locale kitchen
                   :widgetspecs jennair20))
       
       (egg (make-instance 'egg 
              :pos '(-20 40) 
              :locale kitchen))
       
       (popcorn (make-instance 'popcorn 
                  :locale kitchen))
       
       (popcorn-wrapper (make-instance 'wrapper 
                          :shape '(popcorn-wrapper) 
                          :locale kitchen))
       
       (measuring-cup (make-instance 'measuring-cup 
                        :locale kitchen 
                        :full 150))
       
       (measuring-cup2 (make-instance 'measuring-cup 
                         :locale kitchen :full 100 
                         :shape '(empty-cup-2)))

       (pan (make-instance 'pan 
              :pos '(-45 40) 
              :locale kitchen))
       
       (watch (make-instance 'watch 
                :locale kitchen))
       
       (microwave (make-instance 'microwave 
                    :dimensions '(40 30) 
                    :pos '(-30 20) 
                    :locale kitchen))
       
       (macaroni (make-instance 'macaroni 
                   :pos '(-10 40) 
                   :locale kitchen))

       (macaroni-wrapper (make-instance 'wrapper 
                           :shape '(macaroni-wrapper) 
                           :locale kitchen))
       
       (dish1 (make-instance 'dish 
                :shape '(dish-1) 
                :locale kitchen))
       
       (dish2 (make-instance 'dish 
                :shape '(dish-2) 
                :locale kitchen))
       
       (bag1 (make-instance 'trashbag 
               :shape '(in-can) 
               :locale kitchen))
       
       (bag2 (make-instance 'trashbag 
               :locale kitchen))
       
       (can (make-instance 'trashcan 
              :state '(has-bag) 
              :bag bag1 
              :locale kitchen))
       
      
       (bottle-opener (make-instance 'bottle-opener 
                        :locale kitchen))
       
       (mustard-spoon (make-instance 'mustard-spoon 
                        :locale living-room))
       
       (rubber-band (make-instance 'rubber-band 
                      :locale living-room))
       
       (pot (make-instance 'pot
              :pos '(-75 45) 
              :locale kitchen 
              :mass 10))
       
       (olive-oil (make-instance 'olive-oil 
                    :temp 0 
                    :mass 1.764  
                    :cookstate 0 
                    :locale kitchen))
       
       (yellow-onion (make-instance 'yellow-onion 
                       :temp 0 
                       :mass 1.233  
                       :cookstate 0 
                       :locale kitchen))
       
       (celery (make-instance 'celery 
                 :temp 0 
                 :mass 1.254 
                 :cookstate 0 
                 :locale kitchen))
       
       (carrot (make-instance 'carrot 
                 :temp 0 
                 :mass 1.371  
                 :cookstate 0 
                 :locale kitchen))
       
       (tomato (make-instance 'tomato 
                 :temp 0 
                 :mass 1.028  
                 :cookstate 0 
                 :locale kitchen))
       
       (green-bean (make-instance 'green-bean 
                     :temp 0 
                     :mass .004  
                     :cookstate 0 
                     :locale kitchen))
       
       (potato (make-instance 'potato 
                 :temp 0 
                 :mass 2  
                 :cookstate 0 
                 :locale kitchen))
       
       (stock (make-instance 'stock 
                :temp 0 
                :mass 3  
                :cookstate 0 
                :locale kitchen))
       
       (soup (make-instance 'soup 
               :temp 0 
               :mass 1  
               :cookstate 0 
               :locale kitchen))
       
       (rs1 (make-instance 'instruction 
              :directive `(dice ,carrot)))
       
       (rs2 (make-instance 'instruction 
              :directive `(dice ,celery)))
       
       (rs3 (make-instance 'instruction 
              :directive `(dice ,yellow-onion)))
       
       (rs4 (make-instance 'instruction 
              :directive `(dice ,potato)))
       
       (rs5 (make-instance 'instruction 
              :directive `(dice ,tomato)))
       
       (rs6 (make-instance 'instruction 
              :directive `(saute (,yellow-onion ,celery ,carrot 
                                                ,olive-oil)
                                 in ,pot for (2 min))))
       
       (rs7 (make-instance 'instruction
              :directive `(add ,stock to ,pot)))
       
       (rs8 (make-instance 'instruction 
              :directive `(add ,tomato to ,pot)))
       
       (rs9 (make-instance 'instruction 
              :directive `(add ,potato to ,pot)))
       
       (rs10 (make-instance 'instruction 
               :directive `(add ,green-bean to ,pot)))
       
       (rs11 (make-instance 'instruction 
               :directive `(cook (4 min))))

       (recipe (make-instance 'recipe 
                 :name 'vegetable-soup 
                 :ingredients '(yellow-onion celery carrot tomato 
                                green-bean potato olive-oil stock)
                 :instructions
                 (list rs1 rs2 rs3 rs4 rs5 rs6 rs7 rs8 rs9 rs10 rs11)
                 :locale kitchen))
       
       (cookbook (make-instance 'cookbook 
                   :recipes (list recipe) 
                   :locale kitchen 
                   :mass 5))
       
       (food-drawer1 (make-instance 'drawer 
                       :contents (list yellow-onion celery carrot tomato
                                       green-bean potato) 
                       :locale kitchen))
       
       (food-drawer2 (make-instance 'drawer 
                       :contents (list olive-oil stock pot cookbook) 
                       :locale kitchen))
       
       (drawer1 (make-instance 'drawer 
                  :contents (list vase) 
                  ; :contents (list bottle-opener) 
                  :locale kitchen))
       
       (drawer2 (make-instance 'drawer 
                  :contents (list mustard-spoon rubber-band) 
                  :locale living-room)))     
       
    (subscribe walter *walter-router*)
    (subscribe dog *walter-router*)
    (subscribe cd *walter-router*)
    (subscribe computer *walter-router*)
    (subscribe tv *walter-router*)
    (subscribe plant *walter-router*)
  

    (assemble walter)  ;; ! maybe ok to couple with make-instance
        (assemble phone)
    (assemble tv)
    (assemble plant)
    (assemble flower)
    (assemble vase)
    (assemble cd)
    (assemble dog-dish)
    (assemble dog)
    (assemble window)
    (assemble computer)
    (assemble lamp)
    (assemble porch-swing)
    (assemble counter)
    (assemble stovetop)
    (assemble popcorn)
    (assemble egg)
    (assemble pan)
    (assemble measuring-cup)
    (assemble measuring-cup2)
    (assemble watch)    
    (assemble microwave)
    (assemble macaroni)
    (assemble dish2)
    (assemble dish1)
    (assemble bag1)
    (assemble bag2)
    (assemble can)
    (assemble bottle-opener)
    (assemble rubber-band)
    (assemble mustard-spoon)
    (assemble pot)
    (assemble olive-oil)
    (assemble tomato)
    (assemble green-bean)
    (assemble potato)
    (assemble celery)
    (assemble carrot)
    (assemble stock)
    (assemble soup)
    (assemble yellow-onion)
    (assemble cookbook)
    (assemble food-drawer1)
    (assemble food-drawer2)
    (assemble drawer1)
    (assemble drawer2)
    (assemble macaroni-wrapper)
    (assemble popcorn-wrapper)
    
    (assert-physob-relation `(pan ,pan))
    (assert-physob-relation `(on ,pan ,counter))
    (assert-physob-relation `(on ,(eggshell egg) ,counter))

    (start-activity kitchen 'adjusting-temperature :update-interval 100); used to be 1000
  
    ))
