#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; items.rkt
;;
(provide (all-defined-out))

(require
  "thing.rkt")

(define-thing item
  (character #\x)
  (color "white")
  (consumable #f)
  (category "unknown")
  ((on-pick-up item entity world) (void))
  ((on-drop item entity world) (void)))

(define-thing armor item
  (character #\})
  (defense 0)
  (category 'armor)
  ((on-pick-up item entity world)
   (thing-set! entity 'defense (+ (thing-get entity 'defense)
                                  (thing-get item 'defende))))
  ((on-drop item entity world)
   (thing-get entity 'defense (- (thing-get entity 'defense)
                                 (thing-get item 'defense)))))
(define *armor*
  (vector
   (make-thing armor (name "leather")   (color "brown")  (defense 1))
   (make-thing armor (name "chain")     (color "gray")   (defense 2))
   (make-thing armor (name "plate")     (color "white")  (defense 3))
   (make-thing armor (name "enchanted") (color "purple") (defense 5))))

(define-thing weapon item
  (character #\>)
  (attack 0)
  (category 'weapon)
  ((on-pick-up item entity world)
   (thing-set! entity 'attack (+ (thing-get entity 'attack)
                                 (thing-get item 'attack))))
  ((on-drop item entity world)
   (thing-set! entity 'attack (- (thing-get entity 'attack)
                                 (thing-get item 'attack)))))
(define *weapons*
  (vector
   (make-thing weapon (name "club")        (color "brown")  (attack 1))
   (make-thing weapon (name "dagger")      (color "gray")   (attack 2))
   (make-thing weapon (name "battle axe")  (color "white")  (attack 3))
   (make-thing weapon (name "longsword")   (color "white")  (attack 4))
   (make-thing weapon (name "magic sword") (color "purple") (attack 5))))

(define-thing potion item
  (character #\!)
  (catagory 'potion)
  (consumable #t))

(define *potions*
  (vector
   (make-thing potion (name "health potion") (color "red")
               ((on-pick-up item entity world)
                (thing-set! entity 'health (+ 10 (thing-get entity 'health)))))))

(define *all-items*
  (vector *armor* *weapons* *potions*))
