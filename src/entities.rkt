#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; entities.rkt
;;

(provide (all-defined-out))

(require
  "thing.rkt"
  "point.rkt"
  "items.rkt")

(define-thing entity
  (character #\x)
  (color "white")
  (location (pt 0 0))
  (inventory '()))

(define-thing enemy entity
  (name "enemy")
  (attack 10)
  (defense 10)
  (health 10)
  ((act me world) (void)))

(define-thing wandering-enemy enemy
  ((act me world)
   (send world try-move
         me
         (+ (thing-get me 'location)
            (pt (- (random 3) 1)
                (- (random 3) 1))))))

(define-thing exploding-enemy enemy
  ((act me world)
   (define distance-to-player
     (distance (thing-get (send world get-player) 'location)
               (thing-get me ' location)))
   (when (<= distance-to-player 2.0)
     ; log a message
     (send world log (format "~a explodes violently" (thing-get me 'name)))

     (for* ((xd (in-range -1 2))
            (yd (in-range -1 2)))
       (for ((other (send world get-entities
                          (+ (thing-get me 'location)
                             (pt xd yd)))))
         (unless (eqv? me other)
           (send world attack me other))))
     (thing-set! me 'health -1))))

;; an enemy strategy that comes after you...
(define-thing seeking-enemy wandering-enemy
  ((act me world)
   (cond
     ;; 50/50
     ((= 0 (random 2))
      (define player-pt (thing-get (send world get-player) 'location))
      (define me-pt (thing-get me 'location))
      (define dir (unit (- player-pt me-pt)))
      (send world try-move
            me
            (+ me-pt
               (inexact->exact (round (pt-x dir)))
               (inexact->exact (round (pt-y dir))))))
     (else
      (thing-call wandering-enemy 'act me world)))))

;; a fleeing enemy strategy
(define-thing fleeing-enemy wandering-enemy
  ((act me world)
   (cond
     ((= 0 (random 2))
      (define player-pt (thing-get (send world get-player) 'location))
      (define me-pt (thing-get me 'location))
      (define dir (unit (- player-pt me-pt)))
      (send world try-move
            me
            (- me-pt
               (inexact->exact (round (pt-x dir)))
               (inexact->exact (round (pt-y dir))))))
     (else
      (thing-call wandering-enemy 'act me world)))))

;; the actual enemies
;(define random-enemies
(define *enemies*
  (vector
   (make-thing wandering-enemy
               (name "rat")
               (color "silver")
               (character #\r))
   
   (make-thing fleeing-enemy
               (name "cat")
               (character #\c)
               (color "yellow"))
   
   (make-thing exploding-enemy
               (name "bomb")
               (color "white")
               (character #\u000f)
               (attack 50))

   (make-thing seeking-enemy
               (name "teeth")
               (character #\u00e4)
               (color "HotPink")
               (attack 40))
   
   (make-thing seeking-enemy
               (name "goblin")
               (character #\g)
               (color "orange")
               (attack 75))
   
   (make-thing seeking-enemy
               (name "snake")
               (character #\s)
               (color "green")
               (attack 25)
               (defense 5)
               ((act me world)
                (thing-call seeking-enemy 'act me world)
                (thing-call exploding-enemy 'act me world)))
   
;   (make-thing seeking-enemy
;               (name "doom")
;               (character #\u0001)
;               (color "red")
;               (attack 75))
   ))

