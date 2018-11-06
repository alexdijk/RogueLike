#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; entities.rkt
;;

(provide (all-defined-out))

(require
  "thing.rkt"
  "point.rkt")

(define-thing entity
  (character #\x)
  (color "white")
  (location (pt 0 0)))

(define-thing enemy entity
  (name "enemy")
  (color "gray")
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

;(define random-enemies
;  (vector
;   (make-thing wandering-enemy
;            (name "rat")
;            (character #\r))))
