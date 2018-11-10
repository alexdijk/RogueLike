#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; world.rkt
;;

(provide world%)

(require
  noise
  "thing.rkt"
  "entities.rkt"
  "point.rkt")

(define-thing tile
  (walkable #f)
  (character #\space)
  (color "black"))

(define-thing empty tile
  (walkable #t))

(define-thing wall tile
  (character #\#)
  (color "white"))

(define-thing water tile
  (character #\u00db)
  (color "blue"))

(define-thing tree tile
  (character #\u0005)
  (color "green"))

(define world%
  (class object%
    (super-new)
    
    ;; store the player
    (define player
      (make-thing entity
                  (name "player")
                  (attack 10)
                  (defense 10)
                  (health 100)))
    (define/public (get-player) player)

    ;; log messages for the player
    (define log-messages '())

    (define/public (log msg)
      (set! log-messages (cons msg log-messages)))

    (define/public (get-log (count 1))
      (let loop ((i 0) (msgs log-messages))
        (cond
          ((= i count) '())
          ((null? msgs) (cons "" (loop (+ i 1) msgs)))
          (else
           (cons (car msgs)
                 (loop (+ i 1) (cdr msgs)))))))
    
    ; get info on point, caching for future use
    ; hash on (x y) => char
    (define tiles (make-hash))
    
    (define/public (get-tile x y)
      ;; if the tile doesn't exist, generate it
      (unless (hash-has-key? tiles (list x y))
        ;; generate
        (define new-tile
          (let ()
            (define wall?   (> (simplex (* 0.1 x) (* 0.1 y) 0)         0.0))
            (define water?  (> (simplex (* 0.1 x) 0         (* 0.1 y)) 0.5))
            (define tree?   (> (simplex 0         (* 0.1 x) (* 0.1 y)) 0.5))
            (cond
              (wall? wall)
              (water? water)
              (tree? tree)
              (else empty))))
        (hash-set! tiles (list x y) new-tile)

        ;; random generate new enemy
        (when (and (thing-get new-tile 'walkable)
                   (< (random 50) 1))
          (define new-thing
            (make-thing
             (vector-ref random-enemies
                         (random (vector-length random-enemies)))
             (location (pt x y))))
          (set! npcs (cons new-thing npcs))))
      (hash-ref tiles (list x y)))

    ;; try to move entity to a location
    (define/public (try-move entity target)
      (define tile (send this get-tile (pt-x target) (pt-y target)))
      (define others
        (filter
         (λ (thing) (and (not (eqv? thing entity))
                         (= (thing-get thing 'location) target)))
         (cons player npcs)))

      (cond
        ; if it's not walkable, do nothing
        ((not (thing-get tile 'walkable))
         (void))
      
        ((null? others)
         (thing-set! entity 'location target))
        (else
         (for ((other (in-list others)))
           (define damage
             (max 0 (- (random (max 1 (thing-get entity 'attack)))
                       (random (max 1 (thing-get other 'defense))))))
           (thing-set! other 'health (- (thing-get other 'health) damage))

           (send this log
                 (format "~a attacked ~a, did ~a damage"
                         (thing-get entity 'name)
                         (thing-get other 'name)
                         damage))))))
    
    ;; store a list of non-player entities
    (define npcs '())

    ;; update the npcs
    (define/public (update-npcs)
      (for ((npc (in-list npcs)))
        (thing-call npc 'act npc this))
      (set! npcs
            (filter
             (λ (npc)
               (when (<= (thing-get npc 'health) 0)
                 (send this log (format "~a has died" (thing-get npc 'name))))
               (> (thing-get npc 'health) 0))
             npcs)))

    ;; draw npcs on canvas
    (define/public (draw-npcs canvas)
;     (if (empty? npcs) (send this log "npcs empty")
;          (send this log "npcs not empty"))
      
      (for ((npc (in-list npcs)))
        (define x/y (recenter canvas (- (thing-get player 'location)
                                        (thing-get npc 'location))))
        (when (and (<= 0 (pt-x x/y) (sub1 (send canvas get-width-in-characters)))
                   (<= 0 (pt-y x/y) (sub1 (send canvas get-height-in-characters))))
          (send canvas write
                (thing-get npc 'character)
                (pt-x x/y)
                (pt-y x/y)
                (thing-get npc 'color)))))

    (define/public (get-entities p)
      (for/list ((entity (cons player npcs))
                 #:when (= p (thing-get entity 'location)))
        entity))))
