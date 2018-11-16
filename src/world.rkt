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
  "point.rkt"
  "items.rkt")

(define (vector-choose-random v)
  (vector-ref v (random (vector-length v))))

(define (vector-choose-biased v)
  (vector-choose-random v))

(define-thing tile
  (walkable #f)
  (character #\space)
  (color "black")
  (items '()))

(define-thing empty tile
  (walkable #t))

(define-thing wall tile
  (character #\#)
  (color "gainsboro"))

(define-thing water tile
  (character #\u00b1)
  (color "cornflowerblue"))

(define-thing tree tile
  (character #\u0005)
  (color "forestgreen"))

(define world%
  (class object%
    (super-new)
    
    ;; store the player
    (define player
      (make-thing entity
                  (name "player")
                  ;(color "MediumBlue")
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
              (wall? (make-thing wall))
              (water? (make-thing water))
              (tree? (make-thing tree))
              (else (make-thing empty)))))
        (hash-set! tiles (list x y) new-tile)

        ;; random generate new enemy
        (when (and (thing-get new-tile 'walkable)
                   (< (random 100) 1))
          (define new-thing
            ;; (make-thing
            ;;  (vector-ref random-enemies
            ;;             (random (vector-length random-enemies)))
            (let ((base (vector-choose-random *enemies*)))
              (make-thing base
                          (location (pt x y)))))
            
          ;; store it in the npc list 
          (set! npcs (cons new-thing npcs)))

        (when (and (thing-get new-tile 'walkable)
                   (< (random 500) 1))
          (define new-item
            (let ((base (vector-choose-biased (vector-choose-random *all-items*))))
              (make-thing base)))
          (thing-set! new-tile 'items (cons new-item (thing-get new-tile 'items)))))
      
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
         (thing-set! entity 'location target)

        (define (pick-up item)
          (thing-set! entity 'inventory (cons item (thing-get entity 'inventory)))
          (thing-set! tile 'items (remove item (thing-get tile 'items)))
          (thing-call item 'on-pick-up item entity this))

        (define (drop item)
          (thing-set! entity 'inventory (remove item (thing-get entity 'invertory)))
          (thing-set! tile 'items (cons item (thing-get tile 'items)))
          (thing-call item 'on-drop item entity this))

        (define (consume item)
          (thing-set! tile 'items (remove item (thing-get tile 'items)))
          (thing-call item 'on-pick-up item entity this))

        (for ((item (in-list (thing-get tile 'items))))
          (for ((in-inv (in-list (thing-get entity 'inventory)))
                #:when (eq? (thing-get item 'category)
                            (thing-get in-inv 'category)))
            (drop in-inv))

          (if (thing-get item 'consumable)
              (consume item)
              (pick-up item))))

        ;; check this
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
                         damage))))
        ;; until here
        ))

    (define/public (attack entity other)
      ;; damage
      (define damage
        (max 0 (- (random (max 1 (thing-get entity 'attack)))
                  (random (max 1 (thing-get other 'defense))))))
      (thing-set! other 'health (- (thing-get other 'health) damage))

      (send this log
            (format "~a attacked ~a, did ~a damage"
                    (thing-get entity 'name)
                    (thing-get other 'name)
                    damage)))
    
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
