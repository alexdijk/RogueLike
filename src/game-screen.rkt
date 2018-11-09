#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; game-screen.rkt
;;
(provide
 game-screen%)

(require
  noise
  "screen.rkt"
  "thing.rkt"
  "world.rkt"
  "point.rkt")

(define game-screen%
  (class screen%
    (super-new)
    
    ;; create a world
    (define world (new world%))
    
    ;; store the player's state
    ;; use an imaginary number for a point
    (define player (send world get-player))
    
    (define caves (make-hash))
   
    ;; process keyboard events
    (define game-over #f)
    (define/override (update key-event)
      (cond
        (game-over (new game-screen%))
        (else
         ;(define target player)
         (case (send key-event get-key-code)
           ((numpad8 #\w up)    (send world try-move player (+ (pt  0  1) (thing-get player 'location))))
           ((numpad4 #\a left)  (send world try-move player (+ (pt  1  0) (thing-get player 'location))))
           ((numpad2 #\s down)  (send world try-move player (+ (pt  0 -1) (thing-get player 'location))))
           ((numpad6 #\d right) (send world try-move player (+ (pt -1  0) (thing-get player 'location)))))

         (send world update-npcs)

         ;         (when (eq? 'empty (get-tile (pt-x target) (pt-y target)))
         ;           (set! player target))
         
         (when (<= (thing-get player 'health) 0)
           (send world log "You Lose!!")
           (send world log "Press any key to continue")
           (set! game-over #t))
      
         ;; keep the state
         this)))

    ;; draw the game itself
    (define/override (draw canvas)
      (send canvas clear)
      (define player (send world get-player))
      
      (for* ((xi (in-range (send canvas get-width-in-characters)))
             (yi (in-range (send canvas get-height-in-characters))))
        (define x/y (recenter canvas (- (thing-get player 'location) (pt xi yi))))
        (define tile (send world get-tile (pt-x x/y) (pt-y x/y)))
        (send canvas write
              (thing-get tile 'character)
              xi
              yi
              (thing-get tile 'color)))
      
      ;; draw the player
      ;; 0x0 is the center point of the canvas
      (let ((player (recenter canvas (pt 0 0))))
        (send canvas write #\@ (pt-x player) (pt-y player)))

      (send world draw-npcs canvas)
      
      (for ((i (in-naturals))
            (key (in-list '(attack defense health))))
        (send canvas write-string
              (format "~a: ~a" key (thing-get player key))
              1 (+ i 1)
              "green"))
      
      (for ((i (in-naturals))
            (msg (in-list (send world get-log 3))))
        (send canvas write-string
              msg
              1 (- (send canvas get-height-in-characters) i 2)
              "green"))
      
      ; Debug: Show the player location
      ;      (send canvas write-string
      ;            (format "~a, ~a" (pt-x player) (pt-y player))
      ;            1 1
      ;            "cyan")
      
      )))
