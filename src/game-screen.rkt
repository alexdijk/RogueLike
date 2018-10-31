#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; game-screen.rkt
;;
(provide
 game-screen%)

(require
  racket/draw
  "screen.rkt")

(define pt make-rectangular)
(define pt-x real-part)
(define pt-y imag-part)

(define (recenter canvas orig)
  (+ orig (pt (quotient (send canvas get-width-in-characters) 2)
              (quotient (send canvas get-height-in-characters) 2))))

(define game-screen%
  (class screen%
    ;; store the player's state
    ;; use an imaginary number for a point
    (define player (pt 0 0))

    (define/override (update key-event)
      (case (send key-event get-key-code)
        ((numpad8 #\w up) (set! player (+ (pt 0 -1) player)))
        ((numpad4 #\a left) (set! player (+ (pt -1 0) player)))
        ((numpad6 #\s down) (set! player (+ (pt 0 1) player)))
        ((numpad2 #\d right) (set! player (+ (pt 1 0) player))))

      ;; keep this state
      this)

    ;; draw the game itself
    (define/override (draw canvas)
      (send canvas clear)

      ;; draw the player
      ;; 0x0 is the center point of the canvas
      (let ((player (recenter canvas player)))
        (send canvas write #\@ (pt-x player) (pt-y player))))

    (super-new)))
