#lang racket
;;
;; main-menu-screen.rkt
;;
(provide
 main-menu-screen%)

(require
  "screen.rkt")

(define main-menu-screen%
  (class screen%
    ; always turn control over to the game screen
    (define/override (update key-event)
      ; TODO: (new game-screen%)
      this)

    ; basic menu
    (define/override (draw canvas)
      (send canvas clear)
      (send canvas write-center "Racket Roguelike" 10)
      (send canvas write-center "Press any key to continue" 12))

    (super-new)))

