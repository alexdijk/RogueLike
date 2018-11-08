#lang racket
;;
;; main-menu-screen.rkt
;;
(provide
 main-menu-screen%)

(require
  racket/base
  "screen.rkt"
  "game-screen.rkt")

(define main-menu-screen%
  (class screen%
    (super-new)
    
    ; always turn control over to the game screen
    (define/override (update key-event)
      (new game-screen%))

    ; basic menu
    (define/override (draw canvas)
      (send canvas clear)
      (send canvas write-center *game-name* 10)
      (send canvas write-center "Press any key to continue" 12))
    )
  )
