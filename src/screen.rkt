#lang racket
;;
;; screen.rkt
;;
(provide
 screen%
 *game-name*)

(define *game-name* "Survival Dragon")

(define screen%
  (class object%

    (define/public (update key-event)
      (error 'screen% "override this method"))

    (define/public (draw canvas)
      (error 'screen% "override this method"))

    (super-new)))

