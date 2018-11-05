#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; point.rkt
;;

(provide (all-defined-out))

(define pt make-rectangular)
(define pt-x real-part)
(define pt-y imag-part)

(define (recenter canvas orig)
  (+ orig (pt (quotient (send canvas get-width-in-characters) 2)
              (quotient (send canvas get-height-in-characters) 2))))
