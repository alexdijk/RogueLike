#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; point.rkt
;;
(require
  racket/base)

(provide (all-defined-out))

(define pt make-rectangular)
(define pt-x real-part)
(define pt-y imag-part)

(define (recenter canvas orig)
  (+ orig (pt (quotient (send canvas get-width-in-characters) 2)
              (quotient (send canvas get-height-in-characters) 2))))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))

(define (unit p)
  (define d (distance 0 p))
  (pt (/ (pt-x p) d)
      (/ (pt-y p) d)))
