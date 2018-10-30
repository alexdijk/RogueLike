#lang racket
;;
;;  gui.rkt
;;

(require
  racket/gui
  racket/draw
  ascii-canvas
  "screen.rkt"
  "main-menu-screen.rkt")

;; create a new GUI
(define gui%
  (class object%
    (init-field width-in-chars
                height-in-chars)

    ;; create the frame
    (define frame
      (new frame%
           (label "Racket Roquelike")
           (style '(no-resize-border))))

    ;; create the ascii canvas
    (define canvas
      (new ascii-canvas%
           (parent frame)
           (width-in-characters width-in-chars)
           (height-in-characters height-in-chars)))

    ;; the active screen
    (define active-screen (new main-menu-screen%))

    ;; make everything visible
    (send frame show #t)

    ;; do initial drawing
    (send active-screen draw canvas)
    (send frame refresh)
    
    ;; finish init
    (super-new)))




