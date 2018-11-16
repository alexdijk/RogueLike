#lang racket
;;
;;  gui.rkt
;;

(require
  racket/gui
  ascii-canvas
  "screen.rkt"
  "main-menu-screen.rkt")

;; create a new GUI
(define gui%
  (class object%
    (init-field width-in-chars
                height-in-chars)
    (super-new)
  
    ;; create the frame
    (define frame
      (new frame%
           (label *game-name*)
           (style '(no-resize-border))))

    ;; create the ascii canvas
    (define canvas
      (new (class ascii-canvas%
             (inherit-field
              width-in-characters
              height-in-characters)

             ;; finish init
             (super-new
              (parent frame)
              (width-in-characters width-in-chars)
              (height-in-characters height-in-chars))

             (define/override (on-char key-event)
               (case (send key-event get-key-code)
                 ;; exit
                 ((escape) (exit))
                 ;; ignore key
                 ((release menu) (void))
                 ;; everything to the screen
                 (else
                  (set! active-screen (send active-screen update key-event))
                  (cond
                    ;; if it's still valid screen, redraw
                    ((is-a? active-screen screen%)
                     (send active-screen draw this)
                     (send frame refresh))
                    ;; or exit
                    (else
                     (exit)))))))))
 
    ;; the active screen
    (define active-screen (new main-menu-screen%))
    
    ;; make everything visible
    (send frame show #t)

    ;; do initial drawing
    (send active-screen draw canvas)
    (send frame refresh)))
