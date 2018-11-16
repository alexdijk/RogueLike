#lang racket

(require
  racket/gui
  "gui.rkt")

(define roguelike
  (new gui%
       [width-in-chars 40]
       [height-in-chars 24]))
