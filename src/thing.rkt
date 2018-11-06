#lang racket/base

(provide 
  define-thing
  make-thing			
  thing-get
  thing-set!
  thing-call)

; A simple wrapper for things
(define-struct thing (data)
  #:constructor-name new-thing)
    
; A very simple prototype based object system
(define-syntax make-thing
  (syntax-rules ()
    ; Create an empty thing, bind a function
    [(_ [(k arg* ...) body* ...] rest ...)
     (let ([thing (make-thing rest ...)])
       (hash-set! (thing-data thing) 'k
                  (lambda (arg* ...)
                    body* ...))
       thing)]
    [(_ base [(k arg* ...) body* ...] rest ...)
     (let ([thing (make-thing base rest ...)])
       (hash-set! (thing-data thing) 'k
                  (lambda (arg* ...)
                    body* ...))
       thing)]    
    ; Add a key/value pair to a thing
    [(_ [k v] rest ...)
     (let ([thing (make-thing rest ...)])
       (hash-set! (thing-data thing) 'k v)
       thing)]
    [(_ base [k v] rest ...)
     (let ([thing (make-thing base rest ...)])
       (hash-set! (thing-data thing) 'k v)
       thing)]
    ; Create an empty thing
    [(_)
     (new-thing (make-hasheq))]
    ; Copy an existing thing
    [(_ base)
     (if (thing? base)
         (new-thing (hash-copy (thing-data base)))
         (error 'make-thing "~a is not a thing to extend" base))]))

; Shortcut to define a thing
(define-syntax-rule (define-thing name arg* ...)
  (define name (make-thing arg* ...)))

; Access a value from a thing
(define (thing-get thing key [default (void)])
  (cond
    [(not (thing? thing))
     (error 'thing-get "~a is not a thing" thing)]
    [(or (not (void? default))
         (hash-has-key? (thing-data thing) key))
     (hash-ref (thing-data thing) key default)]
    [else
     (error 'thing-get "~a does not contain a value for ~a" thing 'key)]))

; Set a value in a thing
(define (thing-set! thing key val)
  (cond
    [(not (thing? thing))
     (error 'thing-set! "~a is not a thing" thing)]
    [else
     (hash-set! (thing-data thing) key val)]))

; Call a function stored in a thing
(define (thing-call thing key . args)
  (cond
    [(not (thing? thing))
     (error 'thing-call "~a is not a thing" thing)]
    [(thing-get thing key #f)
     => (lambda (f)
          (if (procedure? f)
              (apply f args)
              (error 'thing-call "~a is not a procedure in ~a, it is ~a"
                     key thing f)))]
    [else
     (error 'thing-get "~a does not contain a value for ~a" thing 'key)]))
