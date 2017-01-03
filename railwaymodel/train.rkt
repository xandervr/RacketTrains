#lang racket

;
; Train ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "locomotive.rkt")
(require "cart.rkt")

(provide make-train)

(define (make-train id)
  (let ((type 'train)
        (locomotive (make-locomotive id))
        (carts '())
        (speed 0)
        (schedule '())
        (max-speed  3))

    (define (add-cart!)
      (set! carts (cons (make-cart) carts)))
      
    (define (set-speed! new-speed)
        (set! speed new-speed))

    (define (set-schedule! new-schedule)
        (set! schedule new-schedule))

    (define (dispatch msg)
      (cond
        ((eq? msg 'add-cart!)   (add-cart!))
        ((eq? msg 'get-speed) speed)
        ((eq? msg 'set-speed!) set-speed!)
        ((eq? msg 'get-max-speed) max-speed)
        ((eq? msg 'get-schedule)  schedule)
        ((eq? msg 'set-schedule!) set-schedule!)
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)    type)
        (else (error "Unknown message --- TRAIN"))))
    dispatch))