#lang racket

;
; Train ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "locomotive.rkt")

(provide make-train)

(define (make-train id)
  (let ((type 'train)
        (speed 0)
        (schedule '())
        (locomotive (make-locomotive id))
        (carts '())
        (max-speed  0))

    (define (add-cart! cart)
      (set! carts (cons cart carts)))
      
    (define (set-speed! new-speed)
        (set! speed new-speed)
        ((locomotive 'set-speed!) new-speed))



    (define (dispatch msg)
      (cond
        ((eq? msg 'add-cart!)   add-cart!)
        ((eq? msg 'get-speed) speed)
        ((eq? msg 'set-speed!) set-speed!)
        ((eq? msg 'get-schedule)  schedule)
        ((eq? msg 'set-schedule!) set-schedule!)
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)    type)))
    dispatch))
