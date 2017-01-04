#lang racket

;
; Track ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-track)

(define (make-track nodeA nodeB [max-speed 10])
  (let ((type   'track)
        (occupied  #f))

    (define (free!)
        (set! occupied #f))

    (define (occupy! train-id)
        (set! occupied train-id))

    (define (free? [train-id #f])
      (not occupied))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-type)    type)
        ((eq? msg 'get-nodeA)   nodeA)
        ((eq? msg 'get-nodeB)   nodeB)
        ((eq? msg 'occupied?)   occupied)
        ((eq? msg 'free?) free?)
        ((eq? msg 'free!)   (free!))
        ((eq? msg 'occupy!) occupy!)
        ((eq? msg 'get-max-speed) max-speed)
        (else (error "Unknown message ---- Track"))))
    dispatch))
