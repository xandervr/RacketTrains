#lang racket

;
; Detection-track ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "track.rkt")

(provide make-detection-track)

(define (make-detection-track id nodeA nodeB [max-speed 10])
  (let ([type   'detection-track]
        [occupied #f])

    (define (free!)
        (set! occupied #f))

    (define (occupy! train-id)
        (set! occupied train-id))

    (define (free? [train-id #f])
      (not occupied))


    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)    type)
        ((eq? msg 'get-nodeA)   nodeA)
        ((eq? msg 'get-nodeB)   nodeB)
        ((eq? msg 'get-max-speed) max-speed)
        ((eq? msg 'occupied?) occupied)
        ((eq? msg 'free?) free?)
        ((eq? msg 'free!)   (free!))
        ((eq? msg 'occupy!) occupy!)
        (else (error "Unknown message ---- Detection-track"))))
    dispatch))
