#lang racket

;
; Switch ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-switch)

(define (make-switch id nodeA nodeB nodeC [max-speed 10])
  (let ((type   'switch)
        (occupied  #f))

    (define (free!)
        (set! occupied #f))

    (define (occupy! train-id)
        (set! occupied train-id))


    (define (free? [train-id #f])
      (not occupied))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)  type)
        ((eq? msg 'get-nodeA)   nodeA)
        ((eq? msg 'get-nodeB)   nodeB)
        ((eq? msg 'get-nodeC)   nodeC)
        ((eq? msg 'free?) free?)
        ((eq? msg 'occupied?) occupied)
        ((eq? msg 'free!) (free!))
        ((eq? msg 'occupy!) occupy!)
        ((eq? msg 'get-max-speed) max-speed)))
    dispatch))
