#lang racket

;
; Switch ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-switch)

(define (make-switch id nodeA nodeB nodeC [max-speed 10])
  (let ((type   'switch)
        (position   1)
        (occupied  #f))

    (define (free!)
        (set! occupied #f))

    (define (occupy! train-id)
        (set! occupied train-id))


    (define (free? [train-id #f])
      (not occupied))

    (define (switch!)
      (if (= position 1)
        (set! position 2)
        (set! position 1))
      position)

    (define (get-track)
      #t)

    (define (get-nodeB)
      (if (= position 1)
        nodeB
        nodeC))

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
        ((eq? msg 'get-position) position)
        ((eq? msg 'get-max-speed) max-speed)
        ((eq? msg 'switch!  (switch!)))))
    dispatch))
