#lang racket

;
; Switch ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-switch)

(define (make-switch id nodeA nodeB nodeC)
  (let ((type   'switch)
        (position   nodeB)
        (free?  #f))

    (define (free!)
      (set! free? #t))

    (define (occupy!)
      (set! free? #f))

    (define (switch!)
      (if (eq? position nodeB)
          (set! position nodeC)
          (set! position nodeB))
      position)

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)  type)
        ((eq? msg 'get-nodeA)   nodeA)
        ((eq? msg 'get-nodeB)   nodeB)
        ((eq? msg 'get-nodeC)   nodeC)
        ((eq? msg 'free!) (free!))
        ((eq? msg 'occupy!) (occupy!))
        ((eq? msg 'get-position) position)
        ((eq? msg 'switch!  (switch!)))))
    dispatch))
