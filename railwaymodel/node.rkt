#lang racket

;
; Node ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-node)

(define (make-node id x y)
  
    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id) id)
        ((eq? msg 'get-x) x)
        ((eq? msg 'get-y) y)))
    dispatch)
