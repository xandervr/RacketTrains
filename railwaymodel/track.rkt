#lang racket

;
; Track ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-track)

(define (make-track nodeA nodeB [max-speed 10])
  (let ((type   'track))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-type)    type)
        ((eq? msg 'get-nodeA)   nodeA)
        ((eq? msg 'get-nodeB)   nodeB)
        ((eq? msg 'get-max-speed) max-speed)))
    dispatch))
