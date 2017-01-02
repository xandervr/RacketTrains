#lang racket

;
; Node ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-node)

(define (make-node id x y)
  (let ((type   'node)
        (tracks '()))

    (define (number-of-tracks?)
      (define (track-number-iter number tracks)
        (if (null? tracks)
            number
            (track-number-iter (+ number 1) (cdr tracks))))
      (track-number-iter 0 tracks))

    (define (add-track! track)
      (if (< (number-of-tracks?) 2)
          (set! tracks (cons track tracks))
          (error "ERROR: Number of tracks exceeds node maximum. ----- NODE")))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-type) type)
        ((eq? msg 'get-id) id)
        ((eq? msg 'get-x) x)
        ((eq? msg 'get-y) y)
        ((eq? msg 'get-tracks) tracks)
        ((eq? msg 'add-track!) add-track!)))
    dispatch))
