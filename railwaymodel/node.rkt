#lang racket
(provide make-node)

(define (make-node id x y)
  (let ((type   'node)
        (tracks '()))

    (define (get-amount-of-tracks)
      (define (iter amount tracks)
        (if (null? tracks)
            amount
            (iter (+ amount 1) (cdr tracks))))
      (iter 0 tracks))

    (define (add-track! track)
      (if (< (get-amount-of-tracks) 2)
          (set! tracks (cons track tracks))
          (error "too many tracks in this node")))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-type) type)
        ((eq? msg 'get-id) id)
        ((eq? msg 'get-x) x)
        ((eq? msg 'get-y) y)
        ((eq? msg 'get-tracks) tracks)
        ((eq? msg 'add-track!) add-track!)))
    dispatch))
