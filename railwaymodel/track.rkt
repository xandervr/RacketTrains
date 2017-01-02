#lang racket

(provide make-track)

(define (make-track id node-A node-B)
  (let ((type       'track)
        (reserved?  #f)
        (stop?      #f))

    (define (stop!)
      (set! stop? #t))

    (define (clear!)
      (set! stop? #f))

    (define (reserve!)
      (set! reserved? #t))

    (define (free!)
      (set! reserved? #f))


    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id)      id)
        ((eq? msg 'get-type)    type)
        ((eq? msg 'get-node-A)  node-A)
        ((eq? msg 'get-node-B)  node-B)
        ((eq? msg 'stop?)       stop?)
        ((eq? msg 'stop!)       (stop!))
        ((eq? msg 'clear?)      (not stop?))  ;; TODO overbodig?
        ((eq? msg 'clear!)      (clear!))
        ((eq? msg 'reserved?)   reserved?)
        ((eq? msg 'reserve!)    (reserve!))
        ((eq? msg 'free!)       (free!))))
    dispatch))
