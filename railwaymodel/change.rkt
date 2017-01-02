#lang racket

(provide make-switch)

(define (make-switch id node-A node-B node-C)
  (let ((type       'switch)
        (position   node-B)
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

    (define (change!)
      (if (eq? position node-B)
          (set! position node-C)
          (set! position node-B))
      position)

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id)       id)
        ((eq? msg 'get-type)     type)
        ((eq? msg 'get-node-A)   node-A)
        ((eq? msg 'get-node-B)   node-B)
        ((eq? msg 'get-node-C)   node-C)
        ((eq? msg 'stop?)        stop?)
        ((eq? msg 'stop!)        (stop!))
        ((eq? msg 'clear?)       (not stop?))  ;; TODO overbodig?
        ((eq? msg 'clear!)       (clear!))
        ((eq? msg 'reserved?)    reserved?)
        ((eq? msg 'reserve!)     (reserve!))
        ((eq? msg 'free!)        (free!))
        ((eq? msg 'get-position) position)
        ((eq? msg 'change!       (change!)))))
    dispatch))
