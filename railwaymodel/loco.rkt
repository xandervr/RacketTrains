#lang racket

(provide make-loco)

(define (make-loco id node-A node-B distance)
  (let ((type       'loco)
        (max-speed  14)
        (speed      0)
        (train      #f))

    (define (couple! train-id)
      (set! train train-id))

    (define (uncouple!)
      (set! train #f))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-max-speed)  max-speed)
        ((eq? msg 'get-speed)      speed)
        ((eq? msg 'get-id)         id)
        ((eq? msg 'coupled?)       train)
        ((eq? msg 'couple!)        couple!)
        ((eq? msg 'uncouple!)      (uncouple!))
        ((eq? msg 'get-type)       type)))
    dispatch))
