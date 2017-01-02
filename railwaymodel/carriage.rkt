#lang racket

(provide make-carriage)

(define (make-carriage id node-A node-B distance)
  (let ((type       'carriage)
        (max-speed  14)
        (train      #f))

    (define (couple! train-id)
      (set! train train-id))

    (define (uncouple!)
      (set! train #f))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-max-speed)  max-speed)
        ((eq? msg 'get-id)         id)
        ((eq? msg 'coupled?)       train)
        ((eq? msg 'couple!)        couple!)
        ((eq? msg 'uncouple!)      (uncouple!))
        ((eq? msg 'get-type)       type)))
    dispatch))
