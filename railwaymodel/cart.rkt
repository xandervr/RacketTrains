#lang racket

;
; Cart ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-cart)

(define (make-cart)
  (let ((type   'cart))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-type)  type)))
    dispatch))
