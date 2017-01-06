#lang racket

;
; Train ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "locomotive.rkt")
(require "cart.rkt")

(provide make-train)

(define (make-train id)
  (let ((type 'train)
        (schedule '())
        (max-speed  14))

    (define (set-speed! new-speed)
        (set! speed new-speed))

    (define (set-schedule! new-schedule)
        (set! schedule new-schedule))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-max-speed) max-speed)
        ((eq? msg 'get-schedule)  schedule)
        ((eq? msg 'set-schedule!) set-schedule!)
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)    type)
        (else (error "Unknown message --- TRAIN"))))
    dispatch))