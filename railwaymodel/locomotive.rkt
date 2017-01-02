#lang racket

;
; Locomotive ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-locomotive)

(define (make-locomotive id)
  (let ((type       'locomotive)
        (max-speed  14)
        (speed      0))

    (define (set-speed! new-speed)
      (set! speed new-speed))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-max-speed) max-speed)
        ((eq? msg 'get-speed) speed)
        ((eq? msg 'set-speed!)  set-speed!)
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)  type)))
    dispatch))
