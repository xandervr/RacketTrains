#lang racket

;
; Locomotive ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-locomotive)

(define (make-locomotive id)
  (let ((type       'locomotive)
        (max-speed  14))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-max-speed) max-speed)
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)  type)))
    dispatch))
