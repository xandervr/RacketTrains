#lang racket

;
; Detection-block ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-detection-block)

(define (make-detection-block id track)
  (let ((type   'detection-block)
        (free?  #t))

    (define (free!)
        (set! free? #t))

    (define (occupy!)
        (set! free? #f))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)    type)
        ((eq? msg 'get-track)   track)
        ((eq? msg 'free?)   free?)
        ((eq? msg 'free!)   (free!))
        ((eq? msg 'occupy!) (occupy!))
        (else (error "Unknown message ---- Detection-block"))))
    dispatch))
