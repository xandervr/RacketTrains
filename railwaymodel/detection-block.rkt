#lang racket

;
; Detection-block ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "track.rkt")

(provide make-detection-block)

(define (make-detection-block id nodeA nodeB [max-speed 10])
  (let ((type   'detection-block)
        (track (make-track nodeA nodeB max-speed)))

    (define (free!)
        (track 'free!))

    (define (occupy! train-id)
        ((track 'occupy!) train-id))

    (define (free? [train-id #f])
        ((track 'free?) train-id))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-id)  id)
        ((eq? msg 'get-type)    type)
        ((eq? msg 'get-track)   track)
        ((eq? msg 'free?) free?)
        ((eq? msg 'free!)   (free!))
        ((eq? msg 'occupy!) occupy!)
        (else (error "Unknown message ---- Detection-block"))))
    dispatch))
