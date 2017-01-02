#lang racket

;
; NMBS ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../railwaymodel/rwm.rkt")

(provide make-NMBS)

(define (make-NMBS)
    (define rwm (load-rwm "railway.txt"))
    
    (define (update d)
        d)

    (define (dispatch msg)
        (cond
            ((eq? msg 'update) update))
        )
    dispatch)

