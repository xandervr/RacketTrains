#lang racket

;
; NMBS ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../railwaymodel/rwm.rkt")

(provide make-NMBS)

(define (make-NMBS)
    (define rwm (load-rwm "railway.txt"))
    
    (define (update infrabel)
        (hash-for-each (rwm-ls rwm) 
          (lambda (id train) 
            (process-train infrabel train))))

    (define (process-train infrabel train)
      (let* ([location (hash-ref (rwm-ds rwm) ((infrabel 'get-train-location) (train 'get-id)))]
              [nA ((location 'get-track) 'get-nodeA)] 
              [nB ((location 'get-track) 'get-nodeB)]
              [schedule (train 'get-schedule)])


        (define (process-schedule schedule)
          (cond
            ((or (null? schedule) (null? (cdr schedule))) (error "ENDED"))
            ((eq? (find-db rwm (car schedule) (cadr schedule)) (location 'get-id)) (cons (car schedule) (cdr schedule)))
            (else (process-schedule (cdr schedule)))))
        (set! schedule (process-schedule schedule))
        ((train 'set-schedule!) schedule)))

    (define (get-schedule id)
      ((hash-ref (rwm-ls rwm) id) 'get-schedule))

    (define (dispatch msg)
        (cond
            ((eq? msg 'get-schedule) get-schedule)
            ((eq? msg 'update) update))
        )
    dispatch)

