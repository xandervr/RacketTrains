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
      (let ([location (hash-ref (rwm-ds rwm) ((infrabel 'get-train-location) (train 'get-id)) (lambda () #f))])

        (define (process-schedule schedule)
          (define dbf (hash-ref (rwm-ds rwm) (find-db rwm (car schedule) (cadr schedule)) (lambda () #f)))
          (define tf (find-track rwm (car schedule) (cadr schedule)))
          (cond
            (location (if (and dbf (eq? (location 'get-id) (dbf 'get-id)))
                        ((train 'set-schedule!) schedule)
                        (process-schedule (cdr schedule))))
            (else (if dbf
                    (process-schedule (cdr schedule))
                    ((train 'set-schedule!) schedule)))))
        (process-schedule (train 'get-schedule))))
      

    (define (get-schedule id)
      ((hash-ref (rwm-ls rwm) id) 'get-schedule))

    (define (dispatch msg)
        (cond
            ((eq? msg 'get-schedule) get-schedule)
            ((eq? msg 'update) update))
        )
    dispatch)

