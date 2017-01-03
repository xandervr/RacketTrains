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
          (when (> (length schedule) 1)
          (define dbf (hash-ref (rwm-ds rwm) (find-db rwm (car schedule) (cadr schedule)) (lambda () #f)))
          (define tf (find-track rwm (car schedule) (cadr schedule)))
          (cond
            (location (if (and dbf (eq? (location 'get-id) (dbf 'get-id)))
                        ((train 'set-schedule!) schedule)
                        (process-schedule (cdr schedule))))
            (else (if dbf
                    (process-schedule (cdr schedule))
                    ((train 'set-schedule!) schedule))))))
        (process-schedule (train 'get-schedule))))
      
    (define (add-schedule! train-id schedule)
      (let ([train (hash-ref (rwm-ls rwm) train-id)])
        (if train
          ((train 'set-schedule!) schedule)
          (error "Train id not found!"))))

    (define (get-schedule id)
      ((hash-ref (rwm-ls rwm) id) 'get-schedule))

    (define (dispatch msg)
        (cond
            ((eq? msg 'add-schedule!) add-schedule!)
            ((eq? msg 'get-schedule) get-schedule)
            ((eq? msg 'update) update)))

    (add-schedule! 'T1 '(A1 A2 A3 A4 A5 A6))

    dispatch)

