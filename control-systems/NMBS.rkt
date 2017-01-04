#lang racket

;
; NMBS ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../railwaymodel/rwm.rkt")
(require "../Abstractions/Abstractions.rkt")

(provide  make-NMBS)

(define (make-NMBS)
  (define rwm (load-rwm "railway.txt"))

  (define (print-rail-status)
    (for-each
     (lambda (track)
       (printf "Track: (~a ~a) ~a\n" (track 'get-nodeA) (track 'get-nodeB) ((track 'free?)))) (rwm-ts rwm))
    (hash-for-each (rwm-ds rwm)
                   (lambda (id ds)
                     (printf "Db: ~a ~a\n" id ((ds 'free?))))))

  (define (update infrabel)
    ;(print-rail-status)
    (hash-for-each (rwm-ls rwm) 
                   (lambda (id train) 
                     (process-train infrabel train))))

  (define (process-train infrabel train)
    (let ([location (hash-ref (rwm-ds rwm) ((infrabel 'get-train-location) (train 'get-id)) (lambda () #f))])

      (define (occupy-next-track)
        (let* ([schedule (train 'get-schedule)]
               [tf (fetch-track rwm (current-node schedule) (next-node schedule))])
          (define (set-switch switch nA nB)
            (cond
              ((eq? nA (switch 'get-nodeA))
               (if (eq? nB (switch 'get-nodeB))
                   ((infrabel 'set-switch-state!) (switch 'get-id) 1)
                   ((infrabel 'set-switch-state!) (switch 'get-id) 2)))
              ((eq? nB (switch 'get-nodeA))
               (if (eq? nA (switch 'get-nodeB))
                   ((infrabel 'set-switch-state!) (switch 'get-id) 1)
                   ((infrabel 'set-switch-state!) (switch 'get-id) 2)))
              (else (error "set-switch ---- NMBS"))))

          (when (> (length (cdr schedule)) 1)
            (define (process-next-track schedule free-tracks)
              (let ([t (fetch-track rwm (current-node schedule) (next-node schedule))])
                (cond
                  ((and t (eq? (t 'get-type) 'detection-block)) (when ((t 'free?) (train 'get-id))
                                                                  (for-each  
                                                                   (lambda (t-db) 
                                                                     ((t-db 'occupy!) (train 'get-id)))
                                                                   (cons t free-tracks))))
                  (t (when ((t 'free?) (train 'get-id))
                       (when (eq? (t 'get-type) 'switch) (set-switch t (current-node schedule) (next-node schedule)))
                       (process-next-track (schedule-rest schedule) (cons t free-tracks))))
                  (else (error "OCCUPYERROR")))))
            (process-next-track (schedule-rest schedule) (list tf)))))

      (define (process-schedule schedule)
        (when (> (length schedule) 1)
          (let ([t (fetch-track rwm (current-node schedule) (next-node schedule))])
            (cond
              (location (cond 
                          ((and t (eq? (t 'get-type) 'detection-block) (eq? (location 'get-id) (t 'get-id))) ((train 'set-schedule!) schedule) (occupy-next-track))
                          (else (t 'free!) (process-schedule (schedule-rest schedule)))))
              (else (cond 
                      ((and t (eq? (t 'get-type) 'detection-block)) (t 'free!) (process-schedule (schedule-rest schedule)))
                      (else ((train 'set-schedule!) schedule))))))))
      (process-schedule (train 'get-schedule))))

  (define (add-schedule! train-id schedule)
    (let ([train (hash-ref (rwm-ls rwm) train-id)])
      (if train
          ((train 'set-schedule!) schedule)
          (error "Train id not found!"))))

  (define (get-schedule id)
    ((hash-ref (rwm-ls rwm) id) 'get-schedule))

  (define (track-free? nA nB)
    (let ([t (fetch-track rwm nA nB)])
      (t 'occupied?)))

  (define (dispatch msg)
    (cond
      ((eq? msg 'add-schedule!) add-schedule!)
      ((eq? msg 'get-schedule) get-schedule)
      ((eq? msg 'track-free?) track-free?)
      ((eq? msg 'update) update)))

  dispatch)

