#lang racket

;
; NMBS ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../railwaymodel/rwm.rkt")
(require "../Abstractions/Abstractions.rkt")
(require "../Graphs/graph-calculation.rkt")

(provide  make-NMBS)

(define (make-NMBS infrabel)
  (let ([rwm (load-rwm railway)]
        [graph-calculation (make-graph-calculation)])

    (define (print-rail-status)
      (for-each
       (λ (track)
         (printf "Track: (~a ~a) ~a\n" (node-a track) (node-b track) (free? track))) (rwm-ts rwm))
      (hash-for-each (rwm-ds rwm)
                     (λ (id ds)
                       (printf "Db: ~a ~a\n" id (free? ds)))))

    (define (update)
      (hash-for-each (rwm-ls rwm) 
                     (λ (id train) 
                       (process-train infrabel train))))

    (define (process-train infrabel train)
      (let ([location (hash-ref (rwm-ds rwm) (get-train-location infrabel (id train)) (λ () #f))])

        (define (occupy-next-track)
          (let* ([schedule (schedule train)]
                 [tf (fetch-track rwm (current-node schedule) (next-node schedule))])

            (define (set-switch switch nA nB)
              (cond
                ((eq? nA (node-a switch))
                 (if (eq? nB (node-b switch))
                     (set-switch-state! infrabel (id switch) 1)
                     (set-switch-state! infrabel (id switch) 2)))
                ((eq? nB (node-a switch))
                 (if (eq? nA (node-b switch))
                     (set-switch-state! infrabel (id switch) 1)
                     (set-switch-state! infrabel (id switch) 2)))
                (else (error "set-switch ---- NMBS"))))

            (when (> (length (schedule-rest schedule)) 1)
              (define (process-next-track schedule free-tracks)
                (let ([t (fetch-track rwm (current-node schedule) (next-node schedule))])
                  (cond
                    ((and t (detection-block? t)) (when (free? t (id train))
                                                    (for-each  
                                                     (λ (t-db) 
                                                       (occupy! t-db (id train)))
                                                     (cons t free-tracks))))
                    (t (when (free? t (id train))
                         (when (switch? t) (set-switch t (current-node schedule) (next-node schedule)))
                         (process-next-track (schedule-rest schedule) (cons t free-tracks))))
                    (else (error "OCCUPYERROR")))))
              (process-next-track (schedule-rest schedule) (list tf)))))

        (define (process-schedule schedule)
          (when (> (length schedule) 1)
            (let ([t (fetch-track rwm (current-node schedule) (next-node schedule))])
              (cond
                (location (cond 
                            ((and t (detection-block? t) (eq? (id location) (id t)))
                             (if (and (> (length schedule) 2) (eq? (current-node schedule) (second-node schedule)))
                                 (set-train-schedule! train (schedule-rest schedule))
                                 (set-train-schedule! train schedule))
                             (occupy-next-track))
                            (else (free! t) (process-schedule (schedule-rest schedule)))))
                (else (cond 
                        ((and t (detection-block? t)) (free! t) (process-schedule (schedule-rest schedule)))
                        (else (set-train-schedule! train schedule))))))))
        (process-schedule (schedule train))))

    (define (add-schedule! train-id schedule)
      (let ([train (hash-ref (rwm-ls rwm) train-id)])
        (if train
            (set-train-schedule! train schedule)
            (error "Train id not found!"))))

    (define (get-schedule id)
      (schedule (hash-ref (rwm-ls rwm) id)))

    (define (track-free? nA nB)
      (let ([t (fetch-track rwm nA nB)])
        (t 'occupied?)))

    ;
    ; GRAPHS
    ;

    (define (drive-to! train-id db-id)
      (let* ([location (hash-ref (rwm-ds rwm) (get-train-location infrabel train-id) (λ () #f))]
             [path ((graph-calculation 'calculate-shortest-path) (id location) db-id)])
        (add-schedule! train-id path)))


    (define (dispatch msg)
      (cond
        ((eq? msg 'drive-to!) drive-to!)
        ((eq? msg 'add-schedule!) add-schedule!)
        ((eq? msg 'get-schedule) get-schedule)
        ((eq? msg 'track-free?) track-free?)
        ((eq? msg 'update) (update))))

    dispatch))

