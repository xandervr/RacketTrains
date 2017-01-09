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
                       (printf "Dt: ~a ~a\n" id (free? ds)))))

    (define (update)
      (hash-for-each (rwm-ls rwm) 
                     (λ (id train) 
                       (process-train infrabel train))))

    (define (process-train infrabel train)
      (let* ([tid (id train)]
             [location (hash-ref (rwm-ds rwm) (get-train-location infrabel tid) (λ () #f))])

        (define (occupy-next-track)
          (let* ([schedule (schedule train)]
                 [tf (fetch-track rwm (current-node schedule) (next-node schedule))]
                 [rest-of-schedule (schedule-rest schedule)])

            (define (set-switch switch nA nB)
              (let ([sA (node-a switch)]
                    [sB (node-b switch)]
                    [sid (id switch)])
                (cond
                  ((eq? nA sA)
                   (if (eq? nB sB)
                       (set-switch-state! infrabel sid 1)
                       (set-switch-state! infrabel sid 2)))
                  ((eq? nB sA)
                   (if (eq? nA sB)
                       (set-switch-state! infrabel sid 1)
                       (set-switch-state! infrabel sid 2)))
                  (else (error "set-switch ---- NMBS")))))

            (when (> (length rest-of-schedule) 1)
              (define (process-next-track schedule free-tracks)
                (let* ([curr-node (current-node schedule)]
                       [nxt-node (next-node schedule)]
                       [t (fetch-track rwm curr-node nxt-node)])
                  (cond
                    ((and t (detection-track? t)) (when (free? t tid)
                                                    (for-each  
                                                     (λ (t-dt) 
                                                       (occupy! t-dt tid))
                                                     (cons t free-tracks))))
                    (t (when (free? t tid)
                         (when (switch? t) (set-switch t curr-node nxt-node))
                         (process-next-track (schedule-rest schedule) (cons t free-tracks))))
                    (else (error "OCCUPYERROR")))))
              (process-next-track rest-of-schedule (list tf)))))

        (define (process-schedule schedule)
          (when (> (length schedule) 1)
            (let* ([curr-node (current-node schedule)]
                   [nxt-node (next-node schedule)]
                   [t (fetch-track rwm curr-node nxt-node)])
              (cond
                (location (cond 
                            ((and t 
                                  (detection-track? t) 
                                  (eq? (id location) (id t)))
                             (if (and 
                                  (> (length schedule) 2) 
                                  (eq? curr-node (second-node schedule)))
                                 (set-train-schedule! train (schedule-rest schedule))
                                 (set-train-schedule! train schedule))
                             (occupy-next-track))
                            (else (free! t) (process-schedule (schedule-rest schedule)))))
                (else (cond 
                        ((and t (detection-track? t)) (free! t) (process-schedule (schedule-rest schedule)))
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

    (define (drive-to-destination! train-id dt-id)
      (let* ([location (hash-ref (rwm-ds rwm) (get-train-location infrabel train-id) (λ () #f))]
             [path ((graph-calculation 'calculate-shortest-path) (id location) dt-id)])
        (add-schedule! train-id path)))


    (define (dispatch msg)
      (cond
        ((eq? msg 'drive-to-destination!) drive-to-destination!)
        ((eq? msg 'add-schedule!) add-schedule!)
        ((eq? msg 'get-schedule) get-schedule)
        ((eq? msg 'track-free?) track-free?)
        ((eq? msg 'exit!) (exit))
        ((eq? msg 'update) (update))))

    dispatch))

