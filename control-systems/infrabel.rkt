#lang racket

;
; Infrabel ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;
(require "../railwaymodel/rwm.rkt")
(require "../simulator/interface.rkt")
(require "../Abstractions/Abstractions.rkt")

(provide make-infrabel)

(define (make-infrabel)
  (define rwm (load-rwm railway))

  (define (update NMBS)
    (hash-for-each (rwm-ls rwm) (λ (id train) (process-train NMBS train))))

  (define (get-track-sign dt-id) ; #f = red #t = green
    (let ([sign #t])
      (hash-for-each (rwm-ls rwm)
        (λ (id train)
          (when (eq? (get-train-location id) dt-id)
            (set! sign #f))))
      sign))

  (define (calculate-next-detection-track schedule)
      (let*
        ([curr-node (current-node schedule)]
         [nxt-node (next-node schedule)] 
         [track (fetch-track rwm curr-node nxt-node)]
         [next-nodes (schedule-rest schedule)])
        (cond
          ((null? next-nodes) #f)
          ((detection-track? track) (id track))
          (else (calculate-next-detection-track next-nodes)))))

  (define (calculate-train-speed-direction nA nB)
    (let ([t (fetch-track rwm nA nB)]
          [dir +1])
      (if (eq? (node-a t) nA)
          (set! dir +1)
          (set! dir -1))
      dir))

  (define (calculate-train-speed NMBS train)
    (let* ([schedule (get-train-schedule NMBS (id train))]
          [location (get-loco-detection-block (id train))]
          [curr-node (current-node schedule)]
          [nxt-node (next-node schedule)]
          [current-track (fetch-track rwm curr-node nxt-node)]
          [rest-of-schedule (schedule-rest schedule)])

      (define (calculate-track-max-speed)
        (let* ([t (fetch-track rwm (current-node schedule) (next-node schedule))]
               [max-spd (max-speed t)])
          
          (define (calculate-iter schedule)
            (let ([t (fetch-track rwm (next-node schedule) (second-node schedule))])
              (cond
                ((null? schedule) max-spd)
                ((and t (detection-track? t)) (set! max-spd (min max-spd (max-speed t))) max-spd)
                (t (set! max-spd (min max-spd (max-speed t))) (calculate-iter (schedule-rest schedule)))
                (else (error "Could't calulate max speed.")))))

          (define (calculate-swith-position)
            (let* ([nA (next-node schedule)]
                   [nB (second-node schedule)]
                   [t (fetch-track rwm nA nB)]
                   [tA (node-a t)]
                   [tB (node-b t)])

              (define (find-right-switch-position)
                (if (and (eq? tA nA) (eq? tB nB))
                  (if (= (get-switch-state (id t)) 1) 1 2)
                  (if (= (get-switch-state (id t)) 2) 2 1)))

              (when (and t (switch? t)) (set-switch-state! (id t) (find-right-switch-position)))))
          
          (calculate-swith-position)
          (calculate-iter schedule)))

      (cond
        ((<= (length schedule) 2) 0)
        ((and (detection-track? current-track) (not (get-track-sign (calculate-next-detection-track rest-of-schedule)))) 0)
        (else (* (calculate-train-speed-direction curr-node nxt-node) (min (calculate-track-max-speed) (max-speed train)))))))

  (define (process-train NMBS train)
    (let* ([tid (id train)]
           [schedule (get-train-schedule NMBS tid)])
      (if (null? schedule)
          (set-loco-speed! tid 0)
          (set-loco-speed! tid (calculate-train-speed NMBS train)))))

  (define (get-train-location id)
    (get-loco-detection-block id))

  (define (get-train-speed id)
    (get-loco-speed id))

  (define (get-switch-state id)
    (get-switch-position id))

  (define (set-switch-state! id pos)
    (set-switch-position! id pos))

  (define (dispatch msg)
    (cond
      ((eq? msg 'update)  update)
      ((eq? msg 'get-track-sign) get-track-sign)
      ((eq? msg 'get-train-location) get-train-location)
      ((eq? msg 'get-train-speed) get-train-speed)
      ((eq? msg 'get-switch-state) get-switch-state)
      ((eq? msg 'set-switch-state!) set-switch-state!)
      (else (error "Unknown message ---- Infrabel"))))

  (start-simulator)
  dispatch)