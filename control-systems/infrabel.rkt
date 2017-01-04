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
    (hash-for-each (rwm-ls rwm) (lambda (id train) (process-train NMBS train))))

  (define (get-track-sign db-id) ; #f = red #t = green
    (let ([sign #t])
      (hash-for-each (rwm-ls rwm)
        (lambda (id train)
          (when (eq? (get-train-location id) db-id)
            (set! sign #f))))
      sign))

  (define (find-next-db schedule)
      (let 
        ([track (fetch-track rwm (current-node schedule) (next-node schedule))])
        (cond
          ((null? (schedule-rest schedule)) #f)
          ((detection-block? track) (id track))
          (else (find-next-db (schedule-rest schedule))))))

  (define (calculate-train-speed-direction nA nB)
    (let ([t (fetch-track rwm nA nB)])
      (if (eq? (node-a t) nA)
          +1
          -1)))

  (define (calculate-train-speed NMBS train)
    (let ([schedule (get-train-schedule NMBS (id train))]
          [location (get-loco-detection-block (id train))])

      (define (calculate-track-max-speed)
        (let* ([t (fetch-track rwm (current-node schedule) (next-node schedule))]
               [max-spd (max-speed t)])
          
          (define (calculate-iter schedule)
            (let ([t (fetch-track rwm (next-node schedule) (second-node schedule))])
              (cond
                ((null? schedule) max-spd)
                ((and t (detection-block? t)) (set! max-spd (min max-spd (max-speed t))) max-spd)
                (t (set! max-spd (min max-spd (max-speed t))) (calculate-iter (schedule-rest schedule)))
                (else (error "Could't calulate max speed.")))))
          
          (calculate-iter schedule)))

      (cond
        ((<= (length schedule) 2) 0)
        ((and (detection-block? (fetch-track rwm (current-node schedule) (next-node schedule))) (not (get-track-sign (find-next-db (schedule-rest schedule))))) 0)
        (else (* (calculate-train-speed-direction (current-node schedule) (next-node schedule)) (min (calculate-track-max-speed) (max-speed train)))))))

  (define (process-train NMBS train)
    (let ([schedule (get-train-schedule NMBS (id train))])
      (if (null? schedule)
          (set-loco-speed! (id train) 0)
          (set-loco-speed! (id train) (calculate-train-speed NMBS train)))))

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