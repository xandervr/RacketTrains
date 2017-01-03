#lang racket

;
; Infrabel ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;
(require "../railwaymodel/rwm.rkt")
(require "../simulator/interface.rkt")

(provide make-infrabel)

(define (make-infrabel)
  (define rwm (load-rwm "railway.txt"))

  

  (define (update NMBS)
    (hash-for-each (rwm-ls rwm) (lambda (id train) (process-train NMBS train))))

  (define (calculate-train-speed NMBS train)
    (define schedule ((NMBS 'get-schedule) (train 'get-id)))

    (define (calculate-track-max-speed)
      (define t (fetch-track rwm (car schedule) (cadr schedule)))
      (let ([max-speed (t 'get-max-speed)])
        (define (calculate-iter schedule)

          (define t (fetch-track rwm (cadr schedule) (caddr schedule)))
          (cond
            ((null? schedule) max-speed)
            ((and t (eq? (t 'get-type) 'detection-block)) (set! max-speed (min max-speed (t 'get-max-speed))) max-speed)
            (t (set! max-speed (min max-speed (t 'get-max-speed))) (calculate-iter (cdr schedule)))
            (else (error "Could't calulate max speed."))))
        (calculate-iter schedule)))

    (let ((location (get-loco-detection-block (train 'get-id))))
      (cond
        ((= (length schedule) 2) 0)
        (else (min (calculate-track-max-speed) (train 'get-max-speed))))))

  (define (process-train NMBS train)
    (define schedule ((NMBS 'get-schedule) (train 'get-id)))
    (if (null? schedule)
      (set-loco-speed! (train 'get-id) 0)
      (set-loco-speed! (train 'get-id) (calculate-train-speed NMBS train))))

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
      ((eq? msg 'get-train-location) get-train-location)
      ((eq? msg 'get-train-speed) get-train-speed)
      ((eq? msg 'get-switch-state) get-switch-state)
      ((eq? msg 'set-switch-state!) set-switch-state!)
      (else (error "Unknown message ---- Infrabel"))))

  (start-simulator)
  dispatch)