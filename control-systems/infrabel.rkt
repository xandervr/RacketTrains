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

  (start-simulator)

  (define (update NMBS)
    (hash-for-each (rwm-ls rwm) (lambda (id train) (process-train NMBS train))))

  (define (calculate-train-speed NMBS train)
    (define schedule ((NMBS 'get-schedule) (train 'get-id)))

    (define (calculate-track-max-speed)
      (define dbf (hash-ref (rwm-ds rwm) (find-db rwm (car schedule) (cadr schedule)) (lambda () #f)))
      (define tf (find-track rwm (car schedule) (cadr schedule)))
      (let ([max-speed (if dbf ((dbf 'get-track) 'get-max-speed) (tf 'get-max-speed))])
        (define (calculate-iter schedule)
          (define db (find-db rwm (cadr schedule) (caddr schedule)))
          (define t (find-track rwm (cadr schedule) (caddr schedule)))
          (cond
            ((null? schedule) max-speed)
            (db (set! db (hash-ref (rwm-ds rwm) db (lambda () #f))) (set! max-speed (min max-speed ((db 'get-track) 'get-max-speed))) max-speed)
            (t (set! max-speed (min max-speed (t 'get-max-speed))) (calculate-iter (cdr schedule)))
            (else (error "Could't calulate max speed."))))
        (calculate-iter schedule)))

    ;
    ; DEBUGGING
    ;
    ; (define ds (hash-ref (rwm-ds rwm) (get-loco-detection-block (train 'get-id)) (lambda () #f)))
    ; (if ds 
    ;   (printf "Loco speed: ~a, Location: ~a, Max speed: ~a\n" (get-loco-speed (train 'get-id)) (get-loco-detection-block (train 'get-id)) ((ds 'get-track) 'get-max-speed))
    ;   (printf "Loco speed: ~a, Location: ~a, Max speed: ~a\n" (get-loco-speed (train 'get-id)) (get-loco-detection-block (train 'get-id)) #f))
    
    ;;;; END


    (let ((location (get-loco-detection-block (train 'get-id))))
      (cond
        ; TODO
        ; Track max-speed
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

  (define (dispatch msg)
    (cond
      ((eq? msg 'update)  update)
      ((eq? msg 'get-train-location) get-train-location)
      ((eq? msg 'get-train-speed) get-train-speed)
      (else (error "Unknown message ---- Infrabel"))))



  dispatch)