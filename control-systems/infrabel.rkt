#lang racket

(require "../railwaymodel/rwm.rkt")
(require "../simulator/interface.rkt")

(provide make-infrabel)

(define (make-infrabel)
  (define rwm (load-rwm "railway.txt"))

  (start-simulator)
  (set-loco-speed! '|1| 1)

  (define (update NMBS)
    (hash-for-each (rwm-ls rwm) (lambda (id train) (process-train NMBS train))))

  (define (calculate-train-speed NMBS train)
    (define schedule (list 'D2 'D3 'D4))
    (printf "Loco speed: ~a, Location: ~a\n" (get-loco-speed (train 'get-id)) (get-loco-detection-block (train 'get-id)))
    (let ((location (get-loco-detection-block (train 'get-id))))
      (cond
        ((eq? location (car (reverse schedule))) 0)
        (else (train 'get-max-speed)))))

  (define (process-train NMBS train)
    ;(define schedule ((NMBS 'get-schedule) (train 'get-id)))
    (define schedule (list 'D2 'D3 'D4))

    (if (null? schedule)
      (set-loco-speed! (train 'get-id) 0)
      (set-loco-speed! (train 'get-id) (calculate-train-speed NMBS train))))


  (define (dispatch msg)
    (cond
      ((eq? msg 'update)  update)
      (else (error "msg not understood"))))



  dispatch)