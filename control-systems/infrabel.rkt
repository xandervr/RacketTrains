#lang racket

(require "../railwaymodel/rwm.rkt")
(require "../simulator/interface.rkt")

(provide make-infrabel)

(define (make-infrabel)
  (define rwm (load-rwm "railway.txt"))

  (start-simulator)
  (set-loco-speed! '|1| 1)
  (sleep 3)
  (printf "Detection block: ~a" (get-loco-detection-block '|1|))
  (newline)
  (printf "locomotive speed: ~a" (get-loco-speed '|1|))

  (define (update-train train)
    (define (decide-train-speed train)
      #f)

    #f)

  (define (update Z21)
    (for-each update-train (rwm 'get-trains)))

  (define (get-location id)
    (printf "Detection block: ~a" (get-loco-detection-block id)))


  (define (dispatch msg)
    (cond
      ((eq? msg 'update)    update)
      ((eq? msg 'get-location) get-location)
      (else                 (error "msg not understood"))))



  dispatch)