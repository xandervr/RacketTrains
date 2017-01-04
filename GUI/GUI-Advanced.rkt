#lang racket/gui

(provide make-GUI-adv)

(require "../railwaymodel/rwm.rkt")
(require "../Abstractions/Abstractions.rkt")

(define (make-GUI-adv title infrabel NMBS)
  (let* ((rwm       (load-rwm "railway.txt"))
         (frame     (new frame% [label title] [width 300] [height 300]))
         (btn-panel (new horizontal-panel% [parent frame]))
         (cvs-panel (new horizontal-panel% [parent frame]  [min-height 300] [min-width 300])))

    (define (draw-rwm-status canvas dc)
      (send canvas set-canvas-background (make-object color% 255 255 255))
      (send dc draw-line 0 0 290 290))

    (define (pap-new-route) ;prompt and process
      (let* ((train-id    (string->symbol (get-text-from-user "Add schedule" "Train ID" #f "L1" null)))
             (route (get-text-from-user "Add schedule" "Route" #f "A1 A2 A3 A4 A5 A6 A7 A8 A9" null))
             (schedule (map (lambda (x) (string->symbol x))
                                (string-split route))))
        (insert-schedule! NMBS train-id schedule)))

    (define (dispatch msg)
      (cond 
        ((eq? msg 'redraw!)    (draw-rwm-status canvas dc))
        (else (error "gui:: msg '~a' not understood" msg))))


    ; initialize
    (define button (new button% [parent btn-panel] [label "Schedule a train!"]
                                [callback (lambda (button event) (pap-new-route))]))
    (define canvas (new canvas% [parent cvs-panel] [paint-callback draw-rwm-status]))
    (define dc (send canvas get-dc))

    (send frame show #t)

    dispatch))