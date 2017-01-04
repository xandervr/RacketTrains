#lang racket/gui

;
; NMBS ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-GUI-adv)

(require "../railwaymodel/rwm.rkt")
(require "../Abstractions/Abstractions.rkt")

(define (make-GUI-adv title infrabel NMBS)
  (let* ((rwm       (load-rwm railway))
         (frame     (new frame% [label title] [width 640] [height 480]))
         (button-panel (new horizontal-panel% [parent frame]))
         (canvas-panel (new horizontal-panel% [parent frame]  [min-height 640] [min-width 480])))

    (define (draw-track track canvas dc)
      (let* ([nA  (hash-ref (rwm-ns rwm) (node-a track))]
             [nB  (hash-ref (rwm-ns rwm) (node-b track))]
             [x1  (get-x nA)]
             [y1  (get-y nA)]
             [x2  (get-x nB)]
             [y2  (get-y nB)])
            (send dc draw-line x1 y1 x2 y2)))

    (define (draw-detection-block db canvas dc)
      (let* ([nA  (hash-ref (rwm-ns rwm) (node-a db))]
             [nB  (hash-ref (rwm-ns rwm) (node-b db))]
             [did  (id db)]
             [x1  (get-x nA)]
             [y1  (get-y nA)]
             [x2  (get-x nB)]
             [y2  (get-y nB)]
             [loco  #f])
        (hash-for-each
         (rwm-ls rwm)
         (lambda (lid l)
           (let* ([location  (get-train-location infrabel lid)]
                  [speed     (get-train-speed infrabel lid)]
                  [obj-fail  (lambda () (lambda (x) (lambda (x) #f)))]
                  [db        (hash-ref (rwm-ds rwm) location obj-fail)]
                  [max-spd (max-speed (track db))]
                  [msg       (~a "Train id: " lid ", location: " location
                                 ", speed: " speed " max speed: " max-spd)])
             (when (eq? location did)
               (set! loco lid)))))
        (when loco
          (send dc set-pen
                (send the-pen-list find-or-create-pen
                      "red" 2 'solid 'round)))
        (send dc draw-line x1 y1 x2 y2)
        (send dc set-pen
              (send the-pen-list find-or-create-pen
                    "black" 2 'solid 'round))))

    (define (draw-switch switch canvas dc)
      (let* ([nA  (hash-ref (rwm-ns rwm) (node-a switch))]
             [nB  (hash-ref (rwm-ns rwm) (node-b switch))]
             [nC  (hash-ref (rwm-ns rwm) (node-c switch))]
             [sid (id switch)]
             [nM  (hash-ref (rwm-ns rwm) sid)]
             [pos (get-switch-state infrabel sid)]
             [x1  (get-x nA)]
             [y1  (get-y nA)]
             [x2  (get-x nB)]
             [y2  (get-y nB)]
             [x3  (get-x nC)]
             [y3  (get-y nC)]
             [x4  (get-x nM)]
             [y4  (get-y nM)])
            (send dc draw-line x1 y1 x4 y4)
            (if (= pos 1)
              (send dc draw-line x4 y4 x2 y2)
              (send dc draw-line x4 y4 x3 y3))))

    (define (draw-canvas canvas dc)
      (send dc erase)
      (send canvas set-canvas-background (make-object color% 255 255 255))
      (hash-for-each (rwm-ds rwm) (lambda (id db) (draw-detection-block db canvas dc)))
      (for-each (lambda (t) (draw-track t canvas dc)) (rwm-ts rwm))
      (hash-for-each (rwm-ss rwm) (lambda (id s) (draw-switch s canvas dc))))

    (define (schedule-train) ;prompt and process
      (let* ([train-id    (string->symbol (get-text-from-user "Add schedule" "Train ID" #f train-placeholder null))]
             [to-node (string->symbol (get-text-from-user "Add destination" "Destination" #f schedule-placeholder null))])
        (NMBS-drive-to! NMBS train-id to-node)))

    (define (dispatch msg)
      (cond 
        ((eq? msg 'redraw!) (draw-canvas canvas dc))
        (else (error "gui:: msg '~a' not understood" msg))))


    ; initialize
    (define button (new button% [parent button-panel] [label "Schedule a train!"]
                                [callback (lambda (button event) (schedule-train))]))
    (define canvas (new canvas% [parent canvas-panel] [paint-callback draw-canvas]))
    (define dc (send canvas get-dc))

    (send frame show #t)

    dispatch))