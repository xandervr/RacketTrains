#lang racket/gui

;
; NMBS ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(provide make-GUI-adv)

(require "../railwaymodel/rwm.rkt")
(require "../Abstractions/Abstractions.rkt")

(define (make-GUI-adv title infrabel NMBS)
  (let* ([rwm       (load-rwm railway)])

    (define (change-color dc color)
      (send dc set-pen
            (send the-pen-list find-or-create-pen
                  color 2 'solid 'round)))
        
    (define (draw-node node canvas dc)
      (let* ([x (get-x node)]
             [y (get-y node)]
             [nid   (id node)]
             [lbl-text   (~a nid)])
        (send dc set-text-foreground "blue")
        (send dc draw-text lbl-text (- x 5) (- y 15))
        (change-color dc "blue")
        (send dc draw-ellipse x y 4 4))) 

    (define (draw-track track canvas dc)
      (let* ([nA  (hash-ref (rwm-ns rwm) (node-a track))]
             [nB  (hash-ref (rwm-ns rwm) (node-b track))]
             [x1  (get-x nA)]
             [y1  (get-y nA)]
             [x2  (get-x nB)]
             [y2  (get-y nB)]
             [middle (find-nodes-middle nA nB)]
             [xm (middle-x middle)]
             [ym (middle-y middle)]
             [max-spd (max-speed track)]
             [track-free? (NMBS-track-free? NMBS (id nA) (id nB))]
             [lbl-text (~a "T (" max-spd "m/s")])
            (cond
          (track-free?
           (change-color dc "orange")  
           (send dc set-text-foreground "darkorange"))
          (else
           (change-color dc "green")  
           (send dc set-text-foreground "darkgreen")))
        (send dc draw-line x1 y1 x2 y2)
        (send dc draw-text lbl-text xm ym)))

    (define (draw-detection-block db canvas dc)
      (let* ([nA  (hash-ref (rwm-ns rwm) (node-a db))]
             [nB  (hash-ref (rwm-ns rwm) (node-b db))]
             [did  (id db)]
             [x1  (get-x nA)]
             [y1  (get-y nA)]
             [x2  (get-x nB)]
             [y2  (get-y nB)]
             [middle (find-nodes-middle nA nB)]
             [xm (middle-x middle)]
             [ym (middle-y middle)]
             [max-spd (max-speed db)]
             [track-free? (NMBS-track-free? NMBS (node-a db) (node-b db))]
             [lbl-text (~a "D: " did " (" max-spd "m/s)")]
             [train  #f])
        (hash-for-each
         (rwm-ls rwm)
         (lambda (tid t)
           (let* ([location  (get-train-location infrabel tid)])
             (when (eq? location did)
               (set! train tid)))))

        (cond 
          (train
           (change-color dc "red")  
           (send dc set-text-foreground "darkred"))
          (track-free?
           (change-color dc "orange")  
           (send dc set-text-foreground "darkorange"))
          (else
           (change-color dc "green")  
           (send dc set-text-foreground "darkgreen")))
        (send dc draw-line x1 y1 x2 y2)
        (send dc draw-text lbl-text (+ xm 3) (+ ym 3))
        (when train
          (let* ([tid train]
                 [spd  (get-train-speed infrabel tid)]
                 [train-lbl-text  (~a tid ": " (abs spd) "m/s")])
            
            (send dc draw-text train-lbl-text (+ xm 3) (+ ym 15))))))

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
             [y4  (get-y nM)]
             [track-free? (NMBS-track-free? NMBS (node-a switch) (node-b switch))])
            (cond
          (track-free?
           (change-color dc "orange")  
           (send dc set-text-foreground "darkorange"))
          (else
           (change-color dc "green")  
           (send dc set-text-foreground "darkgreen")))
        (send dc draw-line x1 y1 x4 y4)
        (if (= pos 1)
            (send dc draw-line x4 y4 x2 y2)
            (send dc draw-line x4 y4 x3 y3))
        (change-color dc "gray")
        (if (= pos 2)
            (send dc draw-line x4 y4 x2 y2)
            (send dc draw-line x4 y4 x3 y3))))

    (define (draw-canvas canvas dc)
      (send dc erase)
      (send canvas set-canvas-background (make-object color% 255 255 255))
      (hash-for-each (rwm-ds rwm) (lambda (id db) (draw-detection-block db canvas dc)))
      (for-each (lambda (t) (draw-track t canvas dc)) (rwm-ts rwm))
      (hash-for-each (rwm-ss rwm) (lambda (id s) (draw-switch s canvas dc)))
      (hash-for-each (rwm-ns rwm) (lambda (id n) (draw-node n canvas dc))))

    (define (schedule-train) ;prompt and process
      (let* ([train-id    (string->symbol (get-text-from-user "Add schedule" "Train ID" #f train-placeholder null))]
             [to-node (string->symbol (get-text-from-user "Add destination" "Destination" #f schedule-placeholder null))])
        (NMBS-drive-to! NMBS train-id to-node)))

     (define (calculate-width)
      (let ((width  0))
        (hash-for-each 
         (rwm-ns rwm) 
         (lambda (nid node) (set! width (max width (get-x node)))))
        width))

    (define (calculate-height)
      (let ((height   0))
        (hash-for-each 
         (rwm-ns rwm) 
         (lambda (nid node) (set! height (max height (get-y node)))))
        height))

    (define (dispatch msg)
      (cond 
        ((eq? msg 'redraw!) (draw-canvas canvas dc))
        (else (error "Unknown message ---- GUI"))))


    ; initialize
    (define width (+ (calculate-width) 100))
    (define height (+ (calculate-height) 100))
    (define frame (new frame% [label title] [width 640] [height 480]))
    (define button-panel (new horizontal-panel% [parent frame]))
    (define canvas-panel (new horizontal-panel% [parent frame]  [min-height 640] [min-width 480]))
    (define canvas (new canvas% [parent canvas-panel] [paint-callback draw-canvas]))
    (define button (new button% [parent button-panel] [label "Schedule a train!"]
                                [callback (lambda (button event) (schedule-train))]))
    (define dc (send canvas get-dc))

    (send frame show #t)

    dispatch))