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

    (define (set-pen-color dc color)
      (send dc set-smoothing 'smoothed)
      (if (eq? color "translucent") (send dc set-alpha 0) (send dc set-alpha 1))
      (send dc set-pen
            (send the-pen-list find-or-create-pen
                  color 4 'solid 'round)))
        
    (define (draw-node node canvas dc)
      (let* ([x (get-x node)]
             [y (get-y node)]
             [nid   (id node)]
             [lbl-text   (~a nid)])
        (send dc set-text-foreground "blue")
        (send dc draw-text lbl-text (- x 18) (- y 18))
        (set-pen-color dc "blue")
        (send dc draw-ellipse (- x 4) (- y 4) 8 8))) 

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
             [lbl-text (~a "T (" max-spd " m/s)")])
            (cond
          (track-free?
           (set-pen-color dc "cyan")  
           (send dc set-text-foreground "darkcyan"))
          (else
           (set-pen-color dc "black")  
           (send dc set-text-foreground "white")))
        (send dc draw-line x1 y1 x2 y2)
        (send dc draw-text lbl-text (+ xm 5) (+ ym 5))))

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
             [lbl-text (~a "D: " did " (" max-spd " m/s)")]
             [train  #f])
        (hash-for-each
         (rwm-ls rwm)
         (λ (tid t)
           (let* ([location  (get-train-location infrabel tid)])
             (when (eq? location did)
               (set! train tid)))))

        (cond 
          (train
           (set-pen-color dc "red")  
           (send dc set-text-foreground "darkred"))
          (track-free?
           (set-pen-color dc "cyan")  
           (send dc set-text-foreground "darkcyan"))
          (else
           (set-pen-color dc "black")  
           (send dc set-text-foreground "white")))
        (send dc draw-line x1 y1 x2 y2)
        (send dc draw-text lbl-text (+ xm 6) (+ ym 6))
        (when train
          (let* ([tid train]
                 [spd  (get-train-speed infrabel tid)])
            (send dc draw-bitmap bmp-train (- xm 50) (- ym 30))
            (send label-train-speed set-label (~a "Train speed: " "(" tid ") " (abs spd) " m/s"))))))

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
           (set-pen-color dc "cyan")  
           (send dc set-text-foreground "darkcyan"))
          (else
           (set-pen-color dc "black")  
           (send dc set-text-foreground "white")))
        (send dc draw-line x1 y1 x4 y4)
        (if (= pos 1)
            (send dc draw-line x4 y4 x2 y2)
            (send dc draw-line x4 y4 x3 y3))
        (set-pen-color dc "translucent")
        (if (= pos 2)
            (send dc draw-line x4 y4 x2 y2)
            (send dc draw-line x4 y4 x3 y3))))

    (define (draw-canvas canvas dc)
      (send dc erase)
      (send dc draw-bitmap background 0 0)
      (hash-for-each (rwm-ds rwm) (λ (id db) (draw-detection-block db canvas dc)))
      (for-each (λ (t) (draw-track t canvas dc)) (rwm-ts rwm))
      (hash-for-each (rwm-ss rwm) (λ (id s) (draw-switch s canvas dc)))
      (hash-for-each (rwm-ns rwm) (λ (id n) (draw-node n canvas dc))))

    (define (schedule-train) ;prompt and process
      (let* ([train-id    (string->symbol (get-text-from-user "Add schedule" "Train ID" #f train-placeholder null))]
             [to-node (string->symbol (get-text-from-user "Add destination" "Destination" #f schedule-placeholder null))])
        (NMBS-drive-to! NMBS train-id to-node)))

     (define (calculate-width)
      (let ((width  0))
        (hash-for-each 
         (rwm-ns rwm) 
         (λ (nid node) (set! width (max width (get-x node)))))
        width))

    (define (calculate-height)
      (let ((height   0))
        (hash-for-each 
         (rwm-ns rwm) 
         (λ (nid node) (set! height (max height (get-y node)))))
        height))

    (define (dispatch msg)
      (cond 
        ((eq? msg 'redraw!) (draw-canvas canvas dc))
        (else (error "Unknown message ---- GUI"))))

    (define bitmap-canvas%
      (class canvas%
        (init-field [bitmap #f])
        (inherit get-dc)
        (define/override (on-paint)
          (send (get-dc) draw-bitmap bitmap 0 0))
        (super-new)))

    ; initialize
    (define width (+ (calculate-width) 100))
    (define height (+ (calculate-height) 50))
    (define frame (new frame% [label title]))
    (define background (read-bitmap "Resources/stones.bmp"))
    (define bmp-train (read-bitmap "Resources/train.bmp"))
    (define canvas-panel (new horizontal-panel% [parent frame]  [min-height height] [min-width width]))
    (define button-panel (new horizontal-panel% [parent frame]))
    (define canvas (new bitmap-canvas% [parent canvas-panel] [paint-callback draw-canvas] [bitmap background]))
    (define button (new button% [parent button-panel] [label "Schedule a train!"]
                                [callback (λ (button event) (schedule-train))]))
    (define label-train-speed (new message% [parent button-panel] [label "Train speed: #f 0 m/s"] [min-width (- width 150)]))
    (define dc (send canvas get-dc))

    (send frame show #t)

    dispatch))