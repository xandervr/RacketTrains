#lang racket/gui

;
; GUI ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../railwaymodel/rwm.rkt")
(require "../Abstractions/Abstractions.rkt")

(provide make-GUI-log)

(define (make-GUI-log title infrabel NMBS)
  (let* 
      ([frame (new frame% [label title])]
       [panel (new horizontal-panel% [parent frame])]
       [rwm (load-rwm "railway.txt")]
       [labels '()]
       [panels '()])

    (define (add-label! text [train-id #f] [min-width 400])
      (let* 
          ([new-panel (new horizontal-panel% [parent frame])]
           [label (new message% [parent new-panel]
                       [label text]
                       [min-width min-width])])
        (when train-id
          (define schedule-msg (new message% [parent new-panel]
                                              [label ""]
                                              [min-width 200]))
          (new button% [parent new-panel]
               [label "Load"]
               [callback (lambda (button event)
                           (let ([schedule (map (lambda (x) (string->symbol x)) (string-split (get-text-from-user "Load schedule" "Schedule:" #f "A1 A2 A3 A4 A5 A6 A7 A8 A9" null)))])
                            (insert-schedule! NMBS train-id schedule)
                            (send schedule-msg set-label (~a schedule))))]))
        (set! labels (cons label labels))
        (set! panels (cons new-panel panels))))


    (hash-for-each
     (rwm-ls rwm)
     (lambda (tid train)
       (add-label!
        (~a tid ": " (get-train-location infrabel tid))
        tid)))
    (for-each
     (lambda (track)
       (add-label!
        (~a "(" (node-a track) ", " (node-b track) "): " ((NMBS 'track-free?) (node-a track) (node-b track)))))
     (rwm-ts rwm))
    (hash-for-each
     (rwm-ds rwm)
     (lambda (did db)
       (add-label!
        (~a did ": " ((NMBS 'track-free?) (node-a (track db)) (node-b (track db)))))))
    (hash-for-each
     (rwm-ss rwm)
     (lambda (sid ss)
       (add-label!
        (~a sid ": " (get-switch-state infrabel sid)))))

    (set! labels (reverse labels))

    (send frame show #t)


    (define (redraw!)
      (let ([i 0])
        (hash-for-each
         (rwm-ls rwm)
         (lambda (tid train)
           (send (list-ref labels i)
                 set-label
                 (~a tid ": " (get-train-location infrabel tid) " Speed: " (get-train-speed infrabel tid) " Max-speed: "
                     (max-speed (track (hash-ref (rwm-ds rwm) (get-train-speed infrabel tid)
                                 (lambda ()
                                   (lambda (x)
                                     (lambda (x) #f))))))))
           (set! i (+ i 1))))
        (for-each
         (lambda (track)
           (send (list-ref labels i) set-label
                 (~a "T (" (node-a track) ", " (node-b track) "): " ((NMBS 'track-free?) (node-a track) (node-b track)) " Max-speed: " (max-speed track)))
           (set! i (+ i 1)))
         (rwm-ts rwm))
        (hash-for-each
         (rwm-ds rwm)
         (lambda (did db)
           (send (list-ref labels i) set-label
                 (~a did " (" (node-a db) ", " (node-b db) "): " ((NMBS 'track-free?) (node-a (track db)) (node-b (track db))) " Max-speed: " (max-speed (track db)) " Sign: " ((infrabel 'get-track-sign) did)))
           (set! i (+ i 1))))
        (hash-for-each
         (rwm-ss rwm)
         (lambda (sid ss)
           (send (list-ref labels i) set-label
                 (~a sid " (" (node-a ss) ", " (node-b ss) ", " (node-c ss) "): " ((NMBS 'track-free?) (node-a ss) (node-b ss)) " State: " (get-switch-state infrabel sid) " Max-speed: " (max-speed ss)))
           (set! i (+ i 1))))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'redraw!) (redraw!))
        (else (error "Unknown message ---- GUI"))))
    dispatch))