#lang racket/gui

;
; GUI ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../railwaymodel/rwm.rkt")

(provide make-GUI)

(define (make-GUI title infrabel NMBS)
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
                            ((NMBS 'add-schedule!) train-id schedule)
                            (send schedule-msg set-label (~a schedule))))]))
        (set! labels (cons label labels))
        (set! panels (cons new-panel panels))))


    (hash-for-each
     (rwm-ls rwm)
     (lambda (id train)
       (add-label!
        (~a (train 'get-id) ": " ((infrabel 'get-train-location) (train 'get-id)))
        id)))
    (for-each
     (lambda (track)
       (add-label!
        (~a "(" (track 'get-nodeA) ", " (track 'get-nodeB) "): " ((NMBS 'track-free?) (track 'get-nodeA) (track 'get-nodeB)))))
     (rwm-ts rwm))
    (hash-for-each
     (rwm-ds rwm)
     (lambda (id ds)
       (add-label!
        (~a (ds 'get-id) ": " ((NMBS 'track-free?) ((ds 'get-track) 'get-nodeA) ((ds 'get-track) 'get-nodeB))))))
    (hash-for-each
     (rwm-ss rwm)
     (lambda (id ss)
       (add-label!
        (~a (ss 'get-id) ": " ((infrabel 'get-switch-state) (ss 'get-id))))))

    (set! labels (reverse labels))

    (send frame show #t)


    (define (redraw!)
      (let ([i 0])
        (hash-for-each
         (rwm-ls rwm)
         (lambda (id train)
           (send (list-ref labels i)
                 set-label
                 (~a (train 'get-id) ": " ((infrabel 'get-train-location) (train 'get-id)) " Speed: " ((infrabel 'get-train-speed) (train 'get-id)) " Max-speed: "
                     (((hash-ref (rwm-ds rwm) ((infrabel 'get-train-location) (train 'get-id))
                                 (lambda ()
                                   (lambda (x)
                                     (lambda (x) #f)))) 'get-track) 'get-max-speed)))
           (set! i (+ i 1))))
        (for-each
         (lambda (track)
           (send (list-ref labels i) set-label
                 (~a "T (" (track 'get-nodeA) ", " (track 'get-nodeB) "): " ((NMBS 'track-free?) (track 'get-nodeA) (track 'get-nodeB)) " Max-speed: " (track 'get-max-speed)))
           (set! i (+ i 1)))
         (rwm-ts rwm))
        (hash-for-each
         (rwm-ds rwm)
         (lambda (id db)
           (send (list-ref labels i) set-label
                 (~a (db 'get-id) " (" (db 'get-nodeA) ", " (db 'get-nodeB) "): " ((NMBS 'track-free?) ((db 'get-track) 'get-nodeA) ((db 'get-track) 'get-nodeB)) " Max-speed: " ((db 'get-track) 'get-max-speed) " Sign: " ((infrabel 'get-track-sign) (db 'get-id))))
           (set! i (+ i 1))))
        (hash-for-each
         (rwm-ss rwm)
         (lambda (id ss)
           (send (list-ref labels i) set-label
                 (~a (ss 'get-id) " (" (ss 'get-nodeA) ", " (ss 'get-nodeB) ", " (ss 'get-nodeC) "): " ((NMBS 'track-free?) (ss 'get-nodeA) (ss 'get-nodeB)) " State: " ((infrabel 'get-switch-state) (ss 'get-id)) " Max-speed: " (ss 'get-max-speed)))
           (set! i (+ i 1))))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'redraw!) (redraw!))
        (else (error "Unknown message ---- GUI"))))
    dispatch))