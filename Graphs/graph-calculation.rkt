#lang racket

;
; Graph calulation ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require (prefix-in graph: "graph/unweighted/config.rkt"))
(require (prefix-in bft: "graph-algorithms/undirected/bft-applications.rkt"))
(require "../railwaymodel/rwm.rkt")
(require "../Abstractions/Abstractions.rkt")

(provide make-graph-calculation)

(define (make-graph-calculation)
  (let* ([rwm (load-rwm railway)]
         [gs (make-hash)]
         [nodes-hashmap (rwm-ns rwm)]
         [nodes-count (hash-count nodes-hashmap)]
         [tracks-list (rwm-ts rwm)]
         [db-hashmap (rwm-ds rwm)]
         [switches-hashmap (rwm-ss rwm)]
         [railwaygraph (graph:new #f nodes-count)])

    ; Assign each node to a value for the graph
    (define (populate-graph-hashmap)
      (let ([i 0])
        (hash-for-each nodes-hashmap (λ (nid value)
                                       (hash-set! gs nid i)
                                       (set! i (+ i 1))))
        i))

    ; Generate graph from railway model
    (define (generate-graph-from-railway)
      (populate-graph-hashmap)
      (for-each (λ (track) (add-track-to-graph (node-a track) (node-b track))) tracks-list)
      (hash-for-each db-hashmap (λ (did db) (add-track-to-graph (node-a db) (node-b db))))
      (hash-for-each switches-hashmap (λ (sid switch) (add-track-to-graph (node-a switch) (node-b switch)) (add-track-to-graph (node-a switch) (node-c switch)))))


    ; Find the shortest route between two detection blocks
    (define (calculate-shortest-path dA dB)
      (let* ([db-start    (hash-ref (rwm-ds rwm) dA)]
             [db-end      (hash-ref (rwm-ds rwm) dB)]
             [nA   (node-a db-start)]
             [nB   (node-b db-start)]
             [nC   (node-a db-end)]
             [nD   (node-b db-end)]
             [valA  (get-node-value nA)]
             [valC  (get-node-value nC)]
             [path (bft:shortest-path railwaygraph valA valC)])
        (set! path (map
                    (λ (value)
                      (get-node-id value))
                    (reverse (convert-mcons-to-list path))))
        (when (not (eq? (next-node path) nB))
          (set! path (cons nB path)))
        (set! path (reverse path))
        (when (not (eq? (next-node path) nD))
          (set! path (cons nD path)))
        (set! path (reverse path))
        (fix-path path)))


    ; Fix the path so that it is traversable
    (define (fix-path path)
      (let ([fixed-path '()])

        (define (fix-path-iter path)
          (cond
            ((null? path) fixed-path)
            ((> (length path) 2)
             (let* ([nA (current-node path)]
                    [nB (next-node path)]
                    [nC (second-node path)]
                    [trackAB (fetch-track rwm nA nB)]
                    [trackBC (fetch-track rwm nB nC)])

               (define (set-path-to-next-detection-block current-node previous-node)
                 (let ([inner-path '()])

                   (define (find-detection-block current-node previous-node)
                     (hash-for-each
                      (rwm-ns rwm)
                      (λ (nid n)
                        (let ([next-track (fetch-track rwm current-node nid)]
                              [current-track (fetch-track rwm previous-node current-node)])
                          (when (and next-track
                                     (not (track-eqv? current-track next-track)))
                            (cond
                              ((eq? current-track (node-a next-track))
                               (cond
                                 ((detection-block? next-track) (set! fixed-path (cons (node-b next-track) fixed-path)))
                                 (else (set! inner-path (cons (node-b next-track) inner-path))
                                       (set! fixed-path (cons (node-b next-track) fixed-path))
                                       (find-detection-block (node-b next-track) (node-a next-track)))))
                              (else 
                               (cond
                                 ((detection-block? next-track) (set! fixed-path (cons (node-a next-track) fixed-path)))
                                 (else (set! inner-path (cons (node-a next-track) inner-path))
                                       (set! fixed-path (cons (node-a next-track) fixed-path))
                                       (find-detection-block (node-a next-track) (node-b next-track)))))))))))

                   (find-detection-block current-node previous-node)
                   (for-each (λ (n)
                               (set! fixed-path (cons n fixed-path)))
                             (reverse inner-path))))

              
               (if (and
                    (switch? trackAB)
                    (switch? trackBC)
                    (eq? (id trackAB) (id trackBC)))
                   (begin (set! fixed-path (append (list nB nA) fixed-path))
                          (set-path-to-next-detection-block nB nA)
                          (set! fixed-path (append (list nC nB) fixed-path))
                          (fix-path-iter (schedule-rest path)))
                   (begin (set! fixed-path (cons (current-node path) fixed-path))
                          (fix-path-iter (schedule-rest path))))))
            (else (set! fixed-path (cons (current-node path) (schedule-rest fixed-path)))
                  (fix-path-iter (schedule-rest path)))))

        (fix-path-iter path)
        (reverse fixed-path)))


    ; Get node id from graph value
    (define (get-node-id val)
      (let ([res #f])
        (hash-for-each gs (λ (id value)
                            (when (= val value)
                              (set! res id))))
        res))

    ; Get graph value from node id
    (define (get-node-value id)
      (hash-ref gs id (λ () #f)))

    ; Convert mcons list from BFT to a list
    (define (convert-mcons-to-list mconslist)
      (if (null? mconslist)
          '()
          (cons (mcar mconslist) (convert-mcons-to-list (mcdr mconslist)))))

    ; Create graph edge
    (define (add-track-to-graph nA nB)
      (graph:add-edge! railwaygraph (get-node-value nA) (get-node-value nB)))

    
    (define (dispatch msg)
      (cond
        ((eq? msg 'calculate-shortest-path) calculate-shortest-path)
        (else (error "Unknown message ---- GRAPH-CALCULATION"))))

    (generate-graph-from-railway)
    dispatch))


