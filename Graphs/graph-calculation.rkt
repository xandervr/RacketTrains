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
      (for-each (λ (track) 
                  (let ([nA (node-a track)]
                        [nB (node-b track)])
                    (add-track-to-graph nA nB))) tracks-list)
      (hash-for-each db-hashmap (λ (did dt) 
                                  (let ([nA (node-a dt)]
                                        [nB (node-b dt)])
                                    (add-track-to-graph nA nB))))
      (hash-for-each switches-hashmap (λ (sid switch) 
                                        (let ([nA (node-a switch)]
                                              [nB (node-b switch)]
                                              [nC (node-c switch)])
                                          (add-track-to-graph nA nB) 
                                          (add-track-to-graph nA nC)))))


    ; Find the shortest route between two detection blocks
    (define (calculate-shortest-path dA dB)
      (let* ([dt-start    (hash-ref (rwm-ds rwm) dA)]
             [dt-end      (hash-ref (rwm-ds rwm) dB)]
             [nA-start   (node-a dt-start)]
             [nB-start  (node-b dt-start)]
             [nA-end   (node-a dt-end)]
             [nB-end   (node-b dt-end)]
             [valA-start  (get-node-value nA-start)]
             [valA-end  (get-node-value nA-end)]
             [path (bft:shortest-path railwaygraph valA-start valA-end)])
        (set! path (map
                    (λ (value)
                      (get-node-id value))
                    (reverse (convert-mcons-to-list path))))
        (when (not (eq? (next-node path) nB-start))
          (set! path (cons nB-start path)))
        (set! path (reverse path))
        (when (not (eq? (next-node path) nB-end))
          (set! path (cons nB-end path)))
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

               (define (set-path-to-next-detection-track current-node previous-node)
                 (let ([inner-path '()])

                   (define (find-detection-track current-node previous-node)
                     (hash-for-each
                      (rwm-ns rwm)
                      (λ (nid n)
                        (let* ([next-track (fetch-track rwm current-node nid)]
                               [next-track-nA #f]
                               [next-track-nB #f]
                               [current-track (fetch-track rwm previous-node current-node)]
                               [current-track-nA (node-a current-track)])
                          (when (and next-track
                                     (not (track-eqv? current-track next-track)))
                            (set! next-track-nA (node-a next-track))
                            (set! next-track-nB (node-b next-track))
                            (cond
                              ((eq? current-track-nA next-track-nA)
                               (cond
                                 ((detection-track? next-track) (set! fixed-path (cons next-track-nB fixed-path)))
                                 (else (set! inner-path (cons next-track-nB inner-path))
                                       (set! fixed-path (cons next-track-nB fixed-path))
                                       (find-detection-track next-track-nB next-track-nA))))
                              (else 
                               (cond
                                 ((detection-track? next-track) (set! fixed-path (cons next-track-nA fixed-path)))
                                 (else (set! inner-path (cons next-track-nA inner-path))
                                       (set! fixed-path (cons next-track-nA fixed-path))
                                       (find-detection-track next-track-nA next-track-nB))))))))))

                   (find-detection-track current-node previous-node)
                   (for-each (λ (n)
                               (set! fixed-path (cons n fixed-path)))
                             (reverse inner-path))))

              
               (if (and
                    (switch? trackAB)
                    (switch? trackBC)
                    (eq? (id trackAB) (id trackBC)))
                   (begin (set! fixed-path (append (list nB nA) fixed-path))
                          (set-path-to-next-detection-track nB nA)
                          (set! fixed-path (append (list nC nB) fixed-path))
                          (fix-path-iter (schedule-rest path)))
                   (begin (set! fixed-path (cons (current-node path) fixed-path))
                          (fix-path-iter (schedule-rest path))))))
            (else (set! fixed-path (cons (current-node path) fixed-path))
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
      (let ([valA (get-node-value nA)]
            [valB (get-node-value nB)])
        (graph:add-edge! railwaygraph valA valB)))

    
    (define (dispatch msg)
      (cond
        ((eq? msg 'calculate-shortest-path) calculate-shortest-path)
        ((eq? msg 'get-graph-hashmap) gs)
        ((eq? msg 'get-node-value) get-node-value)
        ((eq? msg 'get-node-id) get-node-id)
        ((eq? msg 'get-graph) railwaygraph)
        (else (error "Unknown message ---- GRAPH-CALCULATION"))))

    (generate-graph-from-railway)
    dispatch))


