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

    (define (populate-graph-hashmap)
      (let ([i 0])
      (hash-for-each nodes-hashmap (λ (nid value)
                                      (hash-set! gs nid i)
                                      (set! i (+ i 1))))
      i))

    (define (generate-graph-from-railway)
      (populate-graph-hashmap)
      (for-each (λ (track) (add-track-to-graph (node-a track) (node-b track))) tracks-list)
      (hash-for-each db-hashmap (λ (did db) (add-track-to-graph (node-a db) (node-b db))))
      (hash-for-each switches-hashmap (λ (sid switch) (add-track-to-graph (node-a switch) (node-b switch)) (add-track-to-graph (node-a switch) (node-c switch)))))

    (define (get-node-id val)
      (let ([res #f])
        (hash-for-each gs (λ (id value)
                            (when (= val value)
                              (set! res id))))
        res))

    (define (get-node-value id)
      (hash-ref gs id (λ () #f)))

    (define (convert-mcons-to-list mconslist)
      (if (null? mconslist)
          '()
          (cons (mcar mconslist) (convert-mcons-to-list (mcdr mconslist)))))

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
        path))

    (define (add-track-to-graph nA nB)
      (graph:add-edge! railwaygraph (get-node-value nA) (get-node-value nB)))

    
    (define (dispatch msg)
      (cond
        ((eq? msg 'calculate-shortest-path) calculate-shortest-path)
        (else (error "Unknown message ---- GRAPH-CALCULATION"))))

    (generate-graph-from-railway)
    dispatch))


