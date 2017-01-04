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
      (hash-for-each nodes-hashmap (lambda (nid value)
                                      (hash-set! gs nid i)
                                      (set! i (+ i 1))))
      i))

    (define (generate-graph-from-railway)
      (populate-graph-hashmap)
      (for-each (lambda (track) (add-track-to-graph (node-a track) (node-b track))) tracks-list)
      (hash-for-each db-hashmap (lambda (did db) (add-track-to-graph (node-a db) (node-b db))))
      (hash-for-each switches-hashmap (lambda (sid switch) (add-track-to-graph (node-a switch) (node-b switch)) (add-track-to-graph (node-a switch) (node-c switch)))))

    (define (get-node-id val)
      (let ([res #f])
        (hash-for-each gs (lambda (id value)
                            (when (= val value)
                              (set! res id))))
        res))

    (define (get-node-value id)
      (hash-ref gs id (lambda () #f)))

    (define (convert-mcons-to-list mconslist)
      (if (null? mconslist)
          '()
          (cons (mcar mconslist) (convert-mcons-to-list (mcdr mconslist)))))

    (define (calculate-shortest-path nA nB)
      (let* ([g railwaygraph]
             [path (bft:shortest-path g (get-node-value nA) (get-node-value nB))])
        (map
         (lambda (x)
           (get-node-id x))
         (reverse (convert-mcons-to-list path)))))

    (define (add-track-to-graph nA nB)
      (graph:add-edge! railwaygraph (get-node-value nA) (get-node-value nB)))

    
    (define (dispatch msg)
      (cond
        ((eq? msg 'calculate-shortest-path) calculate-shortest-path)
        (else (error "Unknown message ---- GRAPH-CALCULATION"))))

    (generate-graph-from-railway)
    dispatch))


