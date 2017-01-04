#lang racket

;
; Graph calulation ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require (prefix-in graph: "graph/unweighted/config.rkt"))
(require (prefix-in bft: "graph-algorithms/undirected/bft-applications.rkt"))

(provide make-graph-calculation)

(define (make-graph-calculation)
  (define railway (let ([rail-graph (graph:new #f 14)])
                    (graph:add-edge! rail-graph 0 1)
                    (graph:add-edge! rail-graph 1 2)
                    (graph:add-edge! rail-graph 2 3)
                    (graph:add-edge! rail-graph 3 4)
                    (graph:add-edge! rail-graph 4 5)
                    (graph:add-edge! rail-graph 5 6)
                    (graph:add-edge! rail-graph 6 7)
                    (graph:add-edge! rail-graph 7 8)
                    (graph:add-edge! rail-graph 8 13)
                    (graph:add-edge! rail-graph 13 9)
                    (graph:add-edge! rail-graph 9 0)
                    (graph:add-edge! rail-graph 13 10)
                    (graph:add-edge! rail-graph 10 11)
                    (graph:add-edge! rail-graph 11 12)
                    rail-graph))

  (define gs (make-hash))
  (define nodes '(A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 S1))
  (define i 0)
  (for-each
   (lambda (x)
     (hash-set! gs x i)
     (set! i (+ i 1)))
   nodes)

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
    (let* ([g railway]
           [path (bft:shortest-path g (get-node-value nA) (get-node-value nB))])
      (for-each
       (lambda (x)
         (printf "~a " (get-node-id x)))
       (reverse (convert-mcons-to-list path)))))

  (define (dispatch msg)
    (cond
      ((eq? msg 'calculate-shortest-path) calculate-shortest-path)
      (else (error "Unknown message ---- GRAPH-CALCULATION"))))
  dispatch)


