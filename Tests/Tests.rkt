#lang racket

;
; TESTS
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../railwaymodel/detection-track.rkt")
(require "../railwaymodel/track.rkt")
(require "../railwaymodel/switch.rkt")
(require "../railwaymodel/train.rkt")
(require "../railwaymodel/node.rkt")
(require "../Graphs/graph-calculation.rkt")
(require "../railwaymodel/rwm.rkt")
(require (prefix-in graph: "../Graphs/graph/unweighted/config.rkt"))

(define (make-test)
  (let ([tests 0]
        [succeeded 0]
        [failed 0]
        [rwm (load-rwm "railway-big.txt")])

  (define (test-ADT)
    (set! tests 0)
    (set! succeeded 0)
    (set! failed 0)
    (define A1 (make-node 'A1 10 10))
    (define A2 (make-node 'A2 20 10))
    (define A3 (make-node 'A3 30 10))
    (define A4 (make-node 'A4 30 20))
    (define A5 (make-node 'A5 40 20))
    (define S1 (make-node 'S1 30 15))

    (define detection-track (make-detection-track 'D1 'A1 'A2 14))
    (define track (make-track 'A2 'A3 10))
    (define switch (make-switch 'S1 'A3 'A4 'A5 12))

    (define train (make-train 'L1))

    (set! tests (+ tests 1))
    (if (eq? (detection-track 'get-type) 'detection-track) (set! succeeded (+ 1 succeeded)) (begin (printf "Detection-track type failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (eq? (track 'get-type) 'track) (set! succeeded (+ 1 succeeded)) (begin (printf "Track type failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (eq? (switch 'get-type) 'switch) (set! succeeded (+ 1 succeeded)) (begin (printf "Switch type failed!\n") (set! failed (+ 1 failed))))

    (set! tests (+ tests 1))
    (if (= (detection-track 'get-max-speed) 14) (set! succeeded (+ 1 succeeded)) (begin (printf "Detection-track max-speed failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (= (track 'get-max-speed) 10) (set! succeeded (+ 1 succeeded)) (begin (printf "Track max-speed failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (= (switch 'get-max-speed) 12) (set! succeeded (+ 1 succeeded)) (begin (printf "Switch max-speed failed!\n") (set! failed (+ 1 failed))))

    (set! tests (+ tests 1))
    ((detection-track 'occupy!) (train 'get-id))
    (if (detection-track 'occupied?) (set! succeeded (+ 1 succeeded)) (begin (printf "Detection-track occupy failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (detection-track 'free!)
    (if (not (detection-track 'occupied?)) (set! succeeded (+ 1 succeeded)) (begin (printf "Detection-track free failed!\n") (set! failed (+ 1 failed))))

    (set! tests (+ tests 1))
    ((track 'occupy!) (train 'get-id))
    (if (track 'occupied?) (set! succeeded (+ 1 succeeded)) (begin (printf "Track occupy failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (track 'free!)
    (if (not (track 'occupied?)) (set! succeeded (+ 1 succeeded)) (begin (printf "Track free failed!\n") (set! failed (+ 1 failed))))

    (set! tests (+ tests 1))
    ((switch 'occupy!) (train 'get-id))
    (if (switch 'occupied?) (set! succeeded (+ 1 succeeded)) (begin (printf "Switch occupy failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (switch 'free!)
    (if (not (switch 'occupied?)) (set! succeeded (+ 1 succeeded)) (begin (printf "Switch free failed!\n") (set! failed (+ 1 failed))))

    (set! tests (+ tests 1))
    (if (eq? (detection-track 'get-nodeA) 'A1) (set! succeeded (+ 1 succeeded)) (begin (printf "Detection-track node-a failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (eq? (track 'get-nodeB) 'A3) (set! succeeded (+ 1 succeeded)) (begin (printf "Track node-b failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (eq? (switch 'get-nodeC) 'A5) (set! succeeded (+ 1 succeeded)) (begin (printf "Switch node-c failed!\n") (set! failed (+ 1 failed))))

    (printf "ADT TESTS (~a): ~a succeeded, ~a failed!\n" tests succeeded failed))

  (define (test-Graphs)
    (set! tests 0)
    (set! succeeded 0)
    (set! failed 0)
    (define graph-calculation (make-graph-calculation))
    (define graph-hashmap (graph-calculation 'get-graph-hashmap)) 

    ; Test if all nodes exist in graph
    (define (node-exists? n)
      (let ([found #f])
      (hash-for-each graph-hashmap
                     (lambda (id value)
                      (when (eq? id n) (set! found #t))))
      found))

    (hash-for-each (rwm-ns rwm)
                   (lambda (id value)
                    (set! tests (+ tests 1))
                    (if (node-exists? id) (set! succeeded (+ 1 succeeded)) (begin (printf "~a was not found in graph!\n" id) (set! failed (+ 1 failed))))))

    ; Test if route is correct
    (set! tests (+ 1 tests))
    (define path ((graph-calculation 'calculate-shortest-path) 'D1 'D9))
    (define correct '(A1 A10 A9 A8 A7 A8 A9 A11 A12 A13))
    (if (eq? path correct) (set! succeeded (+ 1 succeeded)) (begin (printf "Path ~a was incorrect, should be ~a\n" path correct) (set! failed (+ 1 failed))))

    (printf "GRAPHS TESTS (~a): ~a succeeded, ~a failed!\n" tests succeeded failed))


  (define (test-dispatch msg)
    (cond
      ((eq? msg 'test-ADT) (test-ADT))
      ((eq? msg 'test-Graphs) (test-Graphs))))
  test-dispatch))

(define test (make-test))
(test 'test-ADT)
(test 'test-Graphs)
