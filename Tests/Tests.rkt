#lang racket

;
; TESTS
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../ADT/detection-track.rkt")
(require "../ADT/track.rkt")
(require "../ADT/switch.rkt")
(require "../ADT/train.rkt")
(require "../ADT/node.rkt")
(require "../Graphs/graph-calculation.rkt")
(require "../ADT/rwm.rkt")
(require (prefix-in graph: "../Graphs/graph/unweighted/config.rkt"))
(require "../Control-Systems/NMBS.rkt")
(require "../Control-Systems/infrabel.rkt")
(require "../GUI/GUI-Advanced.rkt")
(require rackunit
         rackunit/text-ui)

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

    ; Test types

    (set! tests (+ tests 1))
    (if (eq? (detection-track 'get-type) 'detection-track) (set! succeeded (+ 1 succeeded)) (begin (printf "Detection-track type failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (eq? (track 'get-type) 'track) (set! succeeded (+ 1 succeeded)) (begin (printf "Track type failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (eq? (switch 'get-type) 'switch) (set! succeeded (+ 1 succeeded)) (begin (printf "Switch type failed!\n") (set! failed (+ 1 failed))))

    ; Test max speed
    (set! tests (+ tests 1))
    (if (= (detection-track 'get-max-speed) 14) (set! succeeded (+ 1 succeeded)) (begin (printf "Detection-track max-speed failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (= (track 'get-max-speed) 10) (set! succeeded (+ 1 succeeded)) (begin (printf "Track max-speed failed!\n") (set! failed (+ 1 failed))))
    (set! tests (+ tests 1))
    (if (= (switch 'get-max-speed) 12) (set! succeeded (+ 1 succeeded)) (begin (printf "Switch max-speed failed!\n") (set! failed (+ 1 failed))))

    ; Test occupy and free
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

    ; Test get-node
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
                     (λ (id value)
                      (when (eq? id n) (set! found #t))))
      found))

    (hash-for-each (rwm-ns rwm)
                   (λ (id value)
                    (set! tests (+ tests 1))
                    (if (node-exists? id) (set! succeeded (+ 1 succeeded)) (begin (printf "~a was not found in graph!\n" id) (set! failed (+ 1 failed))))))

    ; Test if route is correct
    (set! tests (+ 1 tests))
    (define path ((graph-calculation 'calculate-shortest-path) 'D1 'D9))
    (define correct '(A1 A10 A9 A8 A7 A8 A9 A11 A12 A13))
    (if (eq? path correct) (set! succeeded (+ 1 succeeded)) (begin (printf "Path ~a was incorrect, should be ~a\n" path correct) (set! failed (+ 1 failed))))

    (printf "GRAPHS TESTS (~a): ~a succeeded, ~a failed!\n" tests succeeded failed))


  (define (test-Mocking)
    (printf "Mocking TESTS\n")
    (let* ([infrabel (make-infrabel)]
           [NMBS (make-NMBS infrabel)]
           [GUI-adv (make-GUI-adv "Trains" infrabel NMBS)])
    
    (define unit-tests
      (test-suite "Trains Mocking"

        ; Test train location
        (test-case
          "Testing train location"
          (let () 
            (simulate)
            (define train-position ((infrabel 'get-train-location) 'L1))
            (check-equal? train-position 'D9)))

        ; Test destination succeeded
        (test-case
          "Test if train gets to destination"
          (let () 
            ((NMBS 'drive-to-destination!) 'L1 'D1)
            (let test-destination ()
                      (sleep 0.1)
                      (simulate)
                      (define train-position ((infrabel 'get-train-location) 'L1))
                      (if (= ((infrabel 'get-train-speed) 'L1) 0) (check-equal? train-position 'D1) (test-destination)))))

        ; Train is on D1
        (test-case
          "Test switch positioning in D1"
          (let () 
            (let test-switch ()
                      (sleep 0.1)
                      (simulate)
                      (define train-position ((infrabel 'get-train-location) 'L1))
                      (define switch-state ((infrabel 'get-switch-state) 'S1))
                      (if (eq? train-position 'D1) 
                        (check-equal? switch-state 1) 
                        (test-switch)))))

        (test-case
          "Test switch positioning in D6 when driving to D9 from D1"
          (let () 
            ((NMBS 'drive-to-destination!) 'L1 'D9)
            (let test-switch ()
                      (sleep 0.1)
                      (simulate)
                      (define train-position ((infrabel 'get-train-location) 'L1))
                      (define switch-state ((infrabel 'get-switch-state) 'S1))
                      (if (eq? train-position 'D6) 
                        (check-equal? switch-state 2)
                        (test-switch)))))

        (test-case
          "Test if trains stops for the other train"
          (let () 
            (define (let-drive-to-D9)
              (sleep 0.1)
              (simulate)
              (define train-position ((infrabel 'get-train-location) 'L1))
              (when (not (eq? train-position 'D9)) (let-drive-to-D9)))
            (let-drive-to-D9)
            ((NMBS 'drive-to-destination!) 'L1 'D4)
            (let test-stop ()
                      (sleep 0.1)
                      (simulate)
                      (define train-position ((infrabel 'get-train-location) 'L1))
                      (if (= ((infrabel 'get-train-speed) 'L1) 0) (check-equal? train-position 'D5) (test-stop)))))

        (test-case
          "Test if train continues when other train moves"
          (let () 
            ((NMBS 'drive-to-destination!) 'L2 'D1)
            (let test-continue ()
                      (sleep 0.1)
                      (simulate)
                      (define train-1-position ((infrabel 'get-train-location) 'L1))
                      (define train-2-position ((infrabel 'get-train-location) 'L2))
                      (if (and (= ((infrabel 'get-train-speed) 'L1) 0) (not (= ((infrabel 'get-train-speed) 'L2) 0)) (eq? train-2-position 'D2)) (check-equal? train-1-position 'D4) (test-continue)))))
        ))

    (define (simulate)
      (NMBS 'update)
      ((infrabel 'update) NMBS)
      (GUI-adv 'redraw!))
    (thread (lambda () (run-tests unit-tests) (NMBS 'exit!) (infrabel 'exit!) (GUI-adv 'exit!)))))


  (define (test-dispatch msg)
    (cond
      ((eq? msg 'test-ADT) (test-ADT))
      ((eq? msg 'test-Graphs) (test-Graphs))
      ((eq? msg 'test-Mocking) (test-Mocking))))
  test-dispatch))

(define test (make-test))
(test 'test-ADT)
(test 'test-Graphs)
(test 'test-Mocking)
