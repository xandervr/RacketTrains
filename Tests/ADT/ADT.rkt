#lang racket

;
; ADT TEST
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "../../railwaymodel/detection-track.rkt")
(require "../../railwaymodel/track.rkt")
(require "../../railwaymodel/switch.rkt")
(require "../../railwaymodel/train.rkt")
(require "../../railwaymodel/node.rkt")

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


; Print types
(printf "Track type: ~a\n" (detection-track 'get-type))
(printf "Track type: ~a\n" (track 'get-type))
(printf "Track type: ~a\n" (switch 'get-type))

; Print max-speed
(printf "Max speed: ~a\n" (detection-track 'get-max-speed))
(printf "Max speed: ~a\n" (track 'get-max-speed))
(printf "Max speed: ~a\n" (switch 'get-max-speed))

; Occupy, print occupation, free, print occupation
(printf "Detection Track\n")
((detection-track 'occupy!) (train 'get-id))
(printf "Occupation: ~a\n" (detection-track 'occupied?))
(detection-track 'free!)
(printf "Occupation: ~a\n" (detection-track 'occupied?))

(printf "Track\n")
((track 'occupy!) (train 'get-id))
(printf "Occupation: ~a\n" (track 'occupied?))
(track 'free!)
(printf "Occupation: ~a\n" (track 'occupied?))

(printf "Switch\n")
((switch 'occupy!) (train 'get-id))
(printf "Occupation: ~a\n" (switch 'occupied?))
(switch 'free!)
(printf "Occupation: ~a\n" (switch 'occupied?))

; Display nodes
(printf "Detection-track (~a): A: ~a B: ~a\n" (detection-track 'get-id) (detection-track 'get-nodeA) (detection-track 'get-nodeB))
(printf "Track: A: ~a B: ~a\n" (track 'get-nodeA) (track 'get-nodeB))
(printf "Switch (~a): A: ~a B: ~a C: ~a\n" (switch 'get-id) (switch 'get-nodeA) (switch 'get-nodeB) (switch 'get-nodeC))

; Node x and y
(printf "Node (~a): (~a, ~a)\n" (A1 'get-id) (A1 'get-x) (A1 'get-y))
(printf "Node (~a): (~a, ~a)\n" (S1 'get-id) (S1 'get-x) (S1 'get-y))