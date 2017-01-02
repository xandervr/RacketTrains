#lang racket

(require "control-systems/infrabel.rkt")
(require "control-systems/NMBS.rkt")


(define (RacketTrains)
  (let 
    ((infrabel   (make-infrabel))
    (NMBS   (make-NMBS)))


    (define (loop)
      ((infrabel 'update) NMBS)
      ;((NMBS 'update) infrabel)
      (sleep 0.1)
      (loop))

  (loop)))


(RacketTrains)