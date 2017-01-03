#lang racket

(require "control-systems/infrabel.rkt")
(require "control-systems/NMBS.rkt")


(define (RacketTrains)
  (let 
    ((infrabel   (make-infrabel))
    (NMBS   (make-NMBS)))


    (define (loop)
      ((NMBS 'update) infrabel)
      ((infrabel 'update) NMBS)
      (sleep 0.1)
      (loop))

  (loop)))


(RacketTrains)