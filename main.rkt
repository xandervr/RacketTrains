#lang racket

(require "control-systems/infrabel.rkt")
(require "control-systems/NMBS.rkt")
(require "GUI/GUI.rkt")


(define (RacketTrains)
  (let* 
    ([infrabel  (make-infrabel)]
     [NMBS  (make-NMBS)]
     [GUI (make-GUI "RacketTrains" infrabel NMBS)])

    
    (define (loop)
      ((NMBS 'update) infrabel)
      ((infrabel 'update) NMBS)
      (GUI 'redraw!)
      (sleep 0.1)
      (loop))

  (thread loop)))


(RacketTrains)