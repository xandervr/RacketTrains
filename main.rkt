#lang racket

;
; NMBS ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "control-systems/infrabel.rkt")
(require "control-systems/NMBS.rkt")
(require "GUI/GUI.rkt")
(require "GUI/GUI-Advanced.rkt")


(define (RacketTrains)
  (let* 
    ([infrabel  (make-infrabel)]
     [NMBS  (make-NMBS infrabel)]
     [GUI-log (make-GUI-log "RacketTrains Log" infrabel NMBS)]
     [GUI-adv (make-GUI-adv "RacketTrains" infrabel NMBS)])
    
    (define (loop)
      (NMBS 'update)
      ((infrabel 'update) NMBS)
      (GUI-log 'redraw!)
      (GUI-adv 'redraw!)
      (sleep 0.1)
      (loop))

  (thread loop)))


(RacketTrains)