;#lang racket

(require "control-systems/infrabel.rkt")
(require "control-systems/NMBS.rkt")


(define (RacketTrains)
  (let 
    (;(Z21  (make-Z21))
    (infrabel   (make-infrabel))
    (NMBS   (make-NMBS)))

    (define (loop)
      ((infrabel 'get-location) '|1|)
      (newline)
      (sleep 1)
      (loop))
    
  ; (define (loop)
  ;  ((NMBS 'update) Z21 infrabel)
  ;  ((infrabel 'update) Z21 NMBS)
  ;  ((gui 'update) Z21 infrabel NMBS)
  ;  (loop))
  ; (loop)
  (loop)
  ))


(RacketTrains)