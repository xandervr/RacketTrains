#lang racket

;
; Main ADT
; Copyright © 2016 Xander Van Raemdonck 2BA CW
;

(require "control-systems/infrabel.rkt")
(require "control-systems/NMBS.rkt")
(require "GUI/GUI-log.rkt")
(require "GUI/GUI-Advanced.rkt")

(define (make-main)
  (let* ([infrabel (make-infrabel)]
         [NMBS (make-NMBS infrabel)]
         [GUI-log #f]
         [GUI-adv (make-GUI-adv "Trains" infrabel NMBS)]
         [logging #f]
         [running #t])

  (when logging
    (set! GUI-log (make-GUI-log "Trains Log" infrabel NMBS)))

  (define (enable-logging)
    (set! logging #t)
    (set! GUI-log (make-GUI-log "Trains Log" infrabel NMBS)))

  (define (disable-logging)
    (set! logging #f)
    (set! GUI-log #f))

  (define (main-loop)
    (NMBS 'update)
    ((infrabel 'update) NMBS)
    (when logging 
      (GUI-log 'redraw!))
    (GUI-adv 'redraw!)
    (sleep 0.1)
    (when running
      (main-loop)))

  (define (dispatch msg)
    (cond
      ((eq? msg 'start) (thread main-loop))
      ((eq? msg 'stop) (set! running #f))
      ((eq? msg 'enable-logging) (enable-logging))
      ((eq? msg 'disable-logging) (disable-logging))))
  dispatch))

(define Trains-project (make-main))
(Trains-project 'disable-logging)
(Trains-project 'start)