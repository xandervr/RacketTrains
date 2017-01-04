#lang racket

(provide current-node
         next-node
         second-node
         schedule-rest
         type
         max-speed
         detection-block?
         track?
         switch?
         id
         track
         get-train-schedule
         set-train-schedule!
         insert-schedule!
         schedule
         node-a
         node-b
         node-c
         free?
         free!
         occupy!
         get-train-schedule
         get-train-location
         get-train-speed
         set-switch-state!
         get-switch-state)

(define current-node car)
(define next-node cadr)
(define second-node caddr)
(define schedule-rest cdr)
(define (type obj)
  (obj 'get-type))
(define (max-speed obj)
  (obj 'get-max-speed))
(define (detection-block? obj)
  (eq? (type obj) 'detection-block))
(define (track? obj)
  (eq? (type obj) 'track))
(define (switch? obj)
  (eq? (type obj) 'switch))
(define (id obj)
  (obj 'get-id))
(define (track db)
  (db 'get-track))
(define (get-train-schedule NMBS id)
  ((NMBS 'get-schedule) id))
(define (set-train-schedule! train schedule)
  ((train 'set-schedule!) schedule))
(define (schedule train)
  (train 'get-schedule))
(define (insert-schedule! NMBS id schedule)
  ((NMBS 'add-schedule!) id schedule))
(define (node-a track)
  (track 'get-nodeA))
(define (node-b track)
  (track 'get-nodeB))
(define (node-c switch)
  (switch 'get-nodeC))
(define (free? obj [id #f])
  ((obj 'free?) id))
(define (free! obj)
  (obj 'free!))
(define (occupy! obj id)
  ((obj 'occupy!) id))
(define (get-train-location infrabel id)
  ((infrabel 'get-train-location) id))
(define (get-train-speed infrabel id)
  ((infrabel 'get-train-speed) id))
(define (set-switch-state! infrabel id pos)
  ((infrabel 'set-switch-state!) id pos))
(define (get-switch-state infrabel id)
  ((infrabel 'get-switch-state) id))



