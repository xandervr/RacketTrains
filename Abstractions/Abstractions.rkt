#lang racket

(provide current-node
         next-node
         second-node
         schedule-rest)

(define current-node car)
(define next-node cadr)
(define second-node caddr)
(define schedule-rest cdr)
