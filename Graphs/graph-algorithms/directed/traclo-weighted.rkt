#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*        Transitive Closure Algorithms for Weighted Graphs        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (basic)
 (export floyd-warshall)
 (import (rnrs base)
         (rnrs control)
         (a-d scheme-tools)
         (a-d graph weighted config)
         (a-d graph-algorithms directed basic)
         (a-d graph-algorithms directed connectivity)
         (a-d graph-traversing dft-unweighted))
 
 (define (ij? v i j)
   (vector-ref (vector-ref v i) j))
 (define (ij! v i j a)
   (vector-set! (vector-ref v i) j a))
 
 (define (floyd-warshall g)
   (define traclo-distances (make-vector (order g) '()))
   (vector-map! 
    traclo-distances 
    (lambda (i w) 
      (define row (make-vector (order g)))
      (vector-map! row (lambda (j w)
                         (weight g i j)))
      row))
   (for-each-node
    g
    (lambda (via)
      (for-each-node
       g
       (lambda (from)
         (for-each-node
          g
          (lambda (to)
            (ij! 
             traclo-distances 
             from to (min (ij? traclo-distances from to)
                          (+ (ij? traclo-distances from via)
                             (ij? traclo-distances via to))))))))))
   traclo-distances))