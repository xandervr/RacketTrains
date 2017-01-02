#lang racket


;;; RAILWAY MODEL ;;;
;; ADT for a railway model, which can be read from
;; a text file.

; (require ...)

(require "cart.rkt")
(require "detection-block.rkt")
(require "node.rkt")
;(require "rwm.rkt")
(require "switch.rkt")
(require "track.rkt")
(require "train.rkt")

(provide (struct-out rwm)
         load-rwm)


(struct rwm (ls ns ss ts ds))


; Reads a railway model from a text file.
(define (load-rwm filename)
  (let ([lines (map string-split (file->lines filename))]
        [ls (make-hash)]
        [ns (make-hash)]
        [ss (make-hash)]
        [ts '()]
        [ds (make-hash)])
    (for-each
     (lambda (l)
       (case (string->symbol (car l))
         [(L) (let* ([lid (list-ref l 1)]
                     [res (make-train lid)])
                (hash-set! ls lid res))]
         [(N) (let* ([id (list-ref l 1)]
                     [x (string->number (list-ref l 2))]
                     [y (string->number (list-ref l 3))]
                     [res (make-node id x y)])
                (hash-set! ns id res))]
         [(S) (let* ([id (list-ref l 1)]
                     [nA (string->symbol (list-ref l 2))]
                     [nB (string->symbol (list-ref l 3))]
                     [nC (string->symbol (list-ref l 4))]
                     [res (make-switch id nA nB nC)])
                (hash-set! ss id res))]
         [(T) (let* ([nA (string->symbol (list-ref l 1))]
                     [nB (string->symbol (list-ref l 2))]
                     [res (make-track nA nB)])
                (set! ts (cons res ts)))]
         [(D) (let* ([id (list-ref l 1)]
                     [nA (string->symbol (list-ref l 2))]
                     [nB (string->symbol (list-ref l 3))]
                     [res (make-detection-block id (make-track nA nB))])
                (hash-set! ds id res))]))
     lines)
    (rwm ls ns ss ts ds)))

; (define rwm-be (load-rwm "be_simple.txt"))



