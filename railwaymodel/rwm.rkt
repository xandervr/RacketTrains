#lang racket


;;; RAILWAY MODEL ;;;
;; ADT for a railway model, which can be read from
;; a text file.

; (require ...)

(require "cart.rkt")
(require "detection-block.rkt")
(require "node.rkt")
(require "switch.rkt")
(require "track.rkt")
(require "train.rkt")
(require "../Abstractions/Abstractions.rkt")

(provide (struct-out rwm)
         load-rwm
         fetch-track
         find-track
         find-db
         find-nodes-middle
         track-eqv?)


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
     (λ (l)
       (case (string->symbol (car l))
         [(L) (let* ([lid (string->symbol (list-ref l 1))]
                     [res (make-train lid)])
                (hash-set! ls lid res))]
         [(N) (let* ([id (string->symbol (list-ref l 1))]
                     [x (string->number (list-ref l 2))]
                     [y (string->number (list-ref l 3))]
                     [res (make-node id x y)])
                (hash-set! ns id res))]
         [(S) (let* ([id (string->symbol (list-ref l 1))]
                     [nA (string->symbol (list-ref l 2))]
                     [nB (string->symbol (list-ref l 3))]
                     [nC (string->symbol (list-ref l 4))]
                     [ms (string->number (list-ref l 5))]
                     [res (make-switch id nA nB nC ms)])
                (hash-set! ss id res))]
         [(T) (let* ([nA (string->symbol (list-ref l 1))]
                     [nB (string->symbol (list-ref l 2))]
                     [ms (string->number (list-ref l 3))]
                     [res (make-track nA nB ms)])
                (set! ts (cons res ts)))]
         [(D) (let* ([id (string->symbol (list-ref l 1))]
                     [nA (string->symbol (list-ref l 2))]
                     [nB (string->symbol (list-ref l 3))]
                     [ms (string->number (list-ref l 4))]
                     [res (make-detection-block id nA nB ms)])
                (hash-set! ds id res))]))
     lines)
    (rwm ls ns ss ts ds)))

; (define rwm-be (load-rwm "be_simple.txt"))



(define (fetch-track rwm nA nB)
  (let ([db (hash-ref (rwm-ds rwm) (find-db rwm nA nB) (λ () #f))]
        [t (find-track rwm nA nB)]
        [s (hash-ref (rwm-ss rwm) (find-s rwm nA nB) (λ () #f))])
    (or db t s)))

(define (track-eqv? t1 t2)
  (or (and (eqv? (node-a t1) (node-a t2))
           (eqv? (node-b t1) (node-b t2)))
      (and (eqv? (node-a t1) (node-b t2))
           (eqv? (node-b t1) (node-a t2)))))

(define (find-track rwm n1 n2)
  (let ([track (findf (λ (t2)
                        (let ([t1 (make-track n1 n2)])
                          (track-eqv? t1 t2)))
                      (rwm-ts rwm))])
    track))

(define (find-db rwm n1 n2)
  (let ([d #f])
    (hash-for-each (rwm-ds rwm) 
                   (λ (did detection-block) 
                     (let   ([t1 (make-track n1 n2)]
                             [t2 (detection-block 'get-track)])
                       (when (track-eqv? t1 t2)
                         (set! d (id detection-block))))))
    d))

(define (find-s rwm n1 n2)
  (let ([s #f])
    (hash-for-each (rwm-ss rwm) 
                   (λ (sid switch) 
                     (let   ([t1 (make-track n1 n2)]
                             [t2 (make-track (node-a switch) (node-b switch))]
                             [t3 (make-track (node-a switch) (node-c switch))])
                       (when (or (track-eqv? t1 t2) (track-eqv? t1 t3))
                         (set! s (id switch))))))
    s))

(define (find-nodes-middle nA nB)
  (let* ((x1  (get-x nA))
         (y1  (get-y nA))
         (x2  (get-x nB))
         (y2  (get-y nB)))
    (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

