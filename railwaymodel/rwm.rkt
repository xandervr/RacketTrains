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

(provide (struct-out rwm)
         load-rwm
         fetch-track
         find-track
         find-db)


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
  (define db (hash-ref (rwm-ds rwm) (find-db rwm nA nB) (lambda () #f)))
  (define t (find-track rwm nA nB))
  (define s (hash-ref (rwm-ss rwm) (find-s rwm nA nB) (lambda () #f)))
  (or db t s))

(define (track-eqv? t1 t2)
  (or (and (eqv? (t1 'get-nodeA) (t2 'get-nodeA))
           (eqv? (t1 'get-nodeB) (t2 'get-nodeB)))
      (and (eqv? (t1 'get-nodeA) (t2 'get-nodeB))
           (eqv? (t1 'get-nodeB) (t2 'get-nodeA)))))

(define (find-track rwm n1 n2)
  (define track (findf (lambda (t2)
   (let ([t1 (make-track n1 n2)])
     (track-eqv? t1 t2)))
  (rwm-ts rwm)))
  track)

(define (find-db rwm n1 n2)
  (define d #f)
  (hash-for-each (rwm-ds rwm) 
    (lambda (id detection-block) 
      (let   ([t1 (make-track n1 n2)]
        [t2 (detection-block 'get-track)])
      (when (track-eqv? t1 t2)
        (set! d (detection-block 'get-id))))))
  d)

(define (find-s rwm n1 n2)
  (define s #f)
  (hash-for-each (rwm-ss rwm) 
    (lambda (id switch) 
      (let   ([t1 (make-track n1 n2)]
              [t2 (make-track (switch 'get-nodeA) (switch 'get-nodeB))]
              [t3 (make-track (switch 'get-nodeA) (switch 'get-nodeC))])
      (when (or (track-eqv? t1 t2) (track-eqv? t1 t3))
        (set! s (switch 'get-id))))))
  s)

