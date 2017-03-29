#lang racket

;
; TCP-RESTAPI
; Copyright © 2017 Xander Van Raemdonck 2BA CW
;

(require net/url json)
(require "status-codes.rkt")

(provide get put post run
         get-param-value
         make-TCP-server)

(define (make-TCP-server [server-name "Racket REST API/1.0"] [mime-type "application/json"])

  (define (serve port)
  (define listener (tcp-listen port 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))

  (define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (with-handlers ([exn:fail? (lambda (exn) (display exn))])
    (handle in out))
  (close-input-port in)
  (close-output-port out))

(define (handle in out)
  (let* ([input (read-line in)]
        [req (regexp-match #rx"^([A-Z]+) (.+) HTTP/[0-9]+\\.[0-9]+" input)])
        (when req
          (regexp-match #rx"(\r\n|^)\r\n" in)

          (define method (string->symbol (list-ref req 1)))
          (define path (list-ref req 2))
          (let* ([output (compile-req path method)]
                     [status (car output)]
                     [json (cdr output)])
              (send-json-response out status json)))))

(define (compile-req str-path method)
  ; Parse the request as a URL:
  (define url (string->url str-path))
  ; Extract the path part:
  (define path (car (regexp-match #rx"[^?]*" str-path)));(map path/param-path (url-path url)))
  ; Find a handler based on the path's first element:
  (printf "~a\n" path)
  (define h #f)
  
  (case method
    ((GET) (set! h (hash-ref compile-get-table path #f)))
    ((PUT) (set! h (hash-ref compile-put-table path #f)))
    ((POST) (set! h (hash-ref compile-post-table path #f))))
  (if h
    (h (url-query url))
    (cons 404 #hasheq((message . "method not found!")))))
 
(define compile-get-table (make-hash))
(define compile-post-table (make-hash))
(define compile-put-table (make-hash))

(define (send-response-headers out status message)
  (fprintf out "HTTP/1.0 ~a ~a\r\n" status message)
  (fprintf out "Server: ~a\r\n" server-name)
  (fprintf out "Content-Type: ~a\r\n" mime-type)
  (fprintf out "\r\n"))

(define (send-json-response out status json)
  (send-response-headers out status (status->message status))
  (write-json json out))

(define (add-path! path method handler)
  (case method
    ((GET) (hash-set! compile-get-table path handler))
    ((PUT) (hash-set! compile-put-table path handler))
    ((POST) (hash-set! compile-post-table path handler))))


(define (TCP-Server-dispatch msg)
  (cond
    ((eq? msg 'run) serve)
    ((eq? msg 'add-path!) add-path!)))
TCP-Server-dispatch)

(define tcp-server (make-TCP-server))

(define (get-param-value query key)
  (cond 
    ((null? query) #f)
    ((eq? (caar query) key) (cdar query))
    (else (get-param-value (cdr query) key))))

(define (get path function)
  ((tcp-server 'add-path!) path 'GET function))

(define (put path function)
  ((tcp-server 'add-path!) path 'PUT function))

(define (post path function)
  ((tcp-server 'add-path!) path 'POST function))

(define (run [port 8080])
  ((tcp-server 'run) port))

