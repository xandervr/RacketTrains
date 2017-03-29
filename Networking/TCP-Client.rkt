#lang racket

;
; TCP-Client
; Copyright © 2017 Xander Van Raemdonck 2BA CW
;

(require net/http-client json)
(require "status-codes.rkt")

(provide connect get post put
         make-TCP-client)

(define (make-TCP-client host port)

  (define (do-request url method)
    (define-values (s h r) (http-sendrecv host url #:port port #:method method))
    (read-json r))

  (define (dispatch-TCP-client msg)
    (cond
      ((eq? msg 'send) do-request)))
  dispatch-TCP-client)

(define tcp-client #f)

(define (connect host [port 8080])
  (set! tcp-client (make-TCP-client host port)))

(define (get url [parse-key #f])
  (when tcp-client
    (let ([res ((tcp-client 'send) url 'GET)])
      (if parse-key
        (parse-json-value res parse-key)
        res))))

(define (post url)
  (when tcp-client
    ((tcp-client 'send) url 'POST)))

(define (put url)
  (when tcp-client
    ((tcp-client 'send) url 'PUT)))

(define (parse-json-value json key)
  (hash-ref json key #f))

(connect "localhost")
(display (get "/cars?id=204" 'result))
