#lang racket

;
; API (REST-Full)
; Copyright © 2017 Xander Van Raemdonck 2BA CW
;

(require "TCP-RESTAPI.rkt")

(get "/cars" (lambda (query) (cons 200 `#hasheq((result . ,(get-param-value query 'id))))))

(run)
