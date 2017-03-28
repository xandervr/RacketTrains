#lang racket

(require "TCP-RESTAPI.rkt")

(get "/cars" (lambda (query) (cons 200 #hasheq((cars . "test")))))

(run)
