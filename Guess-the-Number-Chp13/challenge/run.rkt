#lang racket

;O launch-guess-client "Adam" é o player que escolhe o número
;O que tenta advinhar está no arquivo "run2.rkt"

(require 2htdp/universe "server.rkt" "client.rkt")

(define (run)
  (launch-many-worlds (launch-guess-client "Adam")
                      (launch-guess-server)))
